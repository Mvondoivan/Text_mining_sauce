scrap_all_emoji=function(){
                  str_url=c('https://emojipedia.org/apple/', 'https://emojipedia.org/emojidex/', 'https://emojipedia.org/emojione/', 
                            'https://emojipedia.org/emojipedia/11.1/', 'https://emojipedia.org/facebook/', 'https://emojipedia.org/google/',
                            'https://emojipedia.org/htc/', 'https://emojipedia.org/lg/', 'https://emojipedia.org/messenger/', 
                            'https://emojipedia.org/microsoft/', 'https://emojipedia.org/mozilla/', 'https://emojipedia.org/samsung/',
                            'https://emojipedia.org/twitter/', 'https://emojipedia.org/whatsapp/')
                  
                  scrap_emo=function(y){
                      etsi=function(x, ncol){
                        text_imo=x%>%str_split(., '"')%>%unlist(.)%>%.[grep(pattern = "https://" , .)]%>%.[ncol]%>%
                          str_split(., '/')%>%unlist(.)%>%.[length(.)]%>%str_split(., '[.]')%>%unlist(.)%>%.[1]%>%
                          str_split(., '_')%>%unlist(.)
                          #if (length(text_imo)==4) text_imo=c(paste(text_imo[1],text_imo[2],sep='_'), text_imo[3])
                        if (length(text_imo)==4) text_imo=c(text_imo[1], text_imo[3])
                        return(text_imo)
                      }
                      
                      download.file(y, destfile = "scrapedpage.html", quiet=T)
                      
                      text_emoji = read_html("scrapedpage.html")%>%html_node(.,".emoji-grid")%>%xml_children(.)%>%
                        xml_children(.)%>%xml_children(.)%>%as.character()%>%lapply(.,etsi, 1)%>%
                        data_frame(name.imoji=unlist(.)[c(T,F)],unicode.imoji=unlist(.)[c(F,T)])%>%
                        select(one_of(c("name.imoji", "unicode.imoji")))
                        text_emoji$unicode.imoji=str_split(text_emoji$unicode.imoji, '-')%>%
                                                 lapply(., FUN=function(x) paste0('0x', unlist(x))%>%lapply(., intToUtf8)%>%unlist()%>%paste0(., collapse = ''))%>%
                                                 unlist()%>%as.character()%>%iconv(., "latin1", "ASCII", 'byte')
                      return(text_emoji)}
                  text_emoji=lapply(str_url, scrap_emo)%>%do.call(bind_rows, args=.)%>%.[duplicated(.)==F,]
                  return(text_emoji)}


