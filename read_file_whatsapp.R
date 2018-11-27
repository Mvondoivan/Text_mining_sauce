read.whatsapp.file=function(path, encoding){

text.process=function(txt, date.format, length.check.txt){
  i=1
  j=0
  repeat{
    if (!is.na(as.Date(str_sub(txt[[1]][i], 1, length.check.txt), format = date.format))){
      j=i+1
      while (is.na(as.Date(str_sub(txt[[1]][j], 1, length.check.txt), format = date.format))){
        txt[[1]][i]=paste(txt[[1]][i], txt[[1]][j])
        txt[[1]][j]=''
        j=j+1
        if (j>length(txt[[1]])){break}
      }
      i=j
      if (i>=length(txt[[1]])){break}
    }else{
      i=i+1
      if (i>=length(txt[[1]])){break}}
  }
  txt=txt[[1]][lapply(txt[[1]], nchar)>0]
  return(txt)
}

correct.split=function(txt, patern, length_split, correct){
  
  traite.split=function(x,step){
    if (length(x)>step){
      x[step]=str_c(x[step:length(x)], collapse =patern)
      x=x[1:step]
    }else if(length(x)!=step){x=''}
    return(x)
  }
  if (correct=="True"){
    txt=lapply(txt, str_split, pattern=patern)%>%lapply(., unlist)%>%lapply(., traite.split, length_split)%>%.[lapply(., length)==length_split]}
  else if (correct=="False"){
    txt=lapply(txt, str_split, pattern=patern)%>%lapply(., unlist)%>%lapply(., traite.split, length_split) 
  }
  return(txt)
}

TXT=readtext(file=path, encoding = encoding)%>%corpus()%>%str_split(pattern="\n")%>%
    text.process(date.format="%d/%m/%Y", length.check.txt=10)%>%correct.split(patern=" - ", length_split=2, correct="True")

text.var=correct.split(unlist(TXT)[c(F,T)], patern=": ", length_split=2, correct="False")
time.var=unlist(TXT)[c(T,F)]%>%str_split(.,pattern=" à ")
Day=unlist(time.var)[c(TRUE,FALSE)][lapply(text.var, length)==2]
Hour=paste(unlist(time.var)[c(FALSE,TRUE)][lapply(text.var, length)==2], rep("00",length(Day)), sep=":")
text.var=text.var[lapply(text.var, length)==2]
Name=unlist(text.var[lapply(text.var, length)==2])[c(T,F)]
Comments=unlist(text.var[lapply(text.var, length)==2])[c(F,T)]%>%iconv(., 'UTF-8', 'latin1', 'byte')

Len_comments=as.numeric(as.character(nchar(Comments)))
Weekday=weekdays(as.Date(Day, format="%d/%m/%Y"))
temp=unlist(str_split(Day, pattern="/"))
Year=temp[c(F,F,T)]
Month=temp[c(F,T,F)]
Days=temp[c(T,F,F)]
temp=unlist(str_split(Hour, pattern=":"))
Hours=temp[c(T,F,F)]
Minutes=temp[c(F,T,F)]
TXT=as_tibble(as.data.frame(cbind(Day=Day, Year=Year, Month=Month, Days=Days, 
                                  Hour=Hour, Hours=Hours, Minutes=Minutes,
                                  Weekday=Weekday,
                                  Name=Name, 
                                  Len_comments=Len_comments,
                                  Comments=Comments)))
TXT=TXT%>%mutate(Day=as.Date(Day, format="%d/%m/%Y"), Hour=times(Hour), 
                 Len_comments=as.numeric(as.character(Len_comments)))
TXT=TXT%>%filter(Comments!="<Médias omis>")
return(TXT)}