## Minute extractor
## Hospital by hospital approach 
# for those Minutes that are
# buried in a combined package
rm(list=ls())
RowData<-DBFileList[i,]
FindMinutes<-function(Begins,Ends,RowData){ # rowdata =DBFilelist[i,]
  if(nrow(Begins)==1 & nrow(Ends)==1){
    
    Minutes<-CollectData(RowData,Begins,Ends)
    return(Minutes)}
  
  if(nrow(Begins)==0 & nrow(Ends)==1){
    length(Begins)
    Minutes<-DBMinutes[0,] %>%
      mutate(text=length(Ends)) %>%
      mutate(MinuteText=length(Begins))
    return(Minutes)
    Starts<-str_locate_all(RowData[1,5],c(regex("A special meeting of the Board of Directors")))
    Starts<-Convert2dFrame(Starts)
    #ispresent<-str_locate(RowData[1,5],"Present:")
    #ispresent<-ispresent[1,1]
    #Starts$ispresent<-(ispresent[1,1])
    #Starts<-Starts %>%
     # filter(start<=ispresent) %>%
    #  filter(start==max(start))
    Minutes<-CollectData(RowData,Starts,Ends)
    return(Minutes)}
}

CollectData<-function(DF,begin,end){ 
  FM<-DF%>%
    select(FileName,Organization,text) %>%
    mutate(MinuteText=str_sub(text,begin[1,"start"],end[1,"end"]))
  return(FM)
}
DBMinutes<-data.frame(FileName=character(0),Minutes=character(0),Organization=character(0),
                      Sequence=numeric(0),stringsAsFactors = FALSE)

Convert2dFrame<-function(d2list){
  Bl<-data.frame(start=integer(),end=integer())
  df<-Bl
  for(I in length(d2list))
  {df<-as.data.frame(unlist(d2list[[I]]))
  Bl<-bind_rows(df,Bl)
   }
    return(Bl)
}


SourceFile<-"~/BoardAnalytics/TheGreatOverhaul/SourceData/CombinedMinutes/KingstonHSCText"
# KHSC
DBFileList<-as.data.frame(list.files(SourceFile,full.names = TRUE,all.files=FALSE,recursive = FALSE))
colnames(DBFileList)[1]<-"FileSource"

DBFileList$FileSource<-as.character(DBFileList$FileSource)
Organization<-"KHSC"
DBFileList$Organization<-Organization
OrInd<-length(unlist(Organization))
#DBFileList$Organization<- Organization[[1]][OrInd-1]
DBFileList$Directory<-dirname(DBFileList$FileSource)
DBFileList$FileName<-basename(DBFileList$FileSource)
#DBFileList$text<-sapply(DBFileList$FileSource,read_file)
DBFileList$text<-iconv(sapply(DBFileList$FileSource,read_file),"UTF-8","ASCII",sub="")
i<-2
DBFileList[i,4]

### Begin search for minutes Key Search words embeded in program
DBMinutes<-DBFileList[0,] %>%
  select(FileName,Organization,text)

for(i in 1:4){#nrow(DBFileList)){
Begins<-str_locate_all(DBFileList[i,5],c(regex("\\nA regular meeting of the Board of Directors")))
Begins<-Convert2dFrame(Begins)

Ends<-str_locate_all(DBFileList[i,5],c(regex("\\n *Chair")))
Ends<-Convert2dFrame(Ends)

FoundMinutes<-FindMinutes(Begins,Ends,DBFileList[i,])

#DBMinutes$Date<DBFileList[i,"DATE"]
DBMinutes<-bind_rows(DBMinutes,FoundMinutes)
}
imissing<-anti_join(DBFileList,DBMinutes,by="FileName") %>%
  select(FileName)
missing
i<-1
for(i in 1:nrow(DBMinutes)){
  TheText<-DBMinutes[i,"MinuteText"]
  SAVEHere<-"~/home/skip21/BoardAnalytics/TheGreatOverhaul/SourceData/CombinedMinutes/ThunderBayText/TBayExtracted/TBay1"
  FILENAME<-paste0("TBay - ",DBMinutes[i,"FileName"],".csv",sep="")
  write_file(TheText,FILENAME)

}
str_sub(DBFileList[i,],17700,17735)
