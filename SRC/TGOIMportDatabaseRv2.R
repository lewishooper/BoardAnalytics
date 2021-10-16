#The GReat Overhaul
rm(list=ls())

#devtools::install_github("hadley/lineprof")
library(tools)
#library(readtext)
library(readr)
#library(ngram)
library(tm)
library(textstem)
#library(tesseract)
library(udpipe)
#eng <- tesseract("eng")
library(lubridate)
library(corpus)
#library(textreadr)
library(tidytext)
library(tidyverse)
library(stringr)
library(stringi)
#library(pdftools)
library(wordcloud)
### functions

MotionFinder<- function(row,FullMinutes,MotionDF,Begins,Ends){
  # This function pulls all the motions from the Minute text file ie. FullFiles[row,"text"]
  # find the next instance of MotionKeyWords from set beginning motion keywords 
  # and from there find the close of the motion'
  #Body of Function
  FoundMotions<-DBMotions[0,]
  MotionCounter<-0
  RemainingText<-FullMinutes
  StartPoint<-GetNextStartPoint(1,RemainingText,Begins)
  while(!is.na(StartPoint)){
    EndPoint<-GetNextEndPoint(StartPoint, RemainingText, Ends)
    
    NewMotionText<-substring(RemainingText, StartPoint, EndPoint)
    MotionCounter<-MotionCounter+1
    FoundMotions[MotionCounter,"Motion"]<-NewMotionText
    FoundMotions[MotionCounter,"START"]<-StartPoint
    FoundMotions[MotionCounter,"end"]<-EndPoint
    FoundMotions[MotionCounter,"Organization"]<-DBFileList[row,"Organization"]
    FoundMotions[MotionCounter,"Date"]<-DBFileList[row,"DATE"]
    FoundMotions[MotionCounter,"FileName"]<-DBFileList[row,"FileName"]
    FoundMotions[MotionCounter,"Sequence"]<-MotionCounter
    StartPoint<-EndPoint
    #RemainingText<-substr(RemainingText,StartPoint,str_length(RemainingText))
    StartPoint<-GetNextStartPoint(StartPoint,RemainingText,Begins)
    
  }
  #	End if no endpoint found then d0 and exit while loop
  #End While do loop
  
  #END OF FUNCTION Motionfinder
  return(FoundMotions)
}

GetNextStartPoint<-function(StartPoint,MinuteWords,MStartingWords){
  Bl<-data.frame(start=integer(),end=integer())
  df<-Bl
  for(i in 1:length(MStartingWords)){
    df<-as.data.frame(unlist(MStartingWords[[i]]))
    Bl<-bind_rows(df,Bl)
  }
  #Bl<-data.frame(unlist(Begins))
  Bl<-na.omit(Bl)
  Bl<-arrange(Bl,start)
  StartingPoint<-subset(Bl,start>StartPoint)
  
  return(StartingPoint[1,"start"])
}

GetNextEndPoint<-function(StartPoint,MinuteWords,Ends){
  Bl<-data.frame(start=integer(),end=integer())
  df<-Bl
  for(i in 1:length(Ends)){
    df<-as.data.frame(unlist(Ends[[i]]))
    Bl<-bind_rows(df,Bl)
  }
  #Bl<-data.frame(unlist(Begins))
  Bl<-na.omit(Bl)
  Bl<-arrange(Bl,end)
  StartingPoint<-subset(Bl,end>StartPoint)
  
  return(StartingPoint[1,"end"])
}

CleanDFText<-function(DF,TextColumn){
  
  #SentimentMotions<-SentimentMotions[,c(1,2,6)]
  DF[[TextColumn]]<-gsub(pattern="\\W", replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="\\d",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- tolower(DF[[TextColumn]])
  DF[[TextColumn]]<-removeWords(DF[[TextColumn]],stopwords("english"))
  DF[[TextColumn]]<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<-stripWhitespace(DF[[TextColumn]])
  #SentimentMotions$Motion<-str_split(SentimentMotions$Motion,pattern ="\\s+")
  return(DF)
}
LessCleanDFText<-function(DF,TextColumn){
  
  #SentimentMotions<-SentimentMotions[,c(1,2,6)]
  DF[[TextColumn]]<-gsub(pattern="\\W", replace=" ",DF[[TextColumn]])
  #DF[[TextColumn]]<- gsub(pattern="\\d",replace=" ",DF[[TextColumn]])
  #DF[[TextColumn]]<- tolower(DF[[TextColumn]])
  # DF[[TextColumn]]<-removeWords(DF[[TextColumn]],stopwords("english"))
  #DF[[TextColumn]]<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<-stripWhitespace(DF[[TextColumn]])
  #SentimentMotions$Motion<-str_split(SentimentMotions$Motion,pattern ="\\s+")
  return(DF)
}


SentenceEnd<-function(TheText,Startend)
  {
  EndCode<-regex("\\r\\n{1,6}")
  EndCode<-ifelse(str_detect(TheText,EndCode),EndCode,regex("\\n{1,6}"))
    Interim<-str_locate(str_sub(TheText,Startend,-1),EndCode)   # May need to add |\\n{1,6}
return(Interim[1,2]+Startend+1)
}

### Constants
getwd()
###########note THIS VERISION USED DOCILLION FROM PDF TO TEXT WITH OCR
DBMotions<-data.frame(FileName=character(0),Motion=character(0),Organization=character(0),Date=POSIXct(),
                      Sequence=numeric(0),stringsAsFactors = FALSE)
getwd()
DBSource<-(c("~/BoardAnalytics/TheGreatOverhaul/SourceData/ONTAllHospitalMinutesAsText"))
RDSfiles<-c("Not Convertable")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
#OtherData<-c("ChathamOldBoard/Results")


DBFileList<-as.data.frame(list.files(DBSource,full.names = TRUE,all.files=FALSE,recursive = FALSE))

colnames(DBFileList)[1]<-"FileSource"

colnames(DBFileList)[1]<-"FileSource"

DBFileList$FileSource<-as.character(DBFileList$FileSource)
Organization<-str_split(DBFileList[1,1],"/")
OrInd<-length(unlist(Organization))
#DBFileList$Organization<- Organization[[1]][OrInd-1]
DBFileList$Directory<-dirname(DBFileList$FileSource)
DBFileList$FileName<-basename(DBFileList$FileSource)
#DBFileList$text<-sapply(DBFileList$FileSource,read_file)
DBFileList$text<-iconv(sapply(DBFileList$FileSource,read_file),"UTF-8","ASCII",sub="")
#DBFileList$length<-lengths(gregexpr("\\W+", DBFileList$text)) + 1
DBFileList$Organization<-str_extract(DBFileList$FileName,regex("\\w* -"))
DBFileList$Organization<-str_remove(DBFileList$Organization," -")
DBFileList <-DBFileList%>%
  filter(Organization!="ChathamKentOld")


# date clean up
FilesByOrg<-DBFileList %>%
  select(Organization,FileName) %>%
  group_by(Organization) %>%
  mutate(count=n()) %>%
  select(Organization,count) %>%
  unique()

Months<-c("Decemer","DECEMER",month.name,collapse=NULL)
Months<-c(Months,toupper(month.name))

#Months<-c(Months,tolower(month.name))


DBFileList<-DBFileList %>%
  mutate(openingString=stripWhitespace(str_sub(DBFileList$text,1,700)))
  
row<-1145
DBFileList[row,"FileName"]
#DBFileList[row

for(row in 1:nrow(DBFileList)){
  # find month in Text of Meeting
  BL<-data.frame(str_locate(DBFileList[row,"openingString"],Months))
  BL<-BL[!is.na(BL$start), ]
  if(nrow(BL)>=2){BL<- BL %>% arrange(BL$start) %>% head(1)}
 Month<-substr(DBFileList[row,"openingString"],BL$start,BL$end)
 
 day<-parse_number(substring(DBFileList[row,"openingString"],BL$end+1,BL$end+3))
 year<-parse_number(substring(DBFileList[row,"openingString"],BL$end+4,BL$end+9))
 test<-paste(year,Month,day)
 DBFileList[row,"DATE"]<-parse_date_time(test,orders="ymd")
   ### Identifyfailed date conversions 
 rm(BL)
}
row
#check and prune minutes as needed Get rid of Agenda usually at front
## and CEO and other reports.  USUSALLy at end
# General methodology is specific to each hospital
# the Generic non identifity response is from the begining to end.
# The generic works for most of the files captured but not all files are that clean
MinuteList=list(HSCN=list("HSCN",StartM=c("MOTION:","The Board of Directors APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED","Carried","\\r\\n\\d{1,2}\\.\\d{0,2}")),
                LHSC=list("LHSC",StartM=c("It was MOVED","APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED",regex("\\n\\r\\d{1,2}\\.\\d"),"ADJOURNED by GENERAL CONSENT","\\n\\d{1,2}\\.\\d{0,2}")),
                Aliston=list("Aliston",StartsM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndsM=c("Carried","CARRIED","meeting terminated","Passed","passed")),
                AlmontGeneral=list("AlmontGeneral",StartM=c("IT WAS MOVED"),EndM=c("CARRIED","ADJOURNED")),
                Atikokan=list("Atikokan",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Dryden=list("Dryden",StartM=c("MOTION","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\\n\\r\\n")),   
                SickKids=list("SickKids",StartM=c("Moved","MOVED","On a Motion by","was moved by","MOTION","IT WAS RESOLVED","there being no further business"),EndM=c("Carried","CARRIED","ClosedSK","CloseSK","adjourned")),
                GrandRiver=list("GrandRiver",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                GenericHospital=list("GenericHospital",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                KDH=list("KDH",StartM=c("Motion:","ADJOURNMENT"),EndM=c("\r\\n\\d\\.\\d","\r\\n\\r\\n")),
                HaliburtonHighlands=list("HaliburtonHighlands",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                HHSC=list("HHSC",StartM=c("The Board of Directors APPROVED by GENERAL CONSENT","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                LACGH=list("LACGH",StartM=c("MOTION","Motion ","Adjournment"),EndM=c("The motion was carried.","MOTIONFAILEDTOPASS")),
                
                Mackenzie =list("Mackenzie",StartM=c("MOVED","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MicsGroup=list("MicsGroup",StartM=c("MOVED","Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MSH=list("MSH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NiagaraHealthSystem=list("NiagaraHealthSystem",StartM=c("It was MOVED","MOTION:","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NorfolkGeneral=list("NorfolkGeneral",StartM=c("On a Motion ","It was Moved","On a Motion by","MOVED BY:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                Northumberland=list("Northumberland",StartM=c("MOTION:","Moved","On a motion","On a Motion by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\\n\\d{1,2}\\.\\d{0,2}")),
                
                PerthSmithFalls=list("PerthSmithFalls",StartM=c("MOVED by","MOTION:","moved by","Moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                RMH=list("RMH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Sarnia=list("Sarnia",StartM=c("Motion duly made","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                SouthBruceGrey=list("SouthBruceGrey",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\n\r\n\r\n")),
                Temiskaming=list("Temiskaming",StartM=c("Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                TrilliumHospital=list("TrilliumHospital",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                WilliamOsler=list("WilliamOsler",StartM=c("MOVED ","MOVED/Seconded","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                StThomasElgin=list("StThomasElgin",StartM=c("MOVED","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                CambridgeMemorial=list("CambridgeMemorial",StartM=c("MOTION","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                SinaiHealth=list("SinaiHealth",StartM=c("Whereas","ADJOURNMENT"),EndM=c("resolved.+\\.","adjourned","ADJOURNMENT")),
                
                SunnyBrook=list("SunnyBrook",StartM=c("Upon MOTION duly made","Chair requested approval of the agenda","There being no further business"),EndM=c("CARRIED","Agenda was approved as circulated","MEETING TERMINATED","Chair adjourned the open session","the Chair terminated the Organizational Meeting")),
                Barrie=list("Barrie",StartM=c("duly moved, seconded and carried","ADJOURNMENT"),EndM=c("ClosedB","CloseB","Closedb","Closeb","\\(carried\\)","adjourned","RESOLVED.+\\.")),
                JoeBrant=list("JoeBrant",StartM=c("OpenJB","The Chair requested a motion","duly moved","MOVED","Moved by"),EndM=c("ClosedJB","Carried","CARRIED","adjourned ")),
                
                ChathamKent=list("ChathamKent",StartM=c("IT WAS AGREED THAT","Motion","Moved by","OpenCK","There being no further business"),EndM=c("Carried","CARRIED","The motion was carried.","approved by the Supervisor for all three boards","ClosedCK","adjourned")),
                LakeridgeHealth=list("LakeridgeHealth",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                KHSC=list("KHSC",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","meeting terminated")),
                TBay=list("TBay",StartM=c("Moved by:","MOVED"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                UHN=list("UHN",StartM=c("Moved","MOVED","Management recommends that the Board approve","Upon motion","Upon motion made","On a motion duly made","that the board of trustees approve",
                                        "Adjournment","Management requests that the Board of Trustees approve","Motion for Approval"),
                         EndM=c("\\n\\d{1,2}\\.\\d{1,2}","\r\\n\\t\\d\\.\\d","\r\\n\\w\\)","There being no further business, the meeting was adjourned","the meeting of the Board of Trustees was adjourned."))
)
i<-1
for(i in 1:nrow(DBFileList)){
  TargetHospital<-DBFileList[i,5]
  Begins<-str_locate_all(DBFileList[i,4],unlist(MotionList[[grep(DBFileList[i,5],MotionList)]][2]))
  Ends<-str_locate_all(DBFileList[i,4],unlist(MotionList[[grep(DBFileList[i,5],MotionList)]][3]))
  
  FoundMotions<-MotionFinder(i,DBFileList[i,"text"],DBMotions,Begins,Ends)
  
  FoundMotions$Date<DBFileList[i,"DATE"]
  DBMotions<-bind_rows(DBMotions,FoundMotions)
  
}




### Tokenize sententences prior to any cleanup


BoardSentences<- DBFileList %>%
  group_by(Organization)%>%
 # mutate(text=gsub(pattern="\\W", replace=" ",text)) %>%
 # mutate(text=gsub(pattern="\\b[A-z]\\b{1}",replace=" ",text)) %>%
  #mutate(text=stripWhitespace(text)) %>%
  mutate(text=str_squish(text)) %>%
  mutate(text=gsub('( \\D)\\.',"\\1", text)) %>%
  mutate(text=gsub(' mr\\.', " Mr", text))%>%
  mutate(text=gsub(' Mr\\.', " Mr", text))%>%
  mutate(text=gsub(' mrs\\.', " Mrs", text))%>%
  mutate(text=gsub(' Mrs\\.', " Mrs", text))%>%
  mutate(text=gsub(' Miss\\.', " Miss", text))%>%
  mutate(text=gsub(' Dr\\.', " Dr", text))%>%
  mutate(text=gsub(' dr\\.', " Dr", text))%>%
  mutate(text=gsub(' St\\.', " St", text))%>%
  mutate(text=gsub(' Inc\\.', " Inc", text))%>%
  mutate(text=gsub(' ref\\.', " ref-", text))%>%
  mutate(text=gsub("(\\d)\\. ","\\1-", text))%>%
  mutate(text=gsub("\n(\\d\\d)\\.","\\1-", text))%>%
  mutate(text=gsub("(\\s+\\d)\\.(\\d)","\\1-\\2", text))%>%
  mutate(text=gsub("(\\s+\\d)\\.(\\d)","\\1-\\2", text))%>%
  tidytext::unnest_tokens(SENTENCES,text,token="sentences",to_lower=FALSE)




saveRDS(BoardSentences,file.path(CompletedFiles,"BoardSentencesFile.rds"))

### This Section

#NEWFIND MOTIONS
#GenericHospital=list("GenericHospital",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),

MotionList=list(HSCN=list("HSCN",StartM=c("MOTION:","The Board of Directors APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED","Carried","\\r\\n\\d{1,2}\\.\\d{0,2}")),
                LHSC=list("LHSC",StartM=c("It was MOVED","APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED",regex("\\n\\r\\d{1,2}\\.\\d"),"ADJOURNED by GENERAL CONSENT","\\n\\d{1,2}\\.\\d{0,2}")),
                Aliston=list("Aliston",StartsM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndsM=c("Carried","CARRIED","meeting terminated","Passed","passed")),
                AlmontGeneral=list("AlmontGeneral",StartM=c("IT WAS MOVED"),EndM=c("CARRIED","ADJOURNED")),
                Atikokan=list("Atikokan",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Dryden=list("Dryden",StartM=c("MOTION","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\\n\\r\\n")),   
                SickKids=list("SickKids",StartM=c("Moved","MOVED","On a Motion by","was moved by","MOTION","IT WAS RESOLVED","there being no further business"),EndM=c("Carried","CARRIED","ClosedSK","CloseSK","adjourned")),
                GrandRiver=list("GrandRiver",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                GenericHospital=list("GenericHospital",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                KDH=list("KDH",StartM=c("Motion:","ADJOURNMENT"),EndM=c("\r\\n\\d\\.\\d","\r\\n\\r\\n")),
                HaliburtonHighlands=list("HaliburtonHighlands",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                HHSC=list("HHSC",StartM=c("The Board of Directors APPROVED by GENERAL CONSENT","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                LACGH=list("LACGH",StartM=c("MOTION","Motion ","Adjournment"),EndM=c("The motion was carried.","MOTIONFAILEDTOPASS")),
                
                Mackenzie =list("Mackenzie",StartM=c("MOVED","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MicsGroup=list("MicsGroup",StartM=c("MOVED","Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MSH=list("MSH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NiagaraHealthSystem=list("NiagaraHealthSystem",StartM=c("It was MOVED","MOTION:","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NorfolkGeneral=list("NorfolkGeneral",StartM=c("On a Motion ","It was Moved","On a Motion by","MOVED BY:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                Northumberland=list("Northumberland",StartM=c("MOTION:","Moved","On a motion","On a Motion by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\\n\\d{1,2}\\.\\d{0,2}")),
                
                PerthSmithFalls=list("PerthSmithFalls",StartM=c("MOVED by","MOTION:","moved by","Moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                RMH=list("RMH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Sarnia=list("Sarnia",StartM=c("Motion duly made","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                SouthBruceGrey=list("SouthBruceGrey",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\n\r\n\r\n")),
                Temiskaming=list("Temiskaming",StartM=c("Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                TrilliumHospital=list("TrilliumHospital",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                WilliamOsler=list("WilliamOsler",StartM=c("MOVED ","MOVED/Seconded","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                StThomasElgin=list("StThomasElgin",StartM=c("MOVED","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                CambridgeMemorial=list("CambridgeMemorial",StartM=c("MOTION","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                SinaiHealth=list("SinaiHealth",StartM=c("Whereas","ADJOURNMENT"),EndM=c("resolved.+\\.","adjourned","ADJOURNMENT")),
               
                SunnyBrook=list("SunnyBrook",StartM=c("Upon MOTION duly made","Chair requested approval of the agenda","There being no further business"),EndM=c("CARRIED","Agenda was approved as circulated","MEETING TERMINATED","Chair adjourned the open session","the Chair terminated the Organizational Meeting")),
                Barrie=list("Barrie",StartM=c("duly moved, seconded and carried","ADJOURNMENT"),EndM=c("ClosedB","CloseB","Closedb","Closeb","\\(carried\\)","adjourned","RESOLVED.+\\.")),
                JoeBrant=list("JoeBrant",StartM=c("OpenJB","The Chair requested a motion","duly moved","MOVED","Moved by"),EndM=c("ClosedJB","Carried","CARRIED","adjourned ")),
    
                ChathamKent=list("ChathamKent",StartM=c("IT WAS AGREED THAT","Motion","Moved by","OpenCK","There being no further business"),EndM=c("Carried","CARRIED","The motion was carried.","approved by the Supervisor for all three boards","ClosedCK","adjourned")),
                LakeridgeHealth=list("LakeridgeHealth",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                KHSC=list("KHSC",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","meeting terminated")),
                TBay=list("TBay",StartM=c("Moved by:","MOVED"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                UHN=list("UHN",StartM=c("Moved","MOVED","Management recommends that the Board approve","Upon motion","Upon motion made","On a motion duly made","that the board of trustees approve",
                                        "Adjournment","Management requests that the Board of Trustees approve","Motion for Approval"),
                         EndM=c("\\n\\d{1,2}\\.\\d{1,2}","\r\\n\\t\\d\\.\\d","\r\\n\\w\\)","There being no further business, the meeting was adjourned","the meeting of the Board of Trustees was adjourned."))
)
i<-1335

### TESTGRROUND
test<-c("On a motion duly made, seconded and carried, the Board of Trustees approved the minutes |
  _of the Board of Trustees’ meetings held on February 20 and April 17,2019.
2.2 Approval of Minutes and Embedded Motions
Pre-circulated for members’ approval were the Finance and Audit Committee")
str_locate_all(test,regex("\\n\\d{1,2}\\.\\d{1,2}"))
DBFileList[i,"FileName"]
for(i in 1:nrow(DBFileList)){
  
  Begins<-str_locate_all(DBFileList[i,4],unlist(MotionList[[grep(DBFileList[i,5],MotionList)]][2]))
  Ends<-str_locate_all(DBFileList[i,4],unlist(MotionList[[grep(DBFileList[i,5],MotionList)]][3]))
  
  FoundMotions<-MotionFinder(i,DBFileList[i,"text"],DBMotions,Begins,Ends)
  
  FoundMotions$Date<DBFileList[i,"DATE"]
  DBMotions<-bind_rows(DBMotions,FoundMotions)
  
}
i
#str_locate("TESTME","\r\\n\\d{1,2}\\.\\d")
## Check motions
MotionCheck<-DBMotions %>%
  
  group_by(Organization) %>%
  mutate(MotionCount=n()) %>%
  select(Organization,MotionCount) %>%
  unique()
MeetingCheck<-DBFileList %>%
  select(Organization,FileName) %>%
  unique() %>%
  group_by(Organization) %>%
  mutate(MeetingCheck=n()) %>%
  select(Organization,MeetingCheck) %>%
  unique()
MMChecks<-left_join(MeetingCheck,MotionCheck) 
MMChecks$avgMperM=MMChecks$MotionCount/MMChecks$MeetingCheck

DBMotions$words<-lengths(gregexpr("\\W+", DBMotions$Motion)) + 1
#WriteMotions<-DBMotions
#WriteMotions<-CleanDFText(WriteMotions,2)
#WriteMotions$Motion<-gsub(pattern="\\n", replace="",WriteMotions$Motion)
#WriteMotions$Motion<-gsub(pattern="\\r", replace="",WriteMotions$Motion)
write_csv(DBMotions,file.path(CompletedFiles,"MotionsCSVexcl.csv"))
####################################
###########Clean and Save 
######### Includes lemmanization#########
#########

#CleanFileList<-CleanDFText(DBFileList,"text")
#LessClean<-LessCleanDFText(DBFileList,"text")

# count total words in each file except the pipe
#DBFileList$NewCleanWords<-str_count(DBFileList$text, '\\s+')+1
#CleanFileList$TotalCleanWords<- str_count(CleanFileList$text, '\\s+')+1
#LessClean$TotalCleanWords<- str_count(LessClean$text, '\\s+')+1
#Tokenize at word level with UDPIPE... Takes time

#Data Pipe section can be  turned off for when needed, but used for comparing fundamentsls

ud_modelY<-udpipe_download_model(language="english")
#str(ud_modelY)
ud_modelY<-udpipe_load_model(ud_modelY$file_model)
#x<-udpipe_annotate(ud_model,x=comments$feedback)
#x<-as.data.frame(x)

#DBPipe<-udpipe_annotate(ud_modelY,x=CleanFileList$text,doc_id = CleanFileList$FileName)
#DBPipe<-as.data.frame(DBPipe)
#DBPipe$Organization<-DBFileList$Organization[match(DBPipe$doc_id,DBFileList$FileName)]
FullPipe<-udpipe_annotate(ud_modelY,x=DBFileList$text,doc_id = DBFileList$FileName)
FullPipe<-as.data.frame(FullPipe)
FullPipe$Organization<-DBFileList$Organization[match(FullPipe$doc_id,DBFileList$FileName)]
FullPipe$DATE<-DBFileList$DATE[match(FullPipe$doc_id,DBFileList$FileName)]

#DBPipe$Organization<-DBFileList$Organization[match(DBPipe$doc_id,DBFileList$FileName)]
#DBPipe$TAG1<-DBFileList$TAG1[match(DBPipe$doc_id,DBFileList$FileName)]
#DBPipe$Date<-DBFileList$DATE[match(DBPipe$doc_id,DBFileList$FileName)]

#MotionPipe$fileLength<-str_locate(MotionPipe$FileName,"-")

#MotionPipe$FileName<-substr(MotionPipe$FileName,1,MotionPipe$fileLength-1)
MotionPipe<-udpipe_annotate(ud_modelY,x=DBMotions$Motion,doc_id = paste(DBMotions$FileName,DBMotions$Sequence))
MotionPipe<-as.data.frame(MotionPipe)
MotionPipe$Organization<-DBFileList$Organization[match(MotionPipe$doc_id,DBFileList$FileName)]
MotionPipe$DATE<-DBFileList$DATE[match(MotionPipe$doc_id,DBFileList$FileName)]


#saveRDS(DBMotions,file.path(CompletedFiles,"FullMotioion.rds"))
DBMotions$lemma<-lemmatize_strings(DBMotions$Motion)

### Tooo much is being saved, and leaving too many choices about what is next. 

## maybe leave "Clean up" to thesubsequent users


saveRDS(DBMotions,file.path(CompletedFiles,"FullMotions.rds"))
saveRDS(DBFileList,file.path(CompletedFiles,"FullFiles.rds"))
saveRDS(FullPipe,file.path(CompletedFiles,"FullPipe.rds"))
saveRDS(MotionPipe,file.path(CompletedFiles,"MotionPipe.rds"))
# Consider dropping these files. 
#saveRDS(LessClean,file.path(CompletedFiles,"FullFilesNoSpace.rds"))
#saveRDS(CleanFileList,file.path(CompletedFiles,"CleanFiles.rds"))
#saveRDS(CleanLemmatizedFiles,file.path(CompletedFiles,"CleanLemmatized.rds"))
#saveRDS(NewsestFile,file.path(CompletedFiles,"Newestfile.rds"))
#write_csv(NewsestFile,file.path(CompletedFiles,"Newestfile.csv"))
#saveRDS(DBPipe,file.path(CompletedFiles,"CleanPipe.rds"))

#saveRDS(CleanMotions,file.path(CompletedFiles,"CleanMotions.rds"))
#saveRDS(DBMotions,file.path(CompletedFiles,"CleanLemmatizedMotions.rds"))

