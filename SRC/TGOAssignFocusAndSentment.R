### Import  Block Files

### Sequence 
## first readbyheading3.r
## Then AssignFocusandSentimentv2<<---- We Are Here
## then blockprocessing.r ---> Create riverbed and sentiment graphs????
rm(list=ls())

library(readxl)
#library(busdater)
library(lubridate)
library(tidyverse)
library(tidytext)

#DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData")

CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")

#DBSource<-("~/UbBig/TextAnalysis/SourceData/ONTAllHospitalMinutesAsText")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
OtherData<-("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData") ####
#FociList<-base::load(~/BoardAnalytics/TheGreatOverhaul/FOCILIST.rData")
BlockFiles<-readRDS(file.path(CompletedFiles,"CorrectedBlockFiles.rds")) %>%  ## was just block files
select(1:10)
base::load("~/BoardAnalytics/TheGreatOverhaul/SourceData//OtherData/FOCILIST.rData")
SentimentData<-read_csv(file.path(OtherData,"OrgSentimentsExtraExclusions.csv"))
SentimentData<-SentimentData[is.na(SentimentData$Exclude),]  #Drop the Excluded words for Health Care
SentimentData<-SentimentData[c(1,4)]  # drop unused Coumns



Sys.time()
for(i in 1:nrow(BlockFiles)){
  BlockText<- str_squish(tolower(BlockFiles[i,"BlockText"]))
  
  for(row in 1:length(FociList)){
    NamedColumn<-FociList[[row]][1]
    TestPhrase<-regex(paste0(FociList[[row]][2:length(FociList[[row]])]))
    BlockFiles[i,NamedColumn]<-sum(str_count(BlockText,TestPhrase))
    
  }
}




Sys.time()
warnings()
saveRDS(BlockFiles,file.path(CompletedFiles,"BlockFocus.rds"))
print(paste("SentimentStart=",Sys.time()))
SentimentBlocks<- BlockFiles %>%
  ungroup() %>%
  dplyr::select(Organization,FileName,Heading,BlockText) %>%
  unnest_tokens(word,BlockText,token="words") %>%
  left_join(SentimentData,by="word") %>%
  #TestM<-SentimentData %>%
  #na.omit()%>%
  ungroup() %>%
  group_by(Organization,FileName,Heading) %>%
  mutate(BlockSize=n())  %>%
  dplyr::select(c(Organization,FileName,Heading,BlockSize,word,Sentiment)) %>%
  group_by(Organization,FileName,Heading,Sentiment) %>%
  mutate(SentimentCount=n()) %>%
  select(c(Organization,FileName,Heading,BlockSize,Sentiment,SentimentCount)) %>%
  unique() %>%
  pivot_wider(names_from=Sentiment,values_from=SentimentCount)
print(paste("Sentimenendt=",Sys.time()))
SentimentBlocks<-left_join(BlockFiles,SentimentBlocks,by=c("Organization","FileName","Heading")) 
Sys.time()

saveRDS(SentimentBlocks,file.path(CompletedFiles,"BlockFocusAndSentiment.rds"))

