###
# Create Sparsity Index
#
# overview
rm(list=ls())
#Method A
library(tidyverse)
library(busdater)
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
#1) For each organization calculate the total number of words for the organization (all recorded meetings)  
#2) Divide total by organization by the number of meetings recorded
#3) Calculate average of all organizations (this is basically the Ontario average words per meeting
#4) Divide each organization by the Average of all meetings
#5) The result for each organization is their sparsity score.
#Check results against org type…. May be different for academic organizations>?>
library(tidyverse)
getwd()
SourceFiles <- c("~/BoardAnalytics/TheGreatOverhaul/Results")
SavedGraphics <- c("~/BoardAnalytics/TheGreatOverhaul/Graphics/Other")
OtherData<-c("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData")                                           
SparsityFile<-readRDS(file.path(SourceFiles,"FullPipe.rds"))
SparsityFileSummary<-SparsityFile%>%
  group_by(Organization) %>%
  mutate(TotalWords=n()) %>%
  ungroup()%>%
 dplyr::select(1,17,16,15) %>%
  unique() %>%
  group_by(Organization) %>%
  mutate(TotalMeetings=n()) %>%
  mutate(WordsPerMeeting=TotalWords/TotalMeetings) %>%
  dplyr::select(2,4,5,6) %>%
  unique() %>%
  ungroup()
#SparsityFileSummary2 <- SparsityFileSummary 
 AllWords<-sum(SparsityFileSummary$TotalWords)
AllMeetings<-sum(SparsityFileSummary$TotalMeetings)
SparsityFileSummary$AvgAllOrganizations<-AllWords/AllMeetings
SparsityFileSummary<-SparsityFileSummary %>%
  ungroup() %>%
  mutate(MeanL2=mean(WordsPerMeeting)) %>%
  mutate(Median=median(WordsPerMeeting)) %>%
  mutate(MedianIndex=WordsPerMeeting/Median)

saveRDS(SparsityFileSummary,file.path(SourceFiles,"Sparsityindex.rds"))
ggplot(SparsityFileSummary,aes(y=reorder(Organization,MedianIndex),x=MedianIndex))+geom_point()

SparsityByYear<-readRDS(file.path(SourceFiles,"FullPipe.rds"))
SparsityByYearSummary<-SparsityByYear%>%
  group_by(Organization,doc_id) %>%
  mutate(WordsPerMeeting=n()) %>%
  ungroup()%>%
  dplyr::select(1,17,16,15) %>%
  unique() %>%
  group_by(Organization) %>%
  mutate(TotalMeetings=n()) %>%
  ungroup()
SparsityYearByYear<-SparsityByYearSummary %>%
  ungroup()%>%
  mutate(FY=get_fy(DATE,opt_fy_start="04-01")) %>%
  mutate(FYEnd=paste0(FY-1,"/",FY,"YE")) %>%
  group_by(Organization,FY)%>%
  mutate(MeanperMeetingPerYear=mean(WordsPerMeeting)) %>%
  mutate(TotalWordsPerYear=sum(WordsPerMeeting)) %>%
  mutate(MeetingsPerFY=n()) %>%
  ungroup() %>%
  group_by(Organization)%>%
  mutate(meanMeetingsPerFY=mean(MeetingsPerFY)) %>%
 # filter(MeetingsPerFY>=5) %>%
  distinct(Organization,FY,.keep_all = TRUE) %>%
  ungroup() %>%
  mutate(totalWords=sum(TotalWordsPerYear)) %>%
  mutate(meanWordsPerYear=mean(TotalWordsPerYear)) %>%
  mutate(yearlySparsityIndex=TotalWordsPerYear/meanWordsPerYear)
 write_csv(SparsityByYear,"DumpFile.csv")

ggplot(SparsityYearByYear,aes(x=TotalWordsPerYear,y=MeetingsPerFY)) +geom_smooth()  
testLM<-lm(TotalWordsPerYear~MeetingsPerFY,data=SparsityYearByYear)
summary(testLM)
