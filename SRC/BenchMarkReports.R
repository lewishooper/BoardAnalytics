#BenchMarkReportScript

## clean up
rm(list=ls())
# Libraries
library(tidyverse)
library(tidytext)
library(lubridate)
library(gridExtra)
## Functions
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

## setup files
getwd()
RWorkingFiles<-"Results"
Graphics<-"Graphics/Benchmark"
OtherData<-"SourceData/OtherData"
BFandS<-readRDS(file.path(RWorkingFiles,"BlockFocusAndSentiment.rds"))
NameAndType<-readRDS("SourceData/OtherData/NameAndType.rds")
BFandS<-readRDS(file.path(RWorkingFiles,"BlockFocusAndSentiment.rds")) %>%
  left_join(NameAndType,by=("Organization")) %>%
  select(Organization,FAC,FacilityType,everything())

FocusByYear<-BFandS %>%
  # filters if needed
  mutate(Year=year(Date)) %>%
  select(FAC,Organization,FacilityType,Year,c(ConsentAgenda:Finance)) %>%
  group_by(Organization,Year,FacilityType) %>%
  summarise(across(ConsentAgenda:Finance, sum)) %>%
  mutate(FocusSum = rowSums(across(ConsentAgenda:Finance))) %>%
  group_by(Organization,Year,FacilityType)%>%
  summarise(across(ConsentAgenda:Finance)/FocusSum) %>%
  ungroup() %>%
  pivot_longer(ConsentAgenda:Finance,names_to="FocusArea",values_to = "FocusRate")
  
SentimentByYear<-BFandS %>%
  # filters if needed
  mutate(Year=year(Date)) %>%
  select(FAC,Organization,FacilityType,Year,c(positive:negative)) %>%
  group_by(Organization,Year,FacilityType) %>%
  replace(is.na(.), 0) %>%
  summarise(across(positive:negative, sum)) %>%
  mutate(FocusSum = rowSums(across(positive:negative),na.rm = TRUE)) %>%
  group_by(Organization,Year,FacilityType)%>%
  summarise(across(positive:negative)/FocusSum) %>%
  ungroup() %>%
  pivot_longer(positive:negative,names_to="SentimentArea",values_to = "SentimentSum")


##Create template for graphs
Hospital<-"Barrie"
SMethod<-"loess"
FocusGraph<-"Quality"
p1<-ggplot(FocusByYear[FocusByYear$FocusArea=="Quality",],aes(x=Year,y=FocusRate))+geom_smooth(method=SMethod,color="red")+
  geom_smooth(data=FocusByYear[FocusByYear$Organization==Hospital & FocusByYear$FocusArea=="Quality",],method=SMethod)

FocusGraph<-"Finance"
p2<-ggplot(FocusByYear[FocusByYear$FocusArea=="Finance",],aes(x=Year,y=FocusRate))+geom_smooth(method=SMethod,color="red")+
  geom_smooth(data=FocusByYear[FocusByYear$Organization==Hospital & FocusByYear$FocusArea=="Finance",],method=SMethod)

p3<-grid.arrange(p1, p2, nrow = 1)
g <- arrangeGrob(p1, p2,nrow=1) #generates g
ggsave(file="AGAIN.png", p3) #saves p3
