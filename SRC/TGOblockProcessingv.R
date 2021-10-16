
### Sequence 
## first readbyheading3.r
## Then AssignFocusandSentimentv2<<---- We Are Here
## then blockprocessing.r ---> Create riverbed and sentiment graphs????

rm(list=ls())

library(readxl)
#library(busdater)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(zoo)
library(tidyquant)
#DBSource<-("~/UbBig/TextAnalysis/SourceData/ONTAllHospitalMinutesAsText")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
OtherData<-("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData") 
SavedGraphics<-("~/BoardAnalytics/TheGreatOverhaul/Graphics") 
SentimentGraphics<-("~/BoardAnalytics/TheGreatOverhaul/Graphics/Sentiment") 
FocusGraphics<-("~/BoardAnalytics/TheGreatOverhaul/Graphics/Focus") 
BlockFiles<-readRDS(file.path(CompletedFiles,"BlockFocusAndSentiment.rds"))
# calculate some constants, Words per Meeting, Words per Organization, Words per block, FYyears and Number of Years

BlockRiverBed<-BlockFiles %>%
  mutate(blockSize=lengths(gregexpr("[A-z]\\W+", BlockText)) + 1L) %>%
  group_by(FileName) %>%
  mutate(MeetingSize=sum(blockSize)) %>%
  ungroup()%>% group_by(Organization) %>%
  mutate(OrgSize=sum(MeetingSize)) %>%
  ungroup() %>%
  select(c(1:4,8,32:34,11:26)) %>%
  ungroup() %>%
pivot_longer(cols=9:24,names_to ="FocusArea",values_to = "FocusValue") %>%
  mutate(FileWt=FocusValue/MeetingSize) %>%
  mutate(BlockWt=FocusValue/blockSize) %>%
  mutate(LogicWt=ifelse(FocusValue==0,0,1))
ggplot(BlockRiverBed,aes(FocusArea,FileWt))+geom_point()+coord_flip()


OntarioRiverBedData<-tibble(BlockRiverBed[,-c(12,13)]) %>%
  dplyr::select(1,2,8,9,10,11) %>%
  mutate(newID=row_number()) %>%
  filter(FileWt>0)%>%
  pivot_wider(names_from = "FocusArea",values_from  ="FileWt") %>%
  replace(is.na(.), 0) %>%
  group_by(Organization)%>%
  summarize_at(vars(4:19), list(sum)) %>% #### NOTE behaviour here newID is dropped by Group_by
  rowwise() %>%
  mutate(m = sum(c_across(3:17)))%>%
  pivot_longer(cols=c(3:17),names_to= "FocusArea",values_to="PercentWt") %>%
  mutate(PercentWt=PercentWt/m*100) 
  #pivot_wider(names_from = "FocusArea",values_from = "PercentWt")
ggplot(OntarioRiverBedData,aes(FocusArea,PercentWt))+geom_point()+coord_flip()
  
RiverBedSummary<-OntarioRiverBedData %>%
  group_by(FocusArea) %>%
  dplyr::summarise(banks = quantile(PercentWt, probs=c(0.10,0.5,0.90))) %>%
  # spread(RiverBedSummary,key = FocusArea,value=banks)
  # unnest(col=banks) %>%
  #group_by(FocusArea)%>%
  mutate(m=c("m1","m2","m3") ) %>%
  pivot_wider(id_cols=FocusArea,names_from=m,values_from=banks)

########BASELINE GRAPHIC
#BaseBed<- png::readPNG("Results/OtherData/for Publication/BaseGraphLarge.png")
#BaseRiverRast <- rasterGrob(BaseBed,interpolate = TRUE)
XMIN<-5
XMAX<-11
YMIN<-25
YMAX<-31
X<-XMIN
Y<-YMAX
YADJST<-YMAX*.05
GroupText<-"The Black Line \u2014\u2014 is the \n50th Percentile for Ontario"
AreaText<-"The Light Blue area Represents the 10th to \n90th Perentile Variation for Ontario"
#HospText<-paste("___ is the mean for ",WhichResults)
#TextString<-paste("Black Line is 50thPercentile"," \n\nGray area is from 90th to 10th Percentile","\n\n\n\nRed Line=",WhichResults)
labels=paste("60\u2014\u2014","80", sep="")
h <- ggplot(RiverBedSummary, aes(reorder(FocusArea,m2))) 
h+geom_ribbon(aes(ymin = m1, ymax = m3,group="FocusArea"),fill="#69D2E7",color="gray")+
  geom_line(aes(y = m2,group="FocusArea"),color="black")+
  ylab("Percent of Focus on Area")+
  xlab("Board Focus Area")+
  #scale_y_continuous(labels=percent)+
  labs(title="Percent of Board Focus by Area",subtitle = paste(" For 32 Ontario Hospitals."))+
  theme(axis.text.x = element_text(angle =45, hjust = 1,size=8))+
  ggplot2::annotate("rect",xmin=XMIN,ymin=YMIN,xmax=XMAX,ymax=YMAX,fill="gray",color="red",alpha=.75,size=.25)+
  #ggplot2::annotate("rect",xmin=XMIN+1,ymin=YMIN+.29,xmax=XMAX-1,ymax=YMAX,fill="gray",color="blue",alpha=.75,size=.25)+
  ggplot2::annotate(geom = "text", x =X+3, y =Y-YADJST, label = AreaText,hjust = "centre",size=2.5,color="blue")+
  ggplot2::annotate(geom = "text", x =X+3, y =Y-2.4*YADJST, label = GroupText,hjust = "center",size=2.5,color="gray10")
#ggplot2::annotate(geom = "text", x =X+3, y =Y-3*YADJST, label = HospText,hjust = "center",size=2.5,color="red")
FileName <- paste(SavedGraphics, "/Focus/","Ontario Overall", "Riverbed.png", sep = "")

ggsave(FileName, device = "png", dpi = "print", width = 6, height = 6)




## for Each Hospital Graphic
Skipspalate <- c("#69D2E7", "#A7DBD8", "#E0E4CC", "#F38630", "#FA6900")
row<-"Barrie -ON"
for(row in unique(OntarioRiverBedData$Organization) ) {
  WhichResults<-row
  GroupText<-"The Black Line \u2014\u2014 is the \n50th Percentile for Ontario"
  AreaText<-"The light blue area Represents the 10th to \n90th Perentile Variation for Ontario"
  
  HospText<-paste("\u2014\u2014 is the mean for ",WhichResults)
  
  # TextString<-paste("Black Line is 50thPercentile"," \n\nGray area is from 90th to 10th Percentile","\n\n\n\nRed Line=",WhichResults)
  h <- ggplot(RiverBedSummary, aes(reorder(FocusArea,m2))) 
  h+geom_ribbon(aes(ymin = m1, ymax = m3,group="FocusArea"),fill="#69D2E7",color="gray",alpha=.65)+
    geom_line(aes(y = m2,group="FocusArea"),color="black")+
    geom_line(data=OntarioRiverBedData[OntarioRiverBedData$Organization==row,],aes(x=FocusArea,y=PercentWt,group="FocusArea"),color="red")+
    ylab("Percent of Focus on Area")+
    xlab("Board Focus Area")+
    ggplot2::annotate("rect",xmin=XMIN,ymin=YMIN,xmax=XMAX,ymax=YMAX,fill="gray",color="red",size=.25)+
    #ggplot2::annotate("rect",xmin=XMIN+1,ymin=YMIN+.29,xmax=XMAX-1,ymax=YMAX,fill="gray",color="blue",alpha=.75,size=.25)+
    ggplot2::annotate(geom = "text", x =X+3, y =Y-YADJST, label = AreaText,hjust = "center",size=2.5,color="blue")+
    ggplot2::annotate(geom = "text", x =X+3, y =Y-2.35*YADJST, label = GroupText,hjust = "center",size=2.5,color="black")+
    ggplot2::annotate(geom = "text", x =X+3, y =Y-3.35*YADJST, label = HospText,hjust = "center",size=2.5,color="red")+
    
    #scale_y_continuous(labels=percent)+
    labs(title=paste(WhichResults,"  Percent of Board Focus by Area"),subtitle = " Compared to Ontario Hospitals.",caption="Based on Analysis of Sentences")+
    theme(axis.text.x = element_text(angle =45, hjust = 1))
  
  
  #rlang::last_error()  
  
  FileName <- paste(SavedGraphics, "/Focus/", row, "Riverbed.png", sep = "")
  
  ggsave(FileName, device = "png", dpi = "print", width = 6, height = 6)
}

#### Sentiment Analysis 
SentimentData<-readRDS(file.path(CompletedFiles,"BlockFocusAndSentiment.rds")) %>%
  select(c(1:8,27,31,30,29))


### Calcualte begin and end dates of data for each organzation
OrgList<-SentimentData %>%
  mutate(DATE=DATEString)%>%
  mutate(DATE2=zoo::as.Date(as.integer(DATEString)))%>%
  dplyr::select(Organization,DATE,Date,DATE2) %>%

  group_by(Organization) %>%
 # mutate(DATE=as.Date(Date, "%Y-%m-%d"))
  mutate(MaxDate=max(DATE2)) %>%
  mutate(MinDate=min(DATE2))%>%
  ungroup() %>%
  distinct(Organization,MaxDate,MinDate) %>%
  arrange(MaxDate)
#filter(MaxDate>=as.POSIXct("2020-01-01")) 

#### REVISED FIHAL


OntarioHospitals<-SentimentData %>%  # calculate SEntiment words per 100 Meeting Words
  ungroup() %>%
  mutate(BaseYear=year(DATE))

  mutate(DATE2=zoo::as.Date(as.integer(DATEString)))%>%
  #dplyr::select(-c(7:23)) %>%
  mutate(YMDate=floor_date(DATE2,unit="month")) %>%  ## so we can group meetings by month
  #group_by(YMDate) %>%
  
  
  group_by(Organization,DATE2) %>%
  select(-c(DATE,Date,DATEString,)) %>%
  mutate(wordsPerMeeting=sum(BlockSize)) %>%
  ungroup() %>%
  group_by(YMDate) %>%
  mutate(NumOrgsThattDate=n_distinct(Organization)) %>%
  ungroup() %>%
  group_by(YMDate,Organization) %>%
  replace(is.na(.), 0)%>%
  #ungroup() %>%
  ## Next step is to calculate meanpostive score by Organiztin and databased on each sentence.
  #write.xlsx(OntarioHospitals,file.path(SourceFiles,"CheckSentimentosptials.xlsx"))
  summarize(across(c(BlockSize,positive,negative,trust),sum)) %>% # calcualte the sum of the sentiment by meeeting
  rename(wordsPerMeeting=BlockSize)%>%
  ungroup() %>%
  mutate(Positive=positive/wordsPerMeeting*100) %>%
  mutate(Trust=trust/wordsPerMeeting*100)%>%
  mutate(Negative=negative/wordsPerMeeting*100)%>%
  dplyr::select(-c("positive","negative","trust","wordsPerMeeting")) %>%
  pivot_longer(cols=c(Positive,Trust,Negative),names_to = "Sentiment",values_to="Value")
i<-30
NumberOrgs<-32
for(i in 1:nrow(OrgList)){
  hosp<-as.character(OrgList[i,"Organization"])
 TheHospital<-OntarioHospitals[OntarioHospitals$Organization==hosp,] %>%
    arrange(YMDate) %>%
    mutate(First=first(YMDate)) %>%
    mutate(Last=last(YMDate))
  MatchedHospitals<-OntarioHospitals %>%
    ungroup() %>%
    arrange(YMDate) %>%
    filter(YMDate>=TheHospital[1,"First"] & YMDate<=TheHospital[1,"Last"])

Matchedwith<-nrow(MatchedHospitals)

  ggplot(MatchedHospitals,aes(x=YMDate,y=Value,color=Sentiment))+geom_smooth(method ="loess",se=.05)+
    xlab("Date")+
    ylab("Sentiment Score per 100 words")+
    labs(title=paste("Ontario Sentiment Score \n",NumberOrgs, "Ontario Hospitals \n dotted line = ",hosp),caption=paste("Dates Match ",Matchedwith," Total Board Meetings\n" ,Sys.Date()))+
    geom_smooth(data=MatchedHospitals[MatchedHospitals$Organization==hosp,],aes(x=YMDate,y=Value,color=Sentiment),linetype = "dotted",method ="loess",se=FALSE)
   # coord_x_datetime(xlim = c(as.POSIXct(LowX$First),as.POSIXct(LowX$Last)))
  ggsave(file.path(SentimentGraphics,paste(hosp,"AdustedSentiment.png")),device="png",dpi="print",width=6,height=7)
  
}
warnings()
getwd()

