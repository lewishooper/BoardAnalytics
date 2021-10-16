### Analyze frquency in blocks
## look for key words by date
## area one Specific keywords
rm(list=ls())
library(busdater)
## load files
library(tidyverse)
library(tidytext)
library(tm)
SourceFiles<-"~/BoardAnalytics/TheGreatOverhaul/Results"
BlockFiles<-readRDS(file.path(SourceFiles,"BlockFocusAndSentiment.rds"))
base::load("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/FOCILIST.rData")
 ### functions
CleanDFText<-function(DF,TextColumn){
  
  #SentimentMotions<-SentimentMotions[,c(1,2,6)]
  DF[[TextColumn]]<-gsub(pattern="\\W", replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="\\d",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="_{2,}",replace=" ",DF[[TextColumn]])
  #DF[[TextColumn]]<- tolower(DF[[TextColumn]])
  DF[[TextColumn]]<-removeWords(DF[[TextColumn]],stopwords("english"))
  DF[[TextColumn]]<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<-stripWhitespace(DF[[TextColumn]])
  #SentimentMotions$Motion<-str_split(SentimentMotions$Motion,pattern ="\\s+")
  return(DF)
}
 
## search for specific phrase in Block
MeetingsPerYear<-BlockFiles %>%
  mutate(fy=get_fy(Date,opt_fy_start="04-01")) %>%
  mutate(fy=paste0("fy",fy,"/",as.numeric(str_sub(fy,2,4))+1,seps="")) %>%
  group_by(Organization,fy)%>%
  select(Organization,fy,FileName) %>%
  unique() %>%
  summarise(MeetingperYear=n())



FoundPhrase<-BlockFiles %>%
  mutate(fy=get_fy(Date,opt_fy_start="04-01")) %>%
  mutate(fy=paste0("fy",fy,"/",as.numeric(str_sub(fy,2,4))+1,seps="")) %>%
  select(1,2,4,Date,fy,BlockText,everything())
  #filter(str_detect(BlockText,phrase))
  
  FoundPhrase<-left_join(FoundPhrase,MeetingsPerYear,by=c("Organization"="Organization","fy"="fy")) %>%
    select(1:6,33,everything())
FoundPhrase<-CleanDFText(FoundPhrase,"BlockText")
WordFreq<-FoundPhrase %>%
  select(1:5,ConsentAgenda:Finance) %>%
  unnest_tokens(word,BlockText,token="words",drop=FALSE,to_lower=FALSE) %>%
  group_by(word) %>%
  mutate(n=n())
AssociationGraph<-function(thisText,DF){
phrase<-c("nursing","tender")
 PhraseRiverbed<-CleanDFText(BlockFiles,"BlockText") %>%
   mutate(fy=get_fy(Date,opt_fy_start="04-01")) %>%
   mutate(fy=paste0("fy",fy,"/",as.numeric(str_sub(fy,2,4))+1,seps="")) %>%
   select(1,2,4,Date,fy,BlockText,everything()) %>%
   filter(str_detect(BlockText,phrase)) %>%
   mutate(KeyText= paste( unlist(phrase), collapse=', ')) %>%
   pivot_longer(cols=ConsentAgenda:Finance,names_to = "FocusArea",values_to = "value")
 
 RiverBedSummary<-PhraseRiverbed %>%
   group_by(FocusArea) %>%
   dplyr::summarise(sumFA = sum(value))
   # spread(RiverBedSummary,key = FocusArea,value=banks)
   # unnest(col=banks) %>%
   #group_by(FocusArea)%>%

return(RiverBedSummary)
}
FociList[[2]]
p1<-AssociationGraph(FociList[[2]],BlockFiles)
#ggplot(p1,aes(x=reorder(FocusArea,-sumFA),y=sumFA))+geom_col()+
  #theme_economist()+
 # theme(axis.text.x = element_text(angle = 40,hjust=1))+
  #xlab(paste("Board Focus areas Associated with ",phrase))+
  #ylab(paste("Occurance of ",phrase))


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
   labs(title="Percent of Board Focus by Area",subtitle = paste(" For "," Ontario Hospitals."))+
   theme(axis.text.x = element_text(angle =45, hjust = 1,size=8))+
   ggplot2::annotate("rect",xmin=XMIN,ymin=YMIN,xmax=XMAX,ymax=YMAX,fill="gray",color="red",alpha=.75,size=.25)+
   #ggplot2::annotate("rect",xmin=XMIN+1,ymin=YMIN+.29,xmax=XMAX-1,ymax=YMAX,fill="gray",color="blue",alpha=.75,size=.25)+
   ggplot2::annotate(geom = "text", x =X+3, y =Y-YADJST, label = AreaText,hjust = "centre",size=2.5,color="blue")+
   ggplot2::annotate(geom = "text", x =X+3, y =Y-2.4*YADJST, label = GroupText,hjust = "center",size=2.5,color="gray10")
 #ggplot2::annotate(geom = "text", x =X+3, y =Y-3*YADJST, label = HospText,hjust = "center",size=2.5,color="red")
 FileName <- paste(SavedGraphics, "/qip Focus/","Ontario Overall", "Riverbed.png", sep = "")
 
 ggsave(FileName, device = "png", dpi = "print", width = 6, height = 6)
 
 

