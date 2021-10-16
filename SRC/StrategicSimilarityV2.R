#similiarity between strategy and minutes
rm(list=ls())
library(RNewsflow)
library(word2vec)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(tidytext)
library(quanteda)
library(text2vec)
library(SnowballC)
library(doc2vec)
library(textstem)
#vignette('RNewsflow')

CleanDFText<-function(DF,TextColumn){
  
  #SentimentMotions<-SentimentMotions[,c(1,2,6)]
  DF[[TextColumn]]<-gsub(pattern="\\W", replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="\\d",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="_{2,}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- tolower(DF[[TextColumn]])
  DF[[TextColumn]]<-removeWords(DF[[TextColumn]],stopwords("english"))
  DF[[TextColumn]]<-removeWords(DF[[TextColumn]],c("NA","na"))
  DF[[TextColumn]]<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<-stripWhitespace(DF[[TextColumn]])
  DF[[TextColumn]]<-lemmatize_strings(DF[[TextColumn]])
  #SentimentMotions$Motion<-str_split(SentimentMotions$Motion,pattern ="\\s+")
  return(DF)
}
Graphics<-"~/BoardAnalytics/TheGreatOverhaul/Graphics/Strategy"

DBSource<-("/RawProvinicalResidenceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
OtherData<-c("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData")

NameAndtype<-readRDS(file.path(OtherData,"NameAndType.rds"))

Blockfiles<-readRDS(file.path(RDSfiles,"BlockFocusAndSentiment.rds"))
DBFileList<-readRDS(file.path(RDSfiles,"FullFiles.rds"))
## Remove MSH and RMH from the files
Blockfiles<-Blockfiles %>%
  filter(Organization!="RMH") %>%
  filter(Organization!="MSH")
DBFileList<-DBFileList %>%
  filter(Organization!="RMH") %>%
  filter(Organization!="MSH")
HospStrategy<-readRDS(file.path(RDSfiles,"HospStrategyClean.rds")) %>%
  filter(BoardName!="RMH") %>%
  filter(BoardName!="MSH")

### ERASE ME
#NameAndtype[32,"FacilityType"]<-"Small Hospital"
#saveRDS(NameAndtype,file.path(OtherData,"NameAndType.rds"))
## Update name and type in src file UpdateNameanType in tgo archive
## Create Blank score dataframe
ScoreDF<-Blockfiles[0,] %>%
  select(Organization,FileName,Heading,SectionIDNumber,Date) %>%
  mutate(from_sum=0) %>%
  mutate(from_n=0) %>%
  mutate(from_nz=0)
## BASED ON rnews METHODOLOGY
i
for(i in 1:nrow(HospStrategy)){  
  # create DTM for EACH Hospital Strategy
  OrgStratDTM <-corpus(HospStrategy[i,],docid_field = "BoardName",text_field = "text") # %>%
  OrgStratDTM<-quanteda::tokens(OrgStratDTM,what="word")
   OrgStratDTM<-quanteda::tokens_ngrams(OrgStratDTM,n=2)
  OrgStratDTM<-dfm(OrgStratDTM)
  OrgBlocksDTM<-Blockfiles[Blockfiles$Organization==pull(HospStrategy[i,1]),] %>%
    select(Organization,Date,FileName,Heading,SectionIDNumber,BlockText)%>%
    filter(SectionIDNumber!=1) %>%
    filter(year(Date)>=2017) %>%
    mutate(DocID=paste0(Heading,SectionIDNumber,str_sub(BlockText,1,6),Organization,FileName,sep="")) 
    
  
  OrgBlocksDTM<-corpus(OrgBlocksDTM,docid_field="DocID",text_field="BlockText") # %>%
    OrgBlocksDTM<-quanteda::tokens(OrgBlocksDTM,what="word")
   OrgBlocksDTM<-quanteda::tokens_ngrams(OrgBlocksDTM,n=2)
  OrgBlocksDTM<-dfm(OrgBlocksDTM)
  Scores<-compare_documents(OrgBlocksDTM,OrgStratDTM,measure="cosine")
Scores<-Scores$from_meta 
  #select(-c(document_id))
ScoreDF<-bind_rows(ScoreDF,Scores)

}
ScoreDF<-ScoreDF %>%
  left_join(NameAndtype,by="Organization")
warnings()
ScoreDFSum<-ScoreDF %>%
  group_by(FileName) %>%
  mutate(EachMeeting=sum(from_sum)) %>%
  select(Organization,FileName,EachMeeting,FacilityType) %>%
  unique() %>%
  group_by(Organization) %>%
  mutate(OrgAverage=sum(EachMeeting)/n()) %>%
  select(Organization,OrgAverage,FacilityType) %>%
  unique() %>%
    ungroup() %>%
    mutate(OntarioAvg=mean(OrgAverage)) %>%
  mutate(Indexed=OrgAverage/OntarioAvg)

AvgSizeStrategy<-HospStrategy %>%
  CleanDFText("text") %>%
  unnest_tokens(word,text,token="words") %>%
  group_by(BoardName) %>%
  mutate(tokenNum=n()) %>%
  select(BoardName,tokenNum) %>%
  unique()
AvgSizeStrategy<-AvgSizeStrategy%>%
  left_join(ScoreDFSum,by=c("BoardName"="Organization"))
ggplot(AvgSizeStrategy,aes(x=tokenNum,y=Indexed))+geom_smooth(method="lm")
FIT<-lm(AvgSizeStrategy$Indexed ~ AvgSizeStrategy$tokenNum)

summary(FIT)
### Strategic Similarity method 1 Using Rnews scores
QuantSSM1<-quantile(ScoreDFSum$Indexed)
TargetHospital<-""
TargetPoint<-ScoreDFSum[ScoreDFSum$Organization==TargetHospital,]
ggplot(ScoreDFSum,aes(y=reorder(Organization,OrgAverage),x=Indexed))+geom_point()+
  labs(title = "Linkage between Minutes and Strategic Plan",subtitle = "score indexed to Ontario Average" ,caption="Minutes from 2019\nMethods tm Quanteda, rnewsflow")+
  xlab("Indexed on Ontario Average")+
  ylab("Hospital Strategic Similarity")+
  geom_rect(xmin =QuantSSM1[[2]], xmax =QuantSSM1[[4]],
            ymin =-Inf, ymax =+Inf, fill = 'blue',alpha=.01) +
  annotate("text",x=1,y=29,label="Normal Range\n25th to 75th \nPercentile",color="brown")+
  
  xlab("Hospital")+
  geom_point(data=TargetPoint, colour="red",size=2) +  # this adds a red point
  geom_text(data=TargetPoint, label=TargetHospital, vjust=0,hjust=1,color="red") # this adds a label for the red point
 # theme(axis.text.y=element_blank())

ggsave(file.path(Graphics,"2017RnewsBasedSimilarity.png"),device="png",dpi=600,units="in",width=6,height=6)

ggplot(ScoreDF[year(ScoreDF$Date)>="2017" & ScoreDF$Organization=="StThomasElgin",],aes(x=from_sum,y=Date))+geom_point()
 

OverlapTypesByHosp<-Joined %>%
  select(Organization,fourgrams) %>%
  unique()%>%
  group_by(Organization)%>%
  mutate(CountOverlaps=n())  %>%
  select(Organization,CountOverlaps) %>%
  unique()
# Deep Dive
      
      
      
      
      TEST2vec<-DBFileList %>%
  filter(Organization=="Barrie") %>%
  CleanDFText("text") %>% 
  select(FileName,Organization,text,DATE)
Strat2Vec<-HospStrategy %>%
  #filter(BoardName=="Barrie-Strategy") %>%
  CleanDFText("text") %>%
  rename(Organization=BoardName)
StratModel<-word2vec(x =Strat2Vec$text,type ="skip",dim =50,iter =20)
Hospmodel <-word2vec(x =TEST2vec$text,type ="skip",dim =50,iter =20)  
embedding <-predict(Hospmodel,c("community","digital"),type ="embedding")
lookslike <-predict(Hospmodel,c("care","partner"),type ="nearest",top_n =5)

word2vec_similarity(as.matrix(Hospmodel), as.matrix(StratModel), top_n = +Inf, type = c("cosine"))
paragraph2vec_similarity(as.matrix(Hospmodel), as.matrix(Hospmodel), top_n = 20)
HospParaVec<-TEST2vec %>%
  rename(doc_id=FileName) %>%
  select(doc_id,text) %>%
  paragraph2vec()
StrategyaParaVec<-Strat2Vec %>%
  rename(doc_id=Organization) %>%
  select(doc_id,text) %>%
  paragraph2vec()
paragraph2vec_similarity(as.matrix(Hospmodel), as.matrix(StratModel), top_n = 5)

  ### using sliding window ngap frquency
NGap<-2
TestHospitals<-DBFileList %>%
#filter(Organization=="Barrie"|Organization=="Dryden"|Organization=="TBay"|Organization=="Aliston") %>%
  CleanDFText("text") %>%
  select(Organization,FileName,DATE,text) %>%
  #unnest_tokens(ngram,text,token=ngrams,n=3)
  unnest_tokens(fourgrams, text, token = "ngrams", n = NGap)
TestStrategy<-HospStrategy %>% 
 # filter(BoardName=="Barrie"|BoardName=="Dryden"|BoardName=="TBay"|BoardName=="Aliston") %>%
  CleanDFText("text") %>%
  select(BoardName,text) %>%
  #unnest_tokens(ngram,text,token=ngrams,n=3)
  unnest_tokens(fourgrams, text, token = "ngrams", n = NGap)
Joined<-inner_join(TestHospitals,TestStrategy,by=c("fourgrams"="fourgrams","Organization"="BoardName"))
FreqJoined<-Joined%>%
  #filter(year(DATE>=2010))%>%
  select(Organization,fourgrams)%>%
  group_by(Organization,fourgrams) %>%
  mutate(FreqAtBoard=n())%>%
  unique() %>%
  group_by(Organization) %>%
  mutate(maxTerm=max(FreqAtBoard))
  
Hosp<-"HSCN"
DeepDive<-Joined %>%
  filter(Organization==Hosp)
#ggplot(Joined[Joined$Organization=="UHN",],aes(x=fourgrams))+geom_histogram(stat="count")+coord_flip()
OverviewHosp<-TestHospitals %>%
  select(Organization,FileName) %>%
  unique() %>%
  group_by(Organization) %>%
  mutate(MeetingCount=n()) %>%
  select(Organization,MeetingCount) %>%
  unique()
ByMinutes<-Joined %>% 
  left_join(OverviewHosp,by="Organization") %>%
  left_join(OverlapTypesByHosp,by="Organization") %>%
  mutate(StrategicOverlap=CountOverlaps/MeetingCount) %>%
  select(Organization,StrategicOverlap) %>%
  unique()
ggplot(ByMinutes,aes(x=reorder(Organization,StrategicOverlap),y=StrategicOverlap*100))+geom_col() +coord_flip()

### REVIEW of DATE RELATED mentions of STrategic Plan
DateReview<-full_join(DBFileList,Joined,by=c("Organization"="Organization","DATE"="DATE")) %>%
  select(Organization,DATE,fourgrams) %>%
  group_by(Organization,DATE) %>%
  mutate(CountbyDate=n()) %>%
  mutate(NACells=sum(is.na(fourgrams))) %>%
  mutate(CountbyDate=CountbyDate-NACells) %>%
  select(Organization,DATE,CountbyDate) %>%
  unique() 
 # filter(DATE>="2018-01-01")
ggplot(DateReview[DateReview$Organization=="HaliburtonHighlands",],aes(x=factor(DATE),y=CountbyDate))+geom_col()  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("DATE")

ggplot(DateReview[DateReview$DATE>="2018-01-01",],aes(x=DATE,y=CountbyDate))+geom_area() 


### Analyze Similarity between strategies as a stand alone
StrategyMatrix<-HospStrategy %>%
  group_by(BoardName) %>%
  unnest_tokens(word,text) %>%
 mutate(word=wordStem(word,language="en")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word,BoardName,n) %>%
  cast_sparse(BoardName,word,tf)

Similarities<-sim2(StrategyMatrix,method="cosine",norm="l2")
Similarities<-as.matrix(Similarities)
SIMDF<-data.frame(Similarities)
SIMDF$BoardName<-rownames(SIMDF)
SIMDF<-SIMDF %>%
    select(c(BoardName,1:35)) %>%
  pivot_longer(cols=c(2:35),names_to = "MatchHosp",values_to = "Similarity") %>%
  left_join(HospStrategy,by="BoardName") %>%
  select(c(BoardName,Type,MatchHosp,Similarity)) %>%
  filter(Similarity<0.99999999999)

SummaryScores<-SIMDF %>%
  group_by(BoardName) %>%
  mutate(count=n()) %>%
  mutate(SumByHosp=sum(Similarity)) %>%
  mutate(average=SumByHosp/count) %>%
  select(BoardName,average) %>%
  unique()

 ggplot(SummaryScores,aes(x=reorder(BoardName,average),y=average))+geom_point()+coord_flip()+
   labs(title="Similarity between Hospital Strategies",caption = "Text2Vec Modelling")+
   ylab("Hospital Average Similarity with all others")+
   xlab("Hospital\nMost unique <--- >> Most Similar")
 ggsave(file.path(Graphics,"StrategicSimilarity.png"),device="png",dpi=600,units="in",width=5,height=7)
 
 ### Analyze Similarity between MINUTES as a stand alone
 MinuteMatrix<-DBFileList %>%
   group_by(Organization) %>%
   unnest_tokens(word,text) %>%
   mutate(word=wordStem(word,language="en")) %>%
   count(Organization,word,sort=TRUE) %>%
   bind_tf_idf(word,Organization,n) %>%
   cast_sparse(Organization,word,tf)
 #MinuteMatrix[1:10,1:4]
 BoardSimilarities<-sim2(MinuteMatrix,method="cosine",norm="l2")
 BoardSimilarities<-as.matrix(BoardSimilarities)
 BoardSIMDF<-data.frame(BoardSimilarities)
 BoardSIMDF$Organization<-rownames(BoardSIMDF)
 BoardSIMDF<-BoardSIMDF %>%
   select(c(Organization,1:35)) %>%
   pivot_longer(cols=c(2:35),names_to = "MatchHosp",values_to = "Similarity") %>%
   left_join(HospStrategy,by=c("Organization"="BoardName")) %>%
   select(c(Organization,Type,MatchHosp,Similarity)) %>%
   filter(Similarity<0.99999999999)
 
 SummaryScoresBoard<-BoardSIMDF %>%
   group_by(Organization) %>%
   mutate(count=n()) %>%
   mutate(SumByHosp=sum(Similarity)) %>%
   mutate(average=SumByHosp/count) %>%
   select(Organization,average) %>%
   unique()
 
 ggplot(SummaryScoresBoard,aes(x=reorder(Organization,average),y=average))+geom_point()+coord_flip()+
   labs(title="Similarity between Hospital Minutes",caption = "Text2Vec Modelling")+
   ylab("Hospital Average Similarity with all others")+
   xlab("Hospital\nMost unique <--- >> Most Similar")
 ggsave(file.path(Graphics,"MinutesSimilarity.png"),device="png",dpi=600,units="in",width=5,height=7)
 
 
 ### template fake data
 g1 <- SummaryScoresBoard[2,]
 Hosp<-SummaryScoresBoard[2,1]
 
 Quants<-quantile(pull(SummaryScoresBoard[,"average"]))
 Quants[[2]]
p1<- ggplot(Sc,aes(y=reorder(Organization,average),x=average))+geom_point(size=.5)+
   labs(title="Similarity With Strategy",caption = "Text2Vec Modelling")+
   ylab("Hospital Strategic Similarity")+
   geom_rect(xmin =Quants[[2]], xmax =Quants[[4]],
             ymin =-Inf, ymax =+Inf, fill = 'blue',alpha=.01) +
  
   xlab("Hospital")+
  theme(axis.text.y=element_blank())+
   geom_point(data=g1, colour="red",size=2) +  # this adds a red point
   geom_text(data=g1, label=Hosp, vjust=0,hjust=1,color="red") # this adds a label for the red point
  
 g1 <- SummaryScoresBoard[3,]
 Hosp<-SummaryScoresBoard[3,1]
 
 Quants<-quantile(pull(SummaryScoresBoard[,"average"]))

 # Quants[[2]]
 p2<-ggplot(SummaryScoresBoard,aes(y=reorder(Organization,average),x=average))+geom_point(size=.5)+
   labs(title="Similarity With Strategy",caption = "Text2Vec Modelling")+
   ylab("Hospital Strategic Similarity")+
   geom_rect(xmin =Quants[[2]], xmax =Quants[[4]],
             ymin =-Inf, ymax =+Inf, fill = 'blue',alpha=.01) +
   
   xlab("Hospital")+
   theme(axis.text.y=element_blank())+
   geom_point(data=g1, colour="red",size=2) +  # this adds a red point
   geom_text(data=g1, label=Hosp, vjust=0,hjust=1,color="red") # this adds a label for the red point
 g1 <- SummaryScoresBoard[5,]
 Hosp<-SummaryScoresBoard[5,1]
 
 Quants<-quantile(pull(SummaryScoresBoard[,"average"]))
 Quants[[2]]
p3<- ggplot(SummaryScoresBoard,aes(y=reorder(Organization,average),x=average))+geom_point(size=.5)+
   labs(title="Similarity With Strategy",caption = "Text2Vec Modelling")+
   ylab("Hospital Strategic Similarity")+
   geom_rect(xmin =Quants[[2]], xmax =Quants[[4]],
             ymin =-Inf, ymax =+Inf, fill = 'blue',alpha=.01) +
   
   xlab("Hospital")+
  theme(axis.text.y=element_blank())+
   geom_point(data=g1, colour="red",size=2) +  # this adds a red point
   geom_text(data=g1, label=Hosp, vjust=0,hjust=1,color="red") # this adds a label for the red point
 
Dummy3<-ScoreDF %>%
  select(Organization,from_sum) %>%
  group_by(Organization) %>%
  mutate(summ=sum(from_sum)) %>%
  select(Organization,summ) %>%
  unique()
Dummy3[,"type"]<-'Teaching'
Dummy3[1:7,"type"]<-'Small'
Dummy3[8:22,"type"]<-'Large' 
p4<- ggplot(Dummy3,aes(x=summ,y=type,fill=type))+geom_boxplot()+coord_flip()
 grid.arrange(p1, p2,p3,p4, nrow = 2)
   
 ###  Similarity between Strategy and Minutes
 
## Using ngram 
   
   
 NGap<-2
 
 MinuteNgap<-DBFileList %>%
   #filter(Organization=="Barrie"|Organization=="Dryden"|Organization=="TBay"|Organization=="Aliston") %>%
   CleanDFText("text") %>%
   select(Organization,FileName,DATE,text) %>%
   #unnest_tokens(ngram,text,token=ngrams,n=3)
   unnest_tokens(TheWords, text, token = "ngrams", n = NGap)
 StrategyNgap<-HospStrategy %>% 
   # filter(BoardName=="Barrie"|BoardName=="Dryden"|BoardName=="TBay"|BoardName=="Aliston") %>%
   CleanDFText("text") %>%
   select(BoardName,text) %>%
   #unnest_tokens(ngram,text,token=ngrams,n=3)
   unnest_tokens(TheWords, text, token = "ngrams", n = NGap) 
   
   
   
   
   
   
   
   
     df <- data.frame(facet = rep(c("C","D"), each=6),
                    group = rep(c("A","B"), 6),
                    mean = runif(12))
   
   ggplot(df, aes(x = group, y = mean)) +
     
     theme_bw() +
     geom_rect(xmin = 0, xmax = 1.5, ymin = -0.5, ymax = 1.5,
               fill = 'green', alpha = 0.05) +
     geom_point(size = 2.5) +
     facet_wrap(~ facet)
   