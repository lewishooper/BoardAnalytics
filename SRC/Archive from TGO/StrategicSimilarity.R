## Strategic Cross walk
## e.g. similairty btwn Hosptial Minutes and Strategy
## 
#https://www.r-bloggers.com/2019/06/using-cosine-similarity-to-find-matching-documents-a-tutorial-using-senecas-letters-to-his-friend-lucilius/

# iMPLEMENT tEXT2VEC ON hoSPITAL dATABASES
# and Word2Vec
rm(list=ls())
library(SnowballC)
library(stopwords)
library(text2vec)
#library(data.table)
#library(word2vec)
library(tidyverse)
library(tidytext)
library(tm)
library(reshape2)
library(knitr)
library(kableExtra)

# functions
DF<-TFIDFStrategyAllHeadings
v1<-"cosine"
v2<-"l2"
CreateSimilarityDF<-function(DF,BoardName,word,tf,v1,v2) { #DF is CLEAN Bind TD-if (6 columns) BoardName,n,tf,idf,tf-idf
  ### using text2vec as package source
  temp<-cast_sparse(DF,BoardName,word)
  temp<-sim2(temp,method=v1,norm=v2)
  ## ocnvert to dataframe
  MeetingSimDF<-as.data.frame(as.matrix(temp)) 
  ## convert to long datafame
  temp<-setNames(melt(as.matrix(temp)), c('rows', 'vars', 'values'))
  return(temp)
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
## Ignore MVV 
## focus on Strategy and Hospitalcombined
#Limit Hospital Combined to last 3 years
#Mege Hospital Combined and Strategy so that they are in the same matrix
# then we can use psim to compare 

# Define data source  and file locations
#SourceData<-"~/BoardAnalytics/ProjectStructure/Data/OtherData/SummaryStrategyandMVV.xlsx"
StrategicData<-"~/BoardAnalytics/ProjectStructure/Data/OtherData/SummaryStrategyandMVV.xlsx"
graphics<-"~/BoardAnalytics/TheGreatOverhaul/Graphics/Strategy"
Results<-"~/BoardAnalytics/ProjectStructure/Output/Results"

## read in Data including list of proper names to remove from Board Minuts

CombinedData<-readRDS("~/BoardAnalytics/ProjectStructure/Data/OtherData/CombinedData.rds") 
ProperNamesDF<-readRDS("~/BoardAnalytics/ProjectStructure/Data/OtherData/ProperNamesDF.rds")
Strategy<-readxl::read_xlsx(StrategicData,sheet=3)
DBFileList<-readRDS("~/BoardAnalytics/ProjectStructure/Data/DBFileList.rds")

NameCrossWalk<-readxl::read_xlsx("~/BoardAnalytics/ProjectStructure/Data/OtherData/NameCrossWalk.xlsx",sheet=1) %>%
  # drop these two as there is no matching strategic data
  filter(BoardName!="ChathamKentOld-ON")%>%
  filter(BoardName!="Arnprior -ON")


# Two name errors need to be corrected so we can link to other data
CombinedData<-CombinedData %>%
  ## need to correct some names for consistency
  mutate(Organization=ifelse(Organization=="Kemptville -ON","KDH-ON",Organization)) %>%
  mutate(Organization=ifelse(Organization=="Dryden -ON","Dryden-ON",Organization))

BoardNamesAll<-DBFileList%>%
  ungroup()%>%
  select(Organization) %>%
  unique()

## Create one text file of all minutes by Hospital
## this is used for similarity analysis

HospCombined<-DBFileList %>%
  left_join(CombinedData,by=c("Organization"="Organization")) %>%
  select(Organization,FileName,DATE,"Facility ID",FacilityType,text) %>%
  ungroup() %>%
  filter(Organization!="ChathamKentOld-ON") %>%
  ## Drop MSH and RMH in final runs since there data was submitted and is not in the public domain
  #filter(Organization!="MSH -ON") %>%
  #filter(Organization!="RMH -ON") %>%
  # Limit the data to> 201516 fsscal since its closer to stratetic planning time frame and matches MOHLTTC PandP data
  filter(DATE>="2015-06-01") %>%
  rename(FAC="Facility ID") %>%
  rename(BoardName=Organization)%>%
  select(-c(FileName,DATE)) %>%
  group_by(BoardName) %>%
  # Collapse all the data by hospital into one row
  mutate(text = paste(text, collapse = " , ")) %>%
  unique() %>%
  CleanDFText("text") %>%
  tibble()

## Below used to create table of participants for paper
NamesAndType<-HospCombined %>%
  select(BoardName,FacilityType) %>% 
  unique() %>%
  left_join(NameCrossWalk,by="BoardName") %>%
  select(BoardName,FacilityType,FullName) %>%
  arrange(FacilityType) 

## Add facility type and FAC to Strategy 
Strategy<-left_join(Strategy,NameCrossWalk,by=c("Organization"="StratName"))
# update names and join for identifies and key data mainly Facility type
Strategy<-Strategy %>%
  rename(FAC="Facility ID") %>%
  select(Organization,BoardName,FAC,"Main Heading","Sub Heading")
Strategy<-left_join(Strategy,CombinedData,by=c("FAC"="FAC"))

## split into categories Main Heading Sub Heading for text analysis of headings
MainHeaders<-Strategy %>%
  select(BoardName,"Main Heading") %>%
  group_by(BoardName) %>%
  rename(MainHeading="Main Heading")%>%
  unique() %>%  ## Headings were duplicated in some of the sourcematerial
    mutate(MainHeading=str_remove(MainHeading,"SE ")) ## take3s out code referenceing enablers to the strategy susually SE= Strategic Enablers (i.e. Technology)
  
saveRDS(MainHeaders,file.path(Results,"StrategicMainHeadings.rds")) 

TFIDFMainHeaders<-MainHeaders %>%
  mutate(MainHeading=tolower(MainHeading)) %>%
  unnest_tokens(word,MainHeading,token="words") %>%
  arrange(word) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  anti_join(stop_words,by="word")%>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)
  
  
StrategyWithSubHeaders<-Strategy %>%
  select(BoardName,"Main Heading","Sub Heading") %>%
  rename(MainHeading="Main Heading")%>%
  rename(SubHeading="Sub Heading") %>%
  # Convert subheadings from rows to list under each main heading
  group_by(BoardName,MainHeading) %>%
  mutate(all_Data = paste(SubHeading, collapse = " , ")) %>%
  select(-SubHeading) %>%
  unique() %>%  ## Headings were duplicated in some of the sourcematerial
  mutate(MainHeading=str_remove(MainHeading,"SE "))%>% ## take3s out code referenceing enablers to the strategy susually SE= Strategic Enablers (i.e. Technology)
  mutate(SubHeading=str_remove(all_Data,regex("^NA"))) %>%
  select(-all_Data)
saveRDS(StrategyWithSubHeaders,file.path(Results,"StrategicHeadings.rds")) 


TFIDFStrategyAllHeadings<-StrategyWithSubHeaders %>%
  mutate(BothHeadings=paste(MainHeading,SubHeading,sep=" ")) %>%
  ungroup() %>%
  filter(BoardName!="LACGH -ON") %>%
  select(c(BoardName,BothHeadings)) %>%
  mutate(BothHeadings=tolower(BothHeadings)) %>%
  CleanDFText("BothHeadings") %>%
  unnest_tokens(word,BothHeadings,token="words") %>%
  arrange(word) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  anti_join(stop_words,by="word")%>%
  mutate(word = wordStem(word, language = "en")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)

JusHeadingsSimilarityIDF<-CreateSimilarityDF(TFIDFStrategyAllHeadings,BoardName,word,tf,"cosine","l2") %>%
  filter(rows!=vars) %>%
  filter(values> 0) %>%
  rename(Hospital=rows,ColHospital=vars) %>%
  mutate(Hospital=str_remove(Hospital,"-ON")) %>%
  mutate(Hospital=str_remove(Hospital," ")) %>%
  group_by(Hospital) %>%
  summarize(MeanSimilarity=mean(values))
ggplot(JusHeadingsSimilarityIDF,aes(x=MeanSimilarity,y=reorder(Hospital,MeanSimilarity)))+geom_point()

#CONCATENTATE STRATEGY
## convert multi line strategiest into a single text document
StrategyOrganized<-Strategy %>%
  rename(Organization=Organization.x) %>%
  select(c(Organization,BoardName,"FAC","Main Heading","Sub Heading","FacilityType")) %>%
  rename(SubHeading="Sub Heading") %>%
  rename(MainHeading="Main Heading") %>%
  select(BoardName,FacilityType,MainHeading,SubHeading)%>%
  group_by(BoardName,MainHeading) %>%
  mutate(all_Data = paste(SubHeading, collapse = " , ")) %>%
  select(BoardName,FacilityType,MainHeading,all_Data) %>%
  unique() %>%
  mutate(text=paste(MainHeading,all_Data,collapse=" , ")) %>%
  group_by(BoardName) %>%
  mutate(text = paste(text, collapse = " , ")) %>%
  select(BoardName,FacilityType,text) %>%
  unique() %>%
  mutate(BoardName=str_sub(BoardName,1,-4)) %>%
  mutate(BoardName=str_remove(BoardName," ")) %>%
  mutate(BoardName=paste(BoardName,"-Strategy",collapse="",sep=""))
  # drop in Final runs since MSH and RMH submitted their data in private
  #filter(BoardName!="RMH-Strategy") %>%
  #filter(BoardName!="MSH-Strategy")
StrategyWordCount<-StrategyOrganized %>%
  mutate(Textlength=str_count(text, pattern =" ")) %>%
  ungroup()

  
# combine Strategy and Hospital Data into single dataframe
FullMonty<-HospCombined %>%
  select(BoardName,FacilityType,text) %>%
  bind_rows(StrategyOrganized) %>%
  arrange(BoardName) %>%
  CleanDFText("text") %>%
  mutate(text=gsub(pattern="_",replace=" ",text)) %>%
  mutate(text=stripWhitespace(text))

TokenizedHospitalsAndStrategy<-FullMonty %>%
  unnest_tokens(word,text,token="ngrams",n=2) %>%
  arrange(word) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  anti_join(stop_words,by="word")%>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)


 

SearchforNames<-TokenizedHospitalsAndStrategy %>%
  filter(idf>=4.08 & n<=2) %>%
  filter(word!="closedb") %>%
  filter(word!="closedsk") %>%
  filter(word!="interconnect") %>%
  filter(word!="reqd") %>%
  filter(word!="stole") %>%
  filter(word!="qps")

TokenizedHospitalsAndStrategy <-TokenizedHospitalsAndStrategy %>%
  anti_join(SearchforNames,by=c("word"="word")) %>%
  mutate(word = wordStem(word, language = "en")) %>%
  select(BoardName,word,n) %>%
  ungroup()%>%
  group_by(BoardName,word) %>% 
  summarise(n = sum(n)) %>%
  bind_tf_idf(word,BoardName,n)
HospAndStrategy<-CreateSimilarityDF(TokenizedHospitalsAndStrategy,BoardName,word,tf,"cosine","l2")

### Limit DB to Hospital and stratgey 32 lines of data (LACGH no Strategy) %>%
  HospSimilarityToStrategy <-HospAndStrategy %>%
    rename(Hospital=rows,Strategy=vars) %>%
    mutate(Hospital=str_remove(Hospital,"-ON")) %>%
    mutate(Hospital=str_remove(Hospital," ")) %>%
    mutate(StrategyTF=str_detect(Strategy,"-ON")) %>% # detect hospitals 
    filter(StrategyTF==FALSE) %>% #Remove just hospitals
   mutate(StrategyName=str_remove(Strategy,"-Strategy")) %>%
    mutate(StrategyTF=Hospital==StrategyName) %>%
    filter(StrategyTF==TRUE)
MedianIndex<-median(HospSimilarityToStrategy$values)


ggplot(HospSimilarityToStrategy,aes(x=Hospital,y=values))+geom_point()
ggplot(HospSimilarityToStrategy,aes(x=reorder(Hospital,values),values/MedianIndex))+geom_point()+coord_flip()+
  labs(title="Similarity between Hospital Minutes and Strategy",subtitle = "using Cosine Similarity ")+
  xlab("Hospital")+
  ylab("Strategic Index")
ggsave(file.path(graphics,"HospSimtoStrategytext2vec.png"),device="png",dpi=600,units="in",width=5,height=7)
### Words from Strategy in Meetings
StrategyList<-StrategyOrganized 
StrategyList<-CleanDFText(StrategyList,"text")
TokenziedStrategy<-StrategyList %>%
  unnest_tokens(word,text,token="words") %>%
  filter(word!="sunnybrook") %>%
  filter(BoardName!="MSH-Strategy") %>%
  filter(BoardName!="RMH-Strategy") %>%
  filter(word!="kdh") %>%
  filter(word!="na")%>%
  arrange(word) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  #mutate(word = wordStem(word, language = "en")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n) %>%
  mutate(RootName=str_remove(BoardName,"-Strategy"))
TopFiveStrategyWords<-TokenziedStrategy %>%
  arrange(tf) %>%
  group_by(RootName) %>%
  top_n(n=1,wt=tf)


  warnings()
StrategySimilarity<-CreateSimilarityDF(TokenziedStrategy,BoardName,word,tf_idf,"cosine","l2") %>%
    filter(values>0) %>%
    filter(rows!=vars) %>%
    group_by(rows) %>%
    summarize(AvgSimilarity=mean(values),SD=sd(values),Min=min(values),Max=max(values)) %>%
    left_join(StrategyOrganized,by=c("rows"="BoardName")) %>%
  select(-text)
  
  
ggplot(StrategySimilarity,aes(y=AvgSimilarity,x=reorder(rows,AvgSimilarity),color=FacilityType)) +geom_point() +coord_flip()+
  geom_errorbar(aes(x=rows,y=AvgSimilarity,ymin = AvgSimilarity-SD, ymax = AvgSimilarity+SD), width = 0.2)+
  labs(title="Mean similarity with other hospitals in the Database",subtitle = "Error bars represent plus or minus one standard deviation")+
  xlab("Mean of the similarity score with all other hositals")+
  ylab("Hospital in Database")
ggplot(StrategySimilarity,aes(y=AvgSimilarity,x=reorder(FacilityType,AvgSimilarity),color=FacilityType)) +geom_boxplot()
TokenizedHospital<-DBFileList %>%
  left_join(CombinedData,by=c("Organization"="Organization")) %>%
  select(Organization,FileName,DATE,"Facility ID",FacilityType,text) %>%
  ungroup() %>%
  filter(Organization!="ChathamKentOld-ON") %>%
 # filter(Organization!="MSH -ON") %>%
  #filter(Organization!="RMH -ON") %>%
  filter(DATE>="2015-06-01") %>%
  rename(FAC="Facility ID") %>%
  rename(BoardName=Organization)%>%
 
  select(-c(FileName,DATE)) %>%
  group_by(BoardName) %>%
  mutate(text = paste(text, collapse = " , ")) %>%
  unique() %>%
  CleanDFText("text") %>%
  tibble() %>%
  select(BoardName,text) %>%
  unnest_tokens(word,text,token="words") %>%
  #mutate(word = wordStem(word, language = "en")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n) %>%
  mutate(RootName=str_remove(BoardName,"-ON")) %>%
  mutate(RootName=str_remove(RootName," "))
  
MergedHospStrategy<-left_join(TokenziedStrategy,TokenizedHospital,by=c("word"="word" , "RootName"="RootName")) %>%
  rename(BoardName_Strategy=BoardName.x,Num_Strategy=n.x,TermFreq_Strategy=tf.x,InverseDF_Strategy=idf.x,tf_idf_Strategy="tf_idf.x") %>%
  rename(BoardName_Minutes=BoardName.y,Num_Minutes=n.y,TermFreq_Minutes=tf.y,InverseDF_Minutes=idf.y,tf_idf_Minutes="tf_idf.y")

StrategicImpact<-MergedHospStrategy %>%
  mutate(anb=abs(Num_Strategy-Num_Minutes)) %>%
  mutate(aub=Num_Strategy+Num_Minutes)  %>%
  mutate(Impac=anb/(aub)) %>%
  #mutate(Impac=abs(TermFreq_Strategy-TermFreq_Minutes))%>%
  select(BoardName_Strategy,Impac) %>%
  group_by(BoardName_Strategy) %>%
  summarise(Mean=mean(Impac,na.rm=TRUE)) %>%
  left_join(StrategyWords,by=c("BoardName_Strategy"="BoardName"))
ggplot(StrategicImpact,aes(x=reorder(BoardName_Strategy,Mean),y=Mean))+geom_point()+coord_flip()
ggplot(StrategicImpact,aes(x=Mean,y=Textlength))+geom_smooth(method="lm")+
  geom_point(data=StrategicImpact,aes(x=Mean,y=Textlength))

##### further Cleanup required and we will use word2vec 
library(udpipe)
library(word2vec)
data(brussels_reviews,package ="udpipe")
data2<-StrategyOrganized %>%
  select(BoardName,FacilityType,text) %>%
  tibble() %>%
  mutate(text=tolower(text)) %>%
  CleanDFText("text")
#x <-subset(brussels_reviews, language=="fr")
#x <-tolower(x$feedback
data2$text<-tolower(data2$text)
set.seed(123456789)
model <-word2vec(x =data2$text,type ="skip",dim =50,iter =20)
write.word2vec(model,"strategy.bin")
embeddng<-as.matrix(model)
embedding <-predict(model,c("community","digital"),type ="embedding")
lookslike <-predict(model,c("care","partners"),type ="nearest",top_n =5)
lookslike
HospModel<-DBFileList %>%
  ungroup() %>%
  select(Organization,DATE,text) %>%
  tibble() %>%
  CleanDFText("text")
HospModel<-word2vec(x=HospCombined$text,type="skip",dim=50,iter=40)
write.word2vec(HospModel,"Hospmodel.bin")
WordMatch<-c("care","patient","community")
HospSimilarities <-predict(HospModel,WordMatch,type ="nearest",top_n =5)
StrategicSimilarities <-predict(model,WordMatch,type ="nearest",top_n =5)
test<-word2vec_similarity(as.matrix(model), as.matrix(HospModel),top_n = 4)
HospSimilarities <-HospSimilarities %>%
  bind_rows() %>%
  mutate(Type="minutes")
StrategicSimilarities<-StrategicSimilarities %>%
  bind_rows() %>%
  mutate(Type="strategic")
tset<-bind_rows(StrategicSimilarities,HospSimilarities) %>%
  arrange(term2)

