#https://www.r-bloggers.com/2019/06/using-cosine-similarity-to-find-matching-documents-a-tutorial-using-senecas-letters-to-his-friend-lucilius/


# iMPLEMENT tEXT2VEC ON hoSPITAL dATABASES
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


CreateSimilarityDF<-function(DF,FileName,word,tf,v1,v2) { #DF is CLEAN Bind TD-if (6 columns) BoardName,n,tf,idf,tf-idf
  temp<-cast_sparse(DF,FileName,word,tf)
  temp<-sim2(temp,method=v1,norm=v2)
  
  MeetingSimDF<-as.data.frame(as.matrix(temp)) 
  
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
#Limit Hospital Combined
#Mege Hospital Combined and Strategy so that they are in the same matrix
# then we can use psim to compare 

ProperNamesDF<-readRDS("~/UbBig/TextAnalysis/OtherData/ProperNamesDF.rds")
SourceData<-"~/UbBig/TextAnalysis/OtherData/SummaryStrategyandMVV.xlsx"
Strategy<-readxl::read_xlsx(SourceData,sheet=3)
graphics<-"/home/skip20/UbBig/TextAnalysis/DocumentSimilarity/Graphics"
Results<-"/home/skip20/UbBig/TextAnalysis/DocumentSimilarity/Results"
CombinedData<-readRDS("~/UbBig/TextAnalysis/OtherData/CombinedData.rds") 
#AllLogicalSentences<-readRDS("/home/skip20/UbBig/Results/AllLogicalSentences.rds")
#BlockFiles<-readRDS("~/UbBig/TextAnalysis/blockanalysis/Results/CorrectedBlockFiles.rds")
CombinedData<-CombinedData %>%
  mutate(Organization=ifelse(Organization=="Kemptville -ON","KDH-ON",Organization)) %>%
  mutate(Organization=ifelse(Organization=="Dryden -ON","Dryden-ON",Organization))
DBFileList<-readRDS("~/UbBig/TextAnalysis/blockanalysis/Results/DBFileList.rds")
BoardNamesAll<-DBFileList%>%
  ungroup()%>%
  select(Organization)%>%
  unique()

NameCrossWalk<-readxl::read_xlsx("~/UbBig/TextAnalysis/OtherData/NameCrossWalk.xlsx",sheet=1) %>%
  filter(BoardName!="ChathamKentOld-ON")%>%
  filter(BoardName!="Arnprior -ON")
## Use Hosptials board meetings as a single file
## Trim data after we know it works

HospCombined<-DBFileList %>%
  left_join(CombinedData,by=c("Organization"="Organization")) %>%
  select(Organization,FileName,DATE,"Facility ID",FacilityType,text) %>%
  ungroup() %>%
  filter(Organization!="ChathamKentOld-ON") %>%
  rename(FAC="Facility ID") %>%
  rename(BoardName=Organization)%>%
  select(-c(FileName,DATE)) %>%
  group_by(BoardName) %>%
  mutate(text = paste(text, collapse = " , ")) %>%
  unique() %>%
  CleanDFText("text") %>%
  tibble()

#rm(DBFileList,CombinedData,BlockFiles)
Strategy<-left_join(Strategy,NameCrossWalk,by=c("Organization"="StratName"))
Strategy<-left_join(Strategy,CombinedData,by=c("Facility ID"="FAC"))
 

#CONCATENTATE STRATEGY
StrategyOrganized<-Strategy %>%
  rename(Organization="Organization.x")%>%
  select(c(Organization,BoardName,"Facility ID","Main Heading","Sub Heading","FacilityType")) %>%
  rename(SubHeading="Sub Heading") %>%
  rename(MainHeading="Main Heading") %>%
  select(BoardName,FacilityType,MainHeading,SubHeading)%>%
  group_by(BoardName,MainHeading) %>%
  mutate(all_Data = paste(SubHeading, collapse = " , ")) %>%
  mutate(text=paste(MainHeading,all_Data,collapse=" , ")) %>%
  group_by(BoardName)%>%
  mutate(text = paste(text, collapse = " , ")) %>%
  select(BoardName,FacilityType,text) %>%
  unique() %>%
  mutate(BoardName=str_sub(BoardName,1,-4)) %>%
  mutate(BoardName=str_remove(BoardName," ")) %>%
  mutate(BoardName=paste(BoardName,"-Strategy",collapse="",sep=""))

FullMonty<-HospCombined %>%
  select(BoardName,FacilityType,text) %>%
  bind_rows(StrategyOrganized) %>%
  arrange(BoardName) %>%
  CleanDFText("text") %>%
  mutate(text=gsub(pattern="_",replace=" ",text)) %>%
  mutate(text=stripWhitespace(text))

TokenizedHospitals<-FullMonty %>%
  unnest_tokens(word,text,token="words") %>%
  arrange(word) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)
  

SearchforNames<-TokenizedHospitals %>%
  filter(idf>=4.1 & n>=20) %>%
  filter(word!="closedb") %>%
  filter(word!="closedsk") %>%
  filter(word!="interconnect") %>%
  filter(word!="reqd") %>%
  filter(word!="stole") %>%
  filter(word!="qps")

TokenizedHospitals <-TokenizedHospitals %>%
  anti_join(SearchforNames,by=c("word"="word")) %>%
  mutate(word = wordStem(word, language = "en")) %>%
  select(BoardName,word,n) %>%
  ungroup()%>%
  group_by(BoardName,word) %>% 
  summarise(n = sum(n)) %>%
bind_tf_idf(word,BoardName,n)


#write.csv(SearchforNamesl,file.path("~/UbBig/TextAnalysis/OtherData","SearchforNamesl.csv"))

#getwd()
StratSparseMatrix<-StrategyOrganized %>%
  unnest_tokens(word,text) %>%
  mutate(word=wordStem(word,language="en")) %>%
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)
StratSparseMatrix<-StratSparseMatrix %>%
  cast_sparse(BoardName, word, tf)
sparse_matrix <- TokenizedHospitals %>%
  cast_sparse(BoardName, word, tf)


HospSparseMatrix<-TokenizedHospitals %>%
  cast_sparse(BoardName, word, tf)
dim(HospSparseMatrix)

HospSim2<-sim2(HospSparseMatrix,method="jaccard",norm="none")

HospSim2DF<-as.data.frame(as.matrix(HospSim2)) 

HospSim2DF<-setNames(melt(as.matrix(HospSim2DF)), c('rows', 'vars', 'values')) %>%
left_join(FullMonty,by=c("rows"="BoardName")) %>%
  select(rows,FacilityType,vars,values)
anti_join(BoardNamesAll,HospSim2DF,by=c("Organization"="rows"))

SimOrgStrategy<-HospSim2DF %>%
  mutate(HospRows=sub("-.*","",rows)) %>%
  mutate(HospRows=sub(" .*","",HospRows)) %>%
  mutate(HospVars=sub("-.*","",vars)) %>%
  mutate(Flag1=HospVars==HospRows) %>%
  filter(Flag1==TRUE) %>%
 filter(!str_detect(rows,"-Strategy")) %>%
  filter(!str_detect(vars,"-ON")) %>%
  mutate(midpoint=median(values))

anti_join(BoardNamesAll,SimOrgStrategy,by=c("Organization"="r"))

ggplot(SimOrgStrategy,aes(x=values/midpoint,y=reorder(rows,values),color=FacilityType))+geom_point()

### Look at averge similarity by Hospital ??


MeetingSimilarity<-HospCombined %>%
  select(-FAC) %>%
  mutate(text=gsub(pattern="_",replace=" ",text)) %>%
  mutate(text=stripWhitespace(text)) %>%
  unnest_tokens(word,text) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  anti_join(SearchforNames,by=c("word"="word")) %>%
  
  mutate(word = wordStem(word, language = "en")) %>%
  
  count(BoardName,word,sort=TRUE) %>%
  bind_tf_idf(word, BoardName, n)
Meeting_matrix <- MeetingSimilarity %>%
  cast_sparse(BoardName, word, tf)
dim(Meeting_matrix)
MeetingSim2<-sim2(Meeting_matrix,method="cosine",norm="l2")

MeetingSimDF<-as.data.frame(as.matrix(MeetingSim2)) 

Meeting2SimDF<-setNames(melt(as.matrix(MeetingSimDF)), c('rows', 'vars', 'values')) %>%
  left_join(FullMonty,by=c("rows"="BoardName")) %>%
  select(rows,FacilityType,vars,values) %>%
  filter(values!=1) %>%
  group_by(rows) %>%
  summarize(n=n(),mean=mean(values),SD=sd(values),min=min(values),max=max(values))
ggplot(Meeting2SimDF,aes(x=reorder(rows,mean),y=mean))+geom_point()+coord_flip()

#### Compare similarity by Hospital Type
FacilitybasedMeetingSimilarity<-setNames(melt(as.matrix(MeetingSimDF)), c('rows', 'vars', 'values')) %>%
  left_join(FullMonty,by=c("rows"="BoardName")) %>%
  select(-text) %>%
  rename(RowFacility=FacilityType) %>%
  left_join(FullMonty,by=c("vars"="BoardName")) %>%
  select(-text) %>%
  rename(ColFacility=FacilityType)%>%
  group_by(RowFacility,ColFacility) %>%
  summarize(n=n(),mean=mean(values),SD=sd(values),min=min(values),max=max(values))
### Transfered to a power point matrix
i<-2
### Internal Consistence  Hospital by Meeting
Orgs<-DBFileList%>%
  ungroup() %>%
  select(Organization) %>%unique()
for(i in 1:nrow(Orgs)){
  ThisOrg<-Orgs[i,1]
HospConsistency<-DBFileList[DBFileList$Organization==as.character(ThisOrg),] %>%
  select(Organization,FileName,text) %>%
 # filter(Organization=="Dryden -ON") %>%
  mutate(text=gsub(pattern="_",replace=" ",text)) %>%
  mutate(text=stripWhitespace(text)) %>%
  unnest_tokens(word,text) %>%
  anti_join(ProperNamesDF,by=c("word"="Name")) %>%
  anti_join(SearchforNames,by=c("word"="word")) %>%
  mutate(word = wordStem(word, language = "en")) %>%
  count(FileName,word,sort=TRUE) %>%
  bind_tf_idf(word, FileName, n) %>%
  CreateSimilarityDF(FileName,word,tf,"cosine","l2") %>%
  filter(values <0.999999999) %>%
  group_by(rows) %>%
  summarize(n=n(),mean=mean(values),SD=sd(values),min=min(values),max=max(values)) %>%
  left_join(DBFileList,by=c("rows"="FileName")) %>%
  select(c(rows:max,Organization,DATE))

 ggplot(HospConsistency,aes(x=DATE,y=mean))+geom_point()+
    labs(title=HospConsistency[1,"Organization"])
ggsave(file.path(graphics,paste0(ThisOrg,"->InternalMeetingConsistency.png",sep="")))
}


#### Strategic Similarity ####

