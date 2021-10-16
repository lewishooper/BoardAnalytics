# Review of three methodologies

# rnew
#text2v3
#shingled ngram

library(tidyverse)
library(tm)
library(lubridate)
library(quanteda)
library(textstem)
library(RNewsflow)
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

ResultsFiles<-"~/BoardAnalytics/TheGreatOverhaul/Results"
SourceData<-"~/BoardAnalytics/TheGreatOverhaul/SourceData"
DBFiles<-readRDS("~/BoardAnalytics/TheGreatOverhaul/Results/FullFiles.rds")
OtherData<-"~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData"
Graphics<-"~/BoardAnalytics/TheGreatOverhaul/Graphics/Strategy/Comparisons"

DBFileList<-readRDS(file.path(ResultsFiles,"FullFiles.rds"))


NameAndtype<-readRDS(file.path(OtherData,"NameAndType.rds"))

BlockFiles<-readRDS(file.path(ResultsFiles,"BlockFocusAndSentiment.rds"))

## Remove MSH and RMH from the files
## Clean the files and Link to Namesand type
BlockFiles<-BlockFiles %>%
  filter(Organization!="RMH") %>%
  filter(Organization!="MSH") %>%
  CleanDFText("BlockText") %>%
  left_join(NameAndtype,by="Organization")
DBFileList<-DBFileList %>%
  filter(Organization!="RMH") %>%
  filter(Organization!="MSH") %>%
  CleanDFText("text") %>%
  left_join(NameAndtype,by="Organization")

HospStrategy<-readRDS(file.path(ResultsFiles,"HospStrategyClean.rds")) %>%
  filter(BoardName!="RMH") %>%
  filter(BoardName!="MSH") %>%
  CleanDFText("text") %>%
  rename(Organization=BoardName)%>%
  left_join(NameAndtype,by="Organization")


  

## Create Blank score dataframe
ScoreDF<-BlockFiles[0,] %>%
  select(Organization,FileName,Heading,SectionIDNumber,Date) %>%
  mutate(from_sum=0) %>%
  mutate(from_n=0) %>%
  mutate(from_nz=0)
## BASED ON rnews METHODOLOGY

#step 1
# Comare Minutes across all organizations
AllHospBlockDTM<-DBFileList %>%
  select(Organization,DATE,FileName,text)%>%
  #filter(SectionIDNumber!=1) %>%
  #filter(year(Date)>=2017) %>%
  mutate(DocID=paste0(Organization,FileName,DATE,sep="")) 
AllHospBlockDTM<-corpus(AllHospBlockDTM,docid_field="DocID",text_field="text")
AllHospBlockDTM<-quanteda::tokens(AllHospBlockDTM,what="word")
AllHospBlockDTM<-quanteda::tokens_ngrams(AllHospBlockDTM,n=2)
AllHospBlockDTM<-dfm(AllHospBlockDTM)
ALLMINUTES<-compare_documents(AllHospBlockDTM,AllHospBlockDTM,copy_meta=TRUE,measure="cosine")
DBFILEDOCID<-DBFileList %>%
  select(Organization,DATE,FileName) %>%
  mutate(DBDocID=paste0(Organization,FileName,DATE,sep=""))

TESTFILE<-ALLMINUTES$d%>%
  left_join(DBFILEDOCID,by=c("from"="DBDocID")) %>%
  select(-c(DATE,FileName)) %>%
  rename(FromOrg=Organization)%>%
  left_join(DBFILEDOCID,by=c("to"="DBDocID")) %>%
  select(-c(DATE,FileName)) %>%
  rename(ComparisonOrg=Organization) %>%
  filter(FromOrg!=ComparisonOrg) %>%
  filter(weight<=.9) %>%
  select(FromOrg,ComparisonOrg,weight) %>%
  pivot_wider(names_from = "ComparisonOrg",values_from = "weight",values_fn = mean)
  
# Last Compare Org Strategy with Orga Minutes
for(i in 1:nrow(HospStrategy)){  
  # create DTM for EACH Hospital Strategy
  OrgStratDTM <-corpus(HospStrategy[i,],docid_field = "Organization",text_field = "text") # %>%
  OrgStratDTM<-quanteda::tokens(OrgStratDTM,what="word")
  OrgStratDTM<-quanteda::tokens_ngrams(OrgStratDTM,n=2)
  OrgStratDTM<-dfm(OrgStratDTM)
  OrgBlocksDTM<-BlockFiles[BlockFiles$Organization==pull(HospStrategy[i,1]),] %>%
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
