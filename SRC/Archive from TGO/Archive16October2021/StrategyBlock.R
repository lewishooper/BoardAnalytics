#Document Similarity Strategy and Minutes
library(readxl)
rm(list=ls())
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



DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/RawProvinicalResidenceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData")
LocationNames<-"~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/RawProvinicalResidenceData"
OntarioPlaces<-readRDS(file.path(RDSfiles,"OntarioLocations.rds")) %>%
  mutate(name=tolower(name))
# import and createBoardNames to Strategy
HospStrategyRaw<-read_xlsx("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/SummaryStrategyandMVV.xlsx",sheet = "SDConverted")
MVVRaw<-read_xlsx("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/SummaryStrategyandMVV.xlsx",sheet = "MVVConverted")
HospMinutes<-readRDS(file.path(RDSfiles,"BlockFocusAndSentiment.rds"))
CountHosp<-HospMinutes %>%
  select(Organization) %>%
  unique()
NamesCrossWalk<-read_xlsx("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/NameCrossWalk.xlsx") 
TypeLookup<-read_xlsx("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/lkupTypeDetails.xlsx")
HospStrategy<-HospStrategyRaw %>%
  left_join(NamesCrossWalk,by=c("Organization"="StratName")) %>%
  select(c(1:6)) %>%
  rename(FAC="Facility ID") %>%
  left_join(TypeLookup,by="FAC") %>%
  select(c(1:6,9)) %>%
  rename(BoardName="BoardName.x") %>%
  mutate(BoardName=str_remove(BoardName," -ON"))
MVVRAWNames<-MVVRaw %>%
  select(ORG) %>%
  distinct_all()
MVVHosp<-MVVRaw %>%
  rename(Organization=ORG) %>%
  left_join(NamesCrossWalk,by=c("Organization"="MVV Name")) %>%
  select(c(1:6)) %>%
  rename(FAC="Facility ID") %>%
  left_join(TypeLookup,by="FAC") %>%
  select(c(1:6,9)) %>%
  rename(BoardName="BoardName.x")
MVVNames<-MVVHosp %>%
  select(BoardName) %>%
  distinct_all()

HospStrategyCollapsed<-HospStrategy %>%
  rename(MainHeading="Main Heading") %>%
  rename(SubHeading="Sub Heading") %>%
  mutate(FullText=paste(MainHeading,SubHeading,sep=" ")) %>%
  select(BoardName,Type,FullText) %>%
  group_by(BoardName) %>%
  summarise(text = toString(FullText),Type) %>%
  distinct_all()
MVVCollapsed<-MVVHosp %>%
  select(BoardName,Type,Text)%>%
  group_by(BoardName) %>%
  summarize(text=toString(Text),Type) %>%
  distinct_all()
MVVClean<-CleanDFText(MVVCollapsed,"text")
saveRDS(MVVClean,file.path(RDSfiles,"MVVClean.rds"))
CountStrategy<-HospStrategyCollapsed %>%
  select(BoardName) %>%
  unique()
HospStrategyClean<-CleanDFText(HospStrategyCollapsed,"text")
saveRDS(HospStrategyClean,file.path(RDSfiles,"HospStrategyClean.rds"))

HospStrategyWords<-HospStrategyClean %>%
  ungroup() %>%
  unnest_tokens(word,text,c("words")) %>%
  select(word) %>%
  group_by(word) %>%
  filter(word!="se") %>%
  mutate(freq=n()) %>%
  unique() %>%
  arrange(desc(),freq)
  

ggplot(head(HospStrategyWords,20),aes(x=freq,y=reorder(word,freq)))+geom_col()+
  labs(title="Top 20 words in Hospital Strategy",caption="36 Ontario Hospitals")

StrategyWordsbyType<-HospStrategyClean %>%
 group_by(Type) %>%
  unnest_tokens(word,text,c("words")) %>%
  group_by(Type,word) %>%
  mutate(FreqbyType=n()) %>%
  distinct_all() %>%
  group_by(Type)%>%
  mutate(WordsbyType=sum(FreqbyType)) %>%
  mutate(PctFreq=FreqbyType/WordsbyType*100) %>%
  filter(word!="se") %>%
  slice_max(order_by = PctFreq,n=10)
                            

ggplot(StrategyWordsbyType,aes(x=reorder(word,PctFreq),y=PctFreq,fill=Type))+geom_col()+coord_flip()+ #facet_wrap(~Type) %>%
  labs(title = "Most Frequent Words by Type of Hospital") +
  xlab("Word sorted by Frequency")+ylab("Frequency by type of Hospital")+theme(legend.position="bottom")

# Document Similarity
## small sample

## look at one hospital at a time initially
MinSmall<-HospMinutes %>%
  mutate(DocID=paste0(Organization,"-",FileName,Heading),sep="")%>%
  select(Organization,Heading,Date,DocID,BlockText) %>%
  unique() %>%
  mutate(duplicate=duplicated(DocID)) %>%
  filter(duplicate==FALSE) %>%
  select(-c(duplicate)) %>%
  mutate(HeadLine=str_sub(BlockText,1,50))

MinSmall<-CleanDFText(MinSmall,"BlockText")
  
StratSmall<-HospStrategyClean


d = data.frame(id = c(1,2,3),
               text = c('Socrates is human', 'Humans are mortal', 'Therefore, Socrates is mortal'),
               author = c('Aristotle','Aristotle','Aristotle'),
               stringsAsFactors = F)

corp = corpus(d, docid_field = 'id', text_field='text')
dtm = dfm(corp)

dtm
docvars(dtmMin)
MinDTM<-corpus(MinSmall,docid_field = "DocID",text_field = "BlockText")
dtmMin=dfm(MinDTM)
StratDTM<-corpus(StratSmall,docid_field = "Organization",text_field = "text")
StratDTM=dfm(StratDTM)
docvars(dtmMin)
head(docvars(dtmMin),3)
dtm=rnewsflow_dfm
dtm

