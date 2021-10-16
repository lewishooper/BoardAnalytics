# read in as block delimated text
# blocks are from one heading designation to next heading designation
# e.g. from 1.0 to 1.1 
# Data captured is Org, Date, File Name, Block#, BlockHeading,Block Text
# 

### Sequence 
## first readbyheading3.r
## Then AssignFocusandSentimentv2<<---- We Are Here
## then blockprocessing.r ---> Create riverbed and sentiment graphs????

rm(list=ls())
library(readr)
library(tidyverse)
library(lubridate)
library(tm)

#Functions
ChangeCode<-function(ChangeCode,N,Phrase){
  print(OrgCodes[OrgCodes==target,])
  OrgCodes[OrgCodes==target,N]<-Phrase
  print(OrgCodes[OrgCodes==target,])
  saveRDS(OrgCodes,"~/UbBig/TextAnalysis/SourceData/OrgCodes.rds")
}
###########note THIS VERISION USED DOCILLION FROM PDF TO TEXT WITH OCR
checkme<-function(TEXTBLOCK){write.csv(TEXTBLOCK,"Checkme.csv")}
#Value1<-4.1
#Value2<- 4.0
#Now<-BlockFilesFinal[i,]
#Previous<-BlockFilesFinal[i-1,]

#checkEnd(Now,Previous)
checkEnd<-function(Now,Previous){
  NowString<-str_sub(Now[1,"BlockText"],1,7)
  PrevString<-str_sub(Previous[1,"BlockText"],1,7)
  LenN<-str_length(NowString)
  LenP<-str_length(PrevString)
  ifelse(LenN==LenP,TRUE,ifelse(LenP>LenN,PrevString<-str_sub(PrevString,1,LenN),NowString<-str_sub(NowString,1,LenP)))
  # ifelse(ComputeMatch(NowString,PrevString)==1,	ifelse(Now[1,"topLevel"]-1==Previous[1,"topLevel"],return(TRUE),return(Now[1,"listFlag"])),return(Now[1,"listFlag"]))
  ifelse(Now[1,"topLevel"]-1==Previous[1,"topLevel"] & !str_detect(Now[1,"BlockText"],"\\d{1,2}\\.\\d"),return(TRUE),return(Now[1,"listFlag"]))
}


#CheckNextSequence(Value1,Value2)

CheckNextSequence<-function( Value1,Value2){
  ifelse(Value2==0.0,return(TRUE),
         ifelse(Value1>=Value2,return(FALSE), 
                ifelse(Value2-Value1<=1,TRUE,FALSE)))
  
}


removeSecondPeriod<-function(Heading){ # remove second . in nested heading as in 4.1.1
Location<-str_locate_all(Heading,"\\.")
  first<-str_sub(Heading,1,Location[[1]][2]-1)
  second<-str_sub(Heading,Location[[1]][2]+1)
  paste0(first,second,sep="")
}
InputString<-"4.0 Consent"
CreatePattern<-function(InputString){
  inputList<-as.data.frame(str_split(InputString,"")) %>%
  rename(CharList=1) %>%
  mutate(CharType="")
 i<-4
 for(i in 1:nrow(inputList)){
   inputList[i,"CharType"]<-ifelse(str_detect(inputList[i,"CharList"],"\\d"),"d",ifelse(str_detect(inputList[i,"CharList"],"[A-Z]"),"C",ifelse(str_detect(inputList[i,"CharList"],"[a-z]"),"c",inputList[i,"CharList"])))

 }
 return(tibble(inputList))
}



ComputeMatch<-function(String1,String2){
  Primary<-CreatePattern(String1)
  Secondary<-CreatePattern(String2)

 Secondary<-if(nrow(Primary)==nrow(Secondary)){Secondary} else if(nrow(Primary)<nrow(Secondary)){Secondary[1:nrow(Primary),]} else if(nrow(Primary)>nrow(Secondary)){rbind(Secondary,data.frame(CharList=rep("X",(nrow(Primary)-nrow(Secondary))),CharType=rep("X",(nrow(Primary)-nrow(Secondary)))))}
 inSequence<-Primary[1,1]<=Secondary[1,1]

 test<-as.data.frame(Primary[,2]==Secondary[,2])%>%
    rename(Match=1) %>%
    mutate(Value=as.numeric(Match)) %>%
    mutate(Total=sum(Value))
test[1,"Total"]/nrow(test)
    
}
#ComputeMatch(,String2)


DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")

DBFileList<-readRDS(file.path(RDSfiles,"FullFiles.rds"))
#colnames(DBFileList)[1]<-"FileSource"

OrgCodes<-read.csv("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/OrgCodes.csv")
#target<-"Aliston -ON"
#N<-4
#Phrase<-"Motion Passed"
## OrgCodes edit mode
ChangeCode<-function(ChangeCode,N,Phrase){
print(OrgCodes[OrgCodes==target,])
OrgCodes[OrgCodes==target,N]<-Phrase
print(OrgCodes[OrgCodes==target,])
saveRDS(OrgCodes,"~/UbBig/TextAnalysis/SourceData/OrgCodes.rds")
}
#DBFileList$FileSource<-as.character(DBFileList$FileSource)

#Organization<-str_split(DBFileList[1,1],"/")
#OrInd<-length(unlist(Organization))
#DBFileList$Organization<- Organization[[1]][OrInd-1]
#DBFileList$Directory<-dirname(DBFileList$FileSource)
#DBFileList$FileName<-basename(DBFileList$FileSource)



#DBFileList$text<-iconv(sapply(DBFileList$FileSource,read_file),"UTF-8","ASCII",sub="")
#DBFileList$length<-lengths(gregexpr("\\W+", DBFileList$text)) + 1
#read_lines(DBFileList[814,"FileSource"])

#DBFileList<-DBFileList %>%
 # group_by(FileName) %>%
  #mutate(DashLocal=str_locate(FileName,"-")) %>%
  #mutate(Organization=str_sub(FileName,1,DashLocal[[1]])) %>%
  #mutate(Organization=paste0(Organization,"ON")) %>%
  #dplyr::select(-DashLocal)

# No duplicate file names
### Function   FIND HEADING Block
#Organization<-"Atikokan -ON"
#HeadingCode<-"\\n *\\d+. *\\d*"
#TEXTBLOCK<-DBFileList[49,"text"]
GetBlocks<-function(TEXTBLOCK,HeadingCode){
   HeadingLocation<-str_locate_all(TEXTBLOCK,regex(HeadingCode))
   HeadingLocation<-as.data.frame(do.call(cbind, HeadingLocation)) %>%
    mutate(Heading=str_sub(TEXTBLOCK,start,(end)))
   FirstHead<-tibble(start=1,end=HeadingLocation[1,1]-1,Heading="\n0.0")
    HeadingLocation<-bind_rows(FirstHead,HeadingLocation) %>%
    #HeadingLocation<-VerifyHeadings(HeadingLocation,Organization)
      mutate(textends=lead(start,1)) %>%
      mutate(textends=ifelse(is.na(textends),-1,textends)) %>%
      mutate(BlockText=str_sub(TEXTBLOCK,start,textends)) %>%
      mutate(SectionIDNumber=row_number())
   
   return(HeadingLocation)
   
   
}

#### need to fix Perth Smith files
## have a O. Mckenna that gets read as 0.Mckenna and becomes a file heading.
i<-164
BlockFiles<-tibble(Organization=character(),FileName=character(),Heading=character(),BlockText=character())
for(i in 1:nrow(DBFileList)){
  TEXTBLOCK<-DBFileList[i,"text"]
  Organization<-as.character(DBFileList[i,"Organization"])
  Date<-DBFileList[i,"DATE"]
  dummyDate<-as.character(DBFileList[i,"DATE"])
  FileName<-as.character(DBFileList[i,"FileName"])

HeadingCode<-ifelse(Organization=="Atikokan", "\\n *\\d{1,2}\\. *\\d*",
  ifelse(Organization=="KingstonHSC","(\\n\\t|\\n)\\d{1,2}\\.\\d*",
    ifelse(Organization=="SunnyBrook","(\\n\\t|\\n|\\n *)\\d{1,2}\\.\\d*",
    ifelse(Organization=="Barrie","(\\n\\t|\\n|\\n *|\\r)\\d{1,2}\\.\\d*",
    ifelse(Organization=="SickKids","\\n {0,2}\\b[A-Z'-]{2,}\\b *\\b[A-Z]{0,}\\b *\\b[A-Z]{0,}\\b *\\b[A-Z]{0,}\\b *(\\r|\\n)",
    ifelse(Organization=="WilliamOsler","(\\n *|\\n)\\d{1,2}\\.\\d{0,2}",
    ifelse(Organization=="CambridgeMemorial","(\\n|\\r)\\d{1,2}\\.\\d{0,2}\\.{0,1}\\d*",
    ifelse(Organization=="Dryden","\\n *\\d{1,2}\\.\\d{0,2}",
    ifelse(Organization=="HaliburtonHighlands","(\\n\\t|\\n|\\r|\\r\\t)\\d{1,2}\\.\\d{0,2}",
    ifelse(Organization=="Aliston","(\\n\\t|\\n {1,3}|\\n|\\r|\\r\\t)\\d{1,2}\\.\\d{0,2}\\.{0,1}\\d*",
    ifelse(Organization=="LakeridgeHealth", "\\n\\t{0,1}\\d{1,2}\\.\\d{0,2}",
     ifelse(Organization=="SouthBruceGrey", "\\s\\d{1,2}\\.\\d{0,2}\\d(?!%)",
    ifelse(Organization=="UHN", "\\n\\t{0,1}\\d{1,2}\\.\\d{0,2}",
    ifelse(Organization=="WilliamOsler", "(\\r\\t{0,1}|\\n\\t{0,1})\\d{1,2}\\.\\d{0,2}",
    ifelse(Organization=="MSH",{
           "\\n\\t{0,1}\\d{1,2}\\.\\d{0,2}"},
    ifelse(Organization=="PerthSmithFalls", {TEXTBLOCK<-str_replace_all(TEXTBLOCK,"0.\\. *McKenna","O\\. McKenna")
                                                  DBFileList[i,"text"]<-TEXTBLOCK
                                                  "\\t{0,1}\\n\\d{1,2}\\.\\d{0,2}"
                                               },
    ifelse(Organization=="SinaiHealth", "\\n\\d{1,2}\\.\\d{0,2}",
                  
         "\\n\\d{1,2}\\.\\d{0,2}"))))))))))))))))) #Default
  #SB OPTIONS  "\\n\\t\\d+.\\d*"
  
 # TestHead<-"(\\n|\\r)\\d{1,2}\\.\\d{0,2}"
  
  #HeadingCode<-TestHead
 # write.table(TEXTBLOCK)
ThisFile<-GetBlocks(TEXTBLOCK,HeadingCode)
ThisFile$Organization<-Organization
ThisFile$Date<-Date
ThisFile$DATEString<-dummyDate

ThisFile$FileName<-FileName
ThisFile$FileID<-i
ThisFile<-ThisFile %>%
  select(Organization,FileName,FileID,SectionIDNumber,Date,DATEString,Heading,BlockText)


BlockFiles<-bind_rows(BlockFiles,ThisFile)
  
}

#write_csv(TEXTBLOCK, file="TEXTBLOCKCheck.csv")
#### FIX SHORT SOUTH BRUCE GREY

BlockFile2s<-tibble(BlockFiles) %>%
  mutate(FileID=row_number()) %>%
  mutate(first=str_locate(BlockText,regex("\\n"))) %>%
  mutate(first=first[1]) %>%
  mutate(headingText=str_sub(BlockText,1,first)) %>%
  dplyr::select(-first) %>%
  group_by(FileName) %>%
  mutate(blocksPerFile=n()) %>%
  filter(blocksPerFile<=3)

#write_csv(TEXTBLOCK, file="TEXTBLOCKCheck.csv")

BlockFile2L<-tibble(BlockFiles) %>%
  mutate(FileID=row_number()) %>%
  mutate(first=str_locate(BlockText,regex("\\n"))) %>%
  mutate(first=first[1]) %>%
  mutate(headingText=str_sub(BlockText,1,first)) %>%
  dplyr::select(-first) %>%
  group_by(FileName) %>%
  mutate(blocksPerFile=n()) %>%
  filter(blocksPerFile>=30)

### check sequence of block files and merge out of seqeuency
## purpose here is to clean up those erroneous blocks created when number is read as a block header
## First step will be check sequence  e.g. Is the next block the next logic step
## 4.0 Block 1,  2101 can't be block 2
Heading<-"4.0"
BlockFilesFinal<-BlockFiles %>%
 mutate(Heading=ifelse(Organization=="SickKids",paste(SectionIDNumber-1,".0",sep=""),Heading)) %>%
  mutate(topLevel=as.integer(str_extract(Heading,"\\d{1,2}"))) %>%
  mutate(Lag=lag(topLevel)) %>%
  mutate(CheckFlag=ifelse(SectionIDNumber==1,"Starting",ifelse(topLevel==Lag|topLevel==Lag+1,"inSequence",ifelse(topLevel==1 & Lag>=2,"startList","CheckMe"))))
BlockFilesFinal$listFlag<-FALSE
i<-22309
for(i in 2:nrow(BlockFilesFinal)){
 # StartFlag<-FALSE # are we in a list No not to start
  #EndFlag<-TRUE  # are we ending a list Yes by default
  if(BlockFilesFinal[i,"CheckFlag"]=="startList"){
    StartFlag=TRUE
    EndFlag=FALSE
    BlockFilesFinal[i,"listFlag"]<-TRUE
    next()
  }
  BlockFilesFinal[i,"FileName"]
BlockFilesFinal[i,"listFlag"]<-ifelse(BlockFilesFinal[i-1,"listFlag"]==TRUE,checkEnd(BlockFilesFinal[i,],BlockFilesFinal[i-1,]),BlockFilesFinal[i,"listFlag"])
    
  }
i<-102

BlockFilesCheckMe<-BlockFilesFinal %>%
  mutate(FINALTallyCheck=paste(listFlag,CheckFlag)) %>%
  tibble::rowid_to_column("ID") %>%
  group_by(FINALTallyCheck) %>%
  mutate(maxpergroup=n())
BlockFilesCheckMe$CheckMe2<-FALSE
for(i in 1:nrow(BlockFilesCheckMe)){
  if(BlockFilesCheckMe[i,"CheckFlag"]=="CheckMe"&BlockFilesCheckMe[i,"listFlag"]==FALSE) for(j in -2:2){BlockFilesCheckMe[i+j,"CheckMe2"]<-TRUE}
  
}


NewDF<-BlockFilesFinal[0,]
TempDF<-BlockFilesFinal
TempDF<-TempDF %>%
  ungroup() %>%
  mutate(PercentFlag=str_detect(str_sub(BlockText,1,8),"%")) %>%
 # mutate(NiagraFlag=ifelse(Organization=="NiagaraHealthSytem",ifelse(str_detect(BlockText,"^\\r\\d{2}\\. \\d{2}"),listFlag=TRUE),FALSE),FALSE) %>%
  mutate(listFlag=ifelse(topLevel>=35,TRUE,listFlag))%>%
  mutate(listFlag=ifelse(PercentFlag==TRUE,TRUE,listFlag))

NewText<-""
for(i in 1:(nrow(TempDF)-1)){TempDF[i,"Lead"]<-TempDF[i+1,"listFlag"]}
i<-1554

for(i in 1:nrow(TempDF)){
  ifelse(TempDF[i,"listFlag"]==FALSE & TempDF[i,"Lead"]==FALSE,NewDF<-bind_rows(NewDF,TempDF[i,]),
         ifelse(TempDF[i,"listFlag"]==FALSE & TempDF[i,"Lead"]==TRUE,NewDF<-bind_rows(NewDF,TempDF[i,]),
                ifelse(TempDF[i,"listFlag"]==TRUE & TempDF[i,"Lead"]==TRUE,NewText<-paste(NewText,TempDF[i,"BlockText"]),{
                       NewDF[nrow(NewDF),"BlockText"]<-paste(NewDF[nrow(NewDF),"BlockText"],NewText,TempDF[i,"BlockText"])
                       NewText<-""}) ))
  
}




saveRDS(NewDF,file.path(CompletedFiles,"CorrectedBlockFiles.rds"))
TotalFiles=1500
TotalOrgs=36
TotalBlocksPerOrg<-BlockFilesFinal %>%
  select(Organization) %>%
  group_by(Organization) %>%
   count(Organization) %>%
  rename(blocksperOrg=n)

TotalOrgs<-BlockFilesFinal %>%
  select(Organization,FileName) %>%
  unique() %>%
  select(Organization) %>%
  group_by(Organization) %>%
  count(Organization) %>%
  rename(meetings=n)
blocksperMeeting<-left_join(TotalOrgs,TotalBlocksPerOrg ) %>%
  mutate(AvgBlocks=blocksperOrg/meetings)
  
  