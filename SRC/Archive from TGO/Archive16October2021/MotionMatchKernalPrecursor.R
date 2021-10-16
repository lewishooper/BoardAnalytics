# Create Tagged Motions
## based on The Great overhaul Data
## clean up
rm(list=ls())
# Libraries
library(tidyverse)
library(tm)
#library(devtools)
library(tidytext)
library(textstem)
library(lubridate)
#library(ggpubr)
#library(gridExtra)
#library(readxl)
#library(ggpmisc)
CleanDFText<-function(DF,TextColumn){
  
  #SentimentMotions<-SentimentMotions[,c(1,2,6)]
  DF[[TextColumn]]<-gsub(pattern="\\W", replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- gsub(pattern="\\d",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<- tolower(DF[[TextColumn]])
  DF[[TextColumn]]<-removeWords(DF[[TextColumn]],stopwords("english"))
  DF[[TextColumn]]<-lemmatize_strings(DF[[TextColumn]])
  DF[[TextColumn]]<-gsub(pattern="\\b[A-z]\\b{1}",replace=" ",DF[[TextColumn]])
  DF[[TextColumn]]<-stripWhitespace(DF[[TextColumn]])
  
  #SentimentMotions$Motion<-str_split(SentimentMotions$Motion,pattern ="\\s+")
  return(DF)
}
SourceData<-"~/BoardAnalytics/TheGreatOverhaul/Results"

BlockText<-readRDS("~/BoardAnalytics/TheGreatOverhaul/Results/CorrectedBlockFiles.rds")
Sentences<-readRDS(file.path(SourceData,"BoardSentencesFile.rds"))

Motions<-readRDS("~/BoardAnalytics/TheGreatOverhaul/Results/FullMotions.rds")
FullFiles<-readRDS(file.path(SourceData,"FullFiles.rds"))

FullFiles<-CleanDFText(FullFiles,"text")
