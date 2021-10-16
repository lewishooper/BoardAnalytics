#Error Checking on IMport

rm(list=ls())

#devtools::install_github("hadley/lineprof")
library(tools)
#library(readtext)
library(readr)
#library(ngram)
library(tm)
library(textstem)
#library(tesseract)
library(udpipe)
#eng <- tesseract("eng")
library(lubridate)
library(corpus)
#library(textreadr)
library(tidytext)
library(tidyverse)
library(stringr)
library(stringi)
#library(pdftools)
library(wordcloud)
### functions


DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")

DBFileList<-readRDS(file.path(RDSfiles,"FullFiles.rds"))

DuplicateNames<-DBFileList %>%
  select(FileName) %>%
  unique()
# No duplicate names
DuplicateDates<-tibble(DBFileList) %>%
  group_by(Organization)%>%
  arrange(DATE) %>%
  mutate(flagD=ifelse(lead(DATE)==DATE,TRUE,FALSE)) %>%
  mutate(FlagT=ifelse(lead(text)==text,TRUE,FALSE)) %>%
  mutate(HIT=(FlagT & flagD))

         