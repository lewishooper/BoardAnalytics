library(tm)
library(pdftools)
rm(list=ls())
### File to import Niagara protected PDFs

i<-1
SourceFile<-"~/BoardAnalytics/TheGreatOverhaul/NewMinutes"
HospitalName<-"SinaiHealth"
DBSource<-"~/BoardAnalytics/TheGreatOverhaul/NewMinutes"
OutputFile<-"~/BoardAnalytics/TheGreatOverhaul/NewMinutesasText"
NewFiles<-as.data.frame(list.files(DBSource,full.names = TRUE,all.files=TRUE,recursive = TRUE))
for(i in 1:nrow(NewFiles)){
 
doc<-pdf_ocr_text(NewFiles[i,1])
  
FileName<-NewFiles[i,1]
 # doc<-iconv(doc,"utf-8","ASCII")
  dirname(NewFiles[i,1])
  ParentFolder<-str_remove( dirname(NewFiles[i,1]),"/home/skip21/BoardAnalytics/TheGreatOverhaul/NewMinutes/")
  write_lines(doc,file.path(OutputFile,paste(ParentFolder," - ",basename(NewFiles[i,1]),".csv",sep="")))

}


