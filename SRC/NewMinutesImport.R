library(tm)
library(pdftools)
rm(list=ls())
### File to import Niagara protected PDFs

i<-1
HospitalName<-"SinaiHealth"
DBSource<-"~/BoardAnalytics/HospitalBoardUpdatesJune2021/SinaiHealth"
NHSFiles<-as.data.frame(list.files(DBSource,full.names = TRUE,all.files=FALSE,recursive = FALSE))
for(i in 1:nrow(NHSFiles)){
  #doc <- readPDF(control = list(text = "-layout"))(elem = list(uri = NHSFiles[i,1]),
   #                                                     language = "en",
    #                                                    id = "id1")
doc<-pdf_ocr_text(NHSFiles[i,1])
  
 # doc<-iconv(doc,"utf-8","ASCII")
  
  write_lines(doc,file.path(DBSource,paste("SinaiHealth - ",basename(NHSFiles[i,1]),".csv",sep="")))

}


