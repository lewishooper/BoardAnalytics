## Remove text from source files
## this is exceptional text
## 
## example is embeded strategic directions in section 9 of Dryden minutes
## 
## The dryden extraction is built as a function that can be inserted in the 
## TGOH extraction file (The Great overall)
## testbed built here using DBFile list
DBFILEList<-FullFiles
i<-202
TESTFile<-DBFILEList[i,]
TheText<-DBFileList[i,"text"]
Organization<-"Dryden"
StartPoint<-"NEW BUSINESS:"
EndPoint<-regex("\\r?\\n\\d\\d\\.")
DrydenExtracted<-function(TheText,StartPoint,EndPoint)
  StartHere<-str_locate(TheText,StartPoint)
  EndHere<-str_locate(TheText,EndPoint)
  SubText<-str_sub(TheText,StartHere[1,2],EndHere[1,1])
  SubText<-str_remove_all(SubText," {3,}Priorities *\\r?\\n")
  SubText<-str_remove_all(SubText," {3,}Pillar Topic *\\r?\\n")
  SubText<-str_remove_all(SubText,"\\r?\\n {0,2}Integrate mental *\\r?\\n")
  SubText<-str_remove_all(SubText,"\\r?\\n *health across all *\\r?\\n")
  SubText<-str_remove_all(SubText,"\\r?\\n *services *\\r?\\n")
  SubText<-str_remove_all(SubText,"\r\n *Our Patients improve their *\\r\\n")
  SubText<-str_remove_all(SubText,"\\r?\\n *services *\\r?\\n")
  return(DBFileList)
  