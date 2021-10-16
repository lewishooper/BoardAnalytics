### CHECK MINUTE for Agenda and Attached material

rm(list=ls())
library(readr)
library(tidyverse)
CheckForEndofMinutes<-function(theText,BeginingOfEnd,ActualEnd){# Both sets of keywords can be regex
  # will vary by Hosptial
  # will return a Dataframe Called Closeout
  # Dataframe will have Beginpoint Endpoint and enclosed Text between begin point and end point
  
  }



DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")


DBFileList<-readRDS(file.path(RDSfiles,"FullFiles.rds"))   ## same as DBFileList
MeetingEndsKeywords<-c("adjournment","adjourned ",regex("\\r?\\nChair *Corporate"))

i<-164
DBFileList[i,"FileName"]

for(i in 1:nrow(DBFileList)){
  TestText<-tolower(DBFileList[i,"text"])
  ENDS<-str_locate_all(TestText,MeetingEndsKeywords)
  dbStarts<-tibble(unlist(ENDS))
  DBFileList[i,"spread"]<-max(dbStarts)-min(dbStarts)
  ENDS<-as.String(unlist(ENDS))
  DBFileList[i,"Endings"]<-paste(ENDS,sep="")
  DBFileList[i,"length"]<-nchar(TestText)
  DBFileList[i,"EndGap"]<-nchar(TestText)/max(dbStarts)
}

## Deal with Cambridge
Cambridge<-DBFileList %>%
  filter(Organization=="CambridgeMemorial") %>%
  select(-Endings)
MeetingEndsKeywords<-c("adjournment","adjourned ")
for( i in 1:nrow(Cambridge)){
starts=str_locate(Cambridge[i,"text"],regex("BOARD OF DIRECTORS MEETING"))
first<-as.data.frame(unlist(starts))
StartHere<-first[1,1]
ENDS<-str_locate_all(tolower( Cambridge[i,"text"]),MeetingEndsKeywords)
dbends<-as.data.frame(unlist(ENDS))
Cambridge[i,"start"]<-StartHere
TheEnds<-as.String(dbends[,1])
TheEnds<-str_replace_all(TheEnds,"\n",",")
Cambridge[i,"endings"]<-TheEnds
Cambridge[i,"MultipleStarts"]=nrow(first) # Note all only one so proceed
Cambridge[i,"MinuteText"]<-str_sub(Cambridge[i,"text"],StartHere,max(dbends[,1]))
Cambridge[i,"NewFile"]<-paste("~/BoardAnalytics/DataDumps/Cambridge/CambridgeMemorial - ","Amended-",Cambridge[i,"FileName"],"csv",sep="")
Cambridge[i,"endingText"]<-str_sub(Cambridge[i,"text"],max(dbends[,1])-50,-1)
writeLines(Cambridge[i,"MinuteText"],Cambridge[i,"NewFile"])
}

i<-1 #i<-8
# Generic approach 3
SELECTEDHOSPITAL<-"Atikokan"
Hospital <-DBFileList %>%
  filter(Organization==SELECTEDHOSPITAL) %>%
  select(-Endings)
#(?<=Adjournment).*?(?=CARRIED)
#<=Adjournment)(.*)(?=CARRIED)
MeetingEndsKeywords<-regex("(?<=adjourned).*(?=CEO Report|CARRIED)")
for( i in 1:nrow(Hospital)){
  Hospital[i,"FileName"]
  starts=str_locate(Hospital[i,"text"],regex("Board of Directors Regular Board Meeting"))
  first<-as.data.frame(unlist(starts))
  StartHere<-first[1,1]
  ENDS<-str_locate_all(str_squish(Hospital[i,"text"]),MeetingEndsKeywords)
  dbends<-as.data.frame(unlist(ENDS))
  Hospital[i,"start"]<-StartHere
  
  TheEnds<-as.String(dbends[,1])
  TheEnds<-str_replace_all(TheEnds,"\n",",")
  EndHere<-max(dbends[,1])
  Hospital[i,"endings"]<-TheEnds
  Hospital[i,"MultipleStarts"]=nrow(first) # Note all only one so proceed
  Hospital[i,"MinuteText"]<-str_sub(Hospital[i,"text"],StartHere,EndHere)
  Hospital[i,"NewFile"]<-paste("~/BoardAnalytics/DataDumps/Hospital/",SELECTEDHOSPITAL," - ","Amended-",Hospital[i,"FileName"],"csv",sep="")
  TheText<-Hospital[i,"MinuteText"]
  Hospital[i,"endingText"]<-str_sub(Hospital[i,"text"],EndHere,-1)
  if(!is.na(EndHere)){
    writeLines(TheText,Hospital[i,"NewFile"])
  }
}
str_sub(Hospital[i,"text"],14776,14876)
#########################################


### Generic Hospital approach
SELECTEDHOSPITAL<-"SouthBruceGrey"
Hospital <-DBFileList %>%
  filter(Organization==SELECTEDHOSPITAL) %>%
  select(-Endings)
#(?<=Adjournment).*?(?=CARRIED)
#<=Adjournment)(.*)(?=CARRIED)
MeetingEndsKeywords<-regex("(?<=Adjournment)(.*)(?=CARRIED|Your Health)")
for( i in 1:nrow(Hospital)){
  starts=str_locate(Hospital[i,"text"],regex("Minutes|Winutes"))
  first<-as.data.frame(unlist(starts))
  StartHere<-first[1,1]
  ENDS<-str_locate_all(str_squish(Hospital[i,"text"]),MeetingEndsKeywords)
  dbends<-as.data.frame(unlist(ENDS))
  Hospital[i,"start"]<-StartHere
 
  TheEnds<-as.String(dbends[,1])
  TheEnds<-str_replace_all(TheEnds,"\n",",")
  EndHere<-max(dbends[,1])
  Hospital[i,"endings"]<-TheEnds
  Hospital[i,"MultipleStarts"]=nrow(first) # Note all only one so proceed
  Hospital[i,"MinuteText"]<-str_sub(Hospital[i,"text"],StartHere,EndHere)
  Hospital[i,"NewFile"]<-paste("~/BoardAnalytics/DataDumps/Hospital/",SELECTEDHOSPITAL," - ","Amended-",Hospital[i,"FileName"],"csv",sep="")
  TheText<-Hospital[i,"MinuteText"]
  Hospital[i,"endingText"]<-str_sub(Hospital[i,"text"],EndHere,-1)
  if(!is.na(EndHere)){
  #writeLines(TheText,Hospital[i,"NewFile"])
  }
}
Hospital2<-Hospital %>%
  filter(is.na(MinuteText))
i<-2

### Generic Hospital 2ND LEVEL  approach
SELECTEDHOSPITAL<-"SouthBruceGrey"
Hospital <-Hospital2 %>%
  filter(Organization==SELECTEDHOSPITAL) 
#(?<=Adjournment).*?(?=CARRIED)
#<=Adjournment)(.*)(?=CARRIED)
MeetingEndsKeywords<-regex("(?<=Paul Rosebush )(.*)(?=Health)")
for( i in 1:nrow(Hospital)){
  starts=str_locate(Hospital[i,"text"],regex("Minutes|Winutes"))
  first<-as.data.frame(unlist(starts))
  StartHere<-first[1,1]
  ENDS<-str_locate_all(str_squish(Hospital[i,"text"]),MeetingEndsKeywords)
  dbends<-as.data.frame(unlist(ENDS))
  nrow(dbends)
  if(nrow(dbends)==0){
    MeetingEndsKeywords2<-regex("(?<=adjourned ).*(?=Health)")
    ENDS<-str_locate_all(str_squish(Hospital[i,"text"]),MeetingEndsKeywords2)
    dbends<-as.data.frame(unlist(ENDS))
  }
  if(nrow(dbends)==0){
    MeetingEndsKeywords2<-regex("(?<=irectors ).*(?=Rosebush)")
    ENDS<-str_locate_all(str_squish(Hospital[i,"text"]),MeetingEndsKeywords2)
    dbends<-as.data.frame(unlist(ENDS))
  }
  Hospital[i,"start"]<-StartHere
  
  TheEnds<-as.String(dbends[,1])
  TheEnds<-str_replace_all(TheEnds,"\n",",")
  EndHere<-max(dbends[,1])
  Hospital[i,"endings"]<-TheEnds
  Hospital[i,"MultipleStarts"]=nrow(first) # Note all only one so proceed
  Hospital[i,"MinuteText"]<-str_sub(Hospital[i,"text"],StartHere,EndHere)
  Hospital[i,"NewFile"]<-paste("~/BoardAnalytics/DataDumps/Hospital/",SELECTEDHOSPITAL," - ","Amended-",Hospital[i,"FileName"],"csv",sep="")
  TheText<-Hospital[i,"MinuteText"]
  if(!is.na(EndHere)){    writeLines(TheText,Hospital[i,"NewFile"])  }
}


MinuteList=list(HSCN=list("HSCN",StartM=c("MOTION:","The Board of Directors APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED","Carried","\\r\\n\\d{1,2}\\.\\d{0,2}")),
                LHSC=list("LHSC",StartM=c("It was MOVED","APPROVED by GENERAL CONSENT","\r\n\\s*\\d{1,2}\\.\\dADJOURNMENT"),EndM=c("CARRIED",regex("\\n\\r\\d{1,2}\\.\\d"),"ADJOURNED by GENERAL CONSENT","\\n\\d{1,2}\\.\\d{0,2}")),
                Aliston=list("Aliston",StartsM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndsM=c("Carried","CARRIED","meeting terminated","Passed","passed")),
                AlmontGeneral=list("AlmontGeneral",StartM=c("IT WAS MOVED"),EndM=c("CARRIED","ADJOURNED")),
                Atikokan=list("Atikokan",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Dryden=list("Dryden",StartM=c("MOTION","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\\n\\r\\n")),   
                SickKids=list("SickKids",StartM=c("Moved","MOVED","On a Motion by","was moved by","MOTION","IT WAS RESOLVED","there being no further business"),EndM=c("Carried","CARRIED","ClosedSK","CloseSK","adjourned")),
                GrandRiver=list("GrandRiver",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                GenericHospital=list("GenericHospital",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                KDH=list("KDH",StartM=c("Motion:","ADJOURNMENT"),EndM=c("\r\\n\\d\\.\\d","\r\\n\\r\\n")),
                HaliburtonHighlands=list("HaliburtonHighlands",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                HHSC=list("HHSC",StartM=c("The Board of Directors APPROVED by GENERAL CONSENT","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                LACGH=list("LACGH",StartM=c("MOTION","Motion ","Adjournment"),EndM=c("The motion was carried.","MOTIONFAILEDTOPASS")),
                
                Mackenzie =list("Mackenzie",StartM=c("MOVED","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MicsGroup=list("MicsGroup",StartM=c("MOVED","Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                MSH=list("MSH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NiagaraHealthSystem=list("NiagaraHealthSystem",StartM=c("It was MOVED","MOTION:","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                NorfolkGeneral=list("NorfolkGeneral",StartM=c("On a Motion ","It was Moved","On a Motion by","MOVED BY:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                Northumberland=list("Northumberland",StartM=c("MOTION:","Moved","On a motion","On a Motion by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\\n\\d{1,2}\\.\\d{0,2}")),
                
                PerthSmithFalls=list("PerthSmithFalls",StartM=c("MOVED by","MOTION:","moved by","Moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                RMH=list("RMH",StartM=c("MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                Sarnia=list("Sarnia",StartM=c("Motion duly made","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                SouthBruceGrey=list("SouthBruceGrey",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d","\r\n\r\n\r\n")),
                Temiskaming=list("Temiskaming",StartM=c("Moved by:","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                TrilliumHospital=list("TrilliumHospital",StartM=c("MOVED by","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                WilliamOsler=list("WilliamOsler",StartM=c("MOVED ","MOVED/Seconded","MOTION:","moved by","ADJOURNMENT"),EndM=c("Carried","CARRIED","carried","\r\\n\\d\\.\\d")),
                
                StThomasElgin=list("StThomasElgin",StartM=c("MOVED","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                CambridgeMemorial=list("CambridgeMemorial",StartM=c("MOTION","ADJOURNMENT"),EndM=c("CARRIED","adjourned")),
                SinaiHealth=list("SinaiHealth",StartM=c("Whereas","ADJOURNMENT"),EndM=c("resolved.+\\.","adjourned","ADJOURNMENT")),
                
                SunnyBrook=list("SunnyBrook",StartM=c("Upon MOTION duly made","Chair requested approval of the agenda","There being no further business"),EndM=c("CARRIED","Agenda was approved as circulated","MEETING TERMINATED","Chair adjourned the open session","the Chair terminated the Organizational Meeting")),
                Barrie=list("Barrie",StartM=c("duly moved, seconded and carried","ADJOURNMENT"),EndM=c("ClosedB","CloseB","Closedb","Closeb","\\(carried\\)","adjourned","RESOLVED.+\\.")),
                JoeBrant=list("JoeBrant",StartM=c("OpenJB","The Chair requested a motion","duly moved","MOVED","Moved by"),EndM=c("ClosedJB","Carried","CARRIED","adjourned ")),
                
                ChathamKent=list("ChathamKent",StartM=c("IT WAS AGREED THAT","Motion","Moved by","OpenCK","There being no further business"),EndM=c("Carried","CARRIED","The motion was carried.","approved by the Supervisor for all three boards","ClosedCK","adjourned")),
                LakeridgeHealth=list("LakeridgeHealth",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                KHSC=list("KHSC",StartM=c("Moved","MOVED","The Chair requested a motion","On a Motion by","was moved by"),EndM=c("Carried","CARRIED","meeting terminated")),
                TBay=list("TBay",StartM=c("Moved by:","MOVED"),EndM=c("Carried","CARRIED","adjourned","\r\\n\\d\\.\\d","defeated")),
                UHN=list("UHN",StartM=c("Moved","MOVED","Management recommends that the Board approve","Upon motion","Upon motion made","On a motion duly made","that the board of trustees approve",
                                        "Adjournment","Management requests that the Board of Trustees approve","Motion for Approval"),
                         EndM=c("\\n\\d{1,2}\\.\\d{1,2}","\r\\n\\t\\d\\.\\d","\r\\n\\w\\)","There being no further business, the meeting was adjourned","the meeting of the Board of Trustees was adjourned."))
)
