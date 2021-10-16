## UPDATE NAME AND TYPE
NameAndType<-CombinedData %>%
  mutate(Organization=str_remove(Organization," -ON")) %>%
  select(FAC,Organization,FacilityType) %>%
  filter(Organization!="Kemptville")%>%
  filter(FAC!=978)

NamesInDBFILE<-DBFileList %>%
  select(Organization) %>%
  unique
Missing<-anti_join(NamesInDBFILE,NameAndType)

df<-NameAndType[0,] 

df[1,1]=732
df[1,2]="KDH"
df[1,3]="Small Hospital"

NameAndType<-bind_rows(NameAndType,df)
df<-NameAndType[0,] 

df[1,1]=959
df[1,2]="HSCN"
df[1,3]="Teaching Hospital"

NameAndType<-bind_rows(NameAndType,df)
df<-NameAndType[0,] 

df[1,1]=978
df[1,2]="KHSC"
df[1,3]="Teaching Hospital"

NameAndType<-bind_rows(NameAndType,df)
df<-NameAndType[0,] 

df[1,1]=936
df[1,2]="LHSC"
df[1,3]="Teaching Hospital"

NameAndType<-bind_rows(NameAndType,df)

df<-NameAndType[0,] 

df[1,1]=966
df[1,2]="Sarnia"
df[1,3]="Large Community Hospital"

NameAndType<-bind_rows(NameAndType,df)

df<-NameAndType[0,] 

df[1,1]=935
df[1,2]="TBay"
df[1,3]="Teaching Hospital"

NameAndType<-bind_rows(NameAndType,df)
saveRDS(NameAndType,"~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/NameAndType.rds")
