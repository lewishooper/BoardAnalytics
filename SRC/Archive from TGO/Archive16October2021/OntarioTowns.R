#iMPORT Provincial Data

rm(list=ls())

DBSource<-("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/RawProvinicalResidenceData")
RDSfiles<-c("~/BoardAnalytics/TheGreatOverhaul/Results")
CompletedFiles<-c("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData")
LocationNames<-"~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/RawProvinicalResidenceData"


LocationNames<-read_csv("~/BoardAnalytics/TheGreatOverhaul/SourceData/OtherData/RawProvincialResidenceData/NameFinal.csv")
CityNames<-LocationNames %>%
  select(Name) %>%
  unique()
MunicipalityNames<-LocationNames %>%
  select(Municipality) %>%
  unique() %>%
  rename(Name=Municipality)
CountyNames<-LocationNames %>%
  select(County) %>%
  unique() %>%
  rename(Name=County)

OntarioLocations<-bind_rows(CityNames,MunicipalityNames,CountyNames) %>%unique() %>% drop_na() %>%
mutate(lengthS=stri_length(OntarioLocations$Name)) %>%
  mutate(name=str_replace_all(Name,c("\\W")," ")) %>%
  mutate(name=tolower(name))%>%
  mutate(name=toTitleCase(name))
saveRDS(OntarioLocations,file.path(RDSfiles,"OntarioLocations.rds"))
         