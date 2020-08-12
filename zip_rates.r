library(dplyr)

setwd ("")

zip<-data.frame(read.csv("rms_fio_2019 - zip.csv", header=TRUE, stringsAsFactors =FALSE))
zip$zip<-as.numeric(zip$zip)

denoms<-data.frame(read.csv("Boston_zip_sociodemos.csv", header=TRUE, stringsAsFactors =FALSE))
denoms$totpop<-denoms$SE_A00001_001
denoms$male<-denoms$SE_A02001_002
denoms$female<-denoms$SE_A02001_003
denoms$NHwhite<-denoms$SE_A04001_003
denoms$NHblack<-denoms$SE_A04001_004
denoms$NHnative<-denoms$SE_A04001_005
denoms$NHasianPI<-denoms$SE_A04001_006+denoms$SE_A04001_007
denoms$Latinx<-denoms$SE_A04001_010
denoms$zip<-denoms$Geo_ZCTA5

denoms<-select(denoms, zip, totpop, male, female, NHwhite, NHblack, NHnative, NHasianPI, Latinx)

all<-merge(x=zip, y=denoms, by="zip", all.x=TRUE)

all$total_rate<-ifelse(all$total.reports==0,0,(all$total.reports/all$totpop)*100000)
all$frisk_rate<-(all$frisked/all$totpop)*100000
all$searchperson_rate<-(all$searchperson/all$totpop)*100000
all$searchvehicle_rate<-(all$searchedvehicle/all$totpop)*100000
all$summons_rate<-(all$summonsissued/all$totpop)*100000

write.csv(all, file="overall_rates_by_zip.csv")
