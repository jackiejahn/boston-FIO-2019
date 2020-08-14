setwd ("/Users/zubin/Documents/GitHub/boston-FIO-2019")
library(dplyr)

name<-data.frame(read.csv("mark43_fieldcontacts_name_for_public_2019.csv", header=TRUE, stringsAsFactors =FALSE))
rec<-data.frame(read.csv("mark43_fieldcontacts_for_public_20192.csv", header=TRUE, stringsAsFactors =FALSE))
rname<-data.frame(read.csv("rms_fieldcontacts_name_for_public_2019.csv", header=TRUE, stringsAsFactors =FALSE))
rrec<-data.frame(read.csv("rms_fieldcontacts_for_public_2019.csv", header=TRUE, stringsAsFactors =FALSE))

m1<-merge(name, rec, by="fc_num", all=TRUE)
dim(m1)

m1$stop_duration<-as.numeric(m1$stop_duration)

m1 <- m1 %>% mutate(
  stop_duration2 = case_when(
    stop_duration <5  ~ "Less Than Five Minutes",
    stop_duration >=5 & stop_duration <=10  ~ "Five to Ten Minutes",
    stop_duration >10 & stop_duration <=15  ~ "Ten to Fifteen Minutes",
    stop_duration >15 & stop_duration <=20  ~ "Fifteen to Twenty Minutes",
    stop_duration >20 & stop_duration <=25  ~ "Twenty to Twenty-Five Minutes",
    stop_duration >25 & stop_duration <=30  ~ "Twenty-Five to Thirty Minutes",
    stop_duration >30 & stop_duration <=45  ~ "Thirty to Forty-Five Minutes",
    stop_duration >45 & stop_duration <=60  ~ "Forty-Five to Sixty Minutes",
    stop_duration >60 & stop_duration <=120  ~ "One to Two Hours",
    stop_duration >120   ~ "Longer Than Two Hours"
  )
)

write.csv(m1, file="mark43_fio_2019.csv")

m2<-merge(rname, rrec, by="fc_num", all=TRUE)

write.csv(m2, file="rms_fio_2019.csv")

########### 
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

m1<-data.frame(read.csv("mark43_fio_2019.csv", header=TRUE, stringsAsFactors =FALSE))
m1s<-select(m1,zip, fc_num, contact_date.x,contact_date.y,sex, race, age, build, hair_style, ethnicity, circumstance, basis)

m2<-data.frame(read.csv("rms_fio_2019.csv", header=TRUE, stringsAsFactors =FALSE))
m2s<-select(m2,zip, fc_num, contact_date.x,contact_date.y,sex, race, age, build, hair_style, ethnicity, circumstance, basis)

m3<-rbind(m1s,m2s)
m3$zip<-as.numeric(m3$zip)

white<-m3[(m3$race=="White" & m3$ethnicity=="Not of Hispanic Origin"),] %>% group_by(zip) %>% summarise(white_int=n())
black<-m3[(m3$race=="Black" & m3$ethnicity=="Not of Hispanic Origin"),] %>% group_by(zip) %>% summarise(black_int=n())
latinx<-m3[(m3$ethnicity=="Hispanic Origin"),] %>% group_by(zip) %>% summarise(latinx_int=n())

m4<-merge(white, black, by="zip")
m5<-merge(m4, latinx, by="zip")
out<-merge(x=m5, y=denoms, by="zip", all.x=TRUE)
out$black_rate<-ifelse(out$black_int>out$NHblack,1,out$black_int/out$NHblack)
out$white_rate<-ifelse(out$white_int>out$NHwhite,1,out$white_int/out$NHwhite)
out$latinx_rate<-ifelse(out$latinx_int>out$Latinx,1,out$latinx_int/out$Latinx)
out$bw_irr<-out$black_rate/out$white_rate
out$lw_irr<-out$latinx_rate/out$white_rate

write.csv(out, file="racestrat_rates.csv")

white<-m3[(m3$race=="White" & m3$ethnicity=="Not of Hispanic Origin"),] %>% group_by(zip) %>% summarise(incidents=n())
white$race<-"White"
wd<-select(denoms,zip,NHwhite)
names(wd)[2] <- "denom"
white<-merge(white,wd, by="zip")
black<-m3[(m3$race=="Black" & m3$ethnicity=="Not of Hispanic Origin"),] %>% group_by(zip) %>% summarise(incidents=n())
black$race<-"Black"
bd<-select(denoms,zip,NHblack)
names(bd)[2] <- "denom"
white<-merge(black,bd, by="zip")
latinx<-m3[(m3$ethnicity=="Hispanic Origin"),] %>% group_by(zip) %>% summarise(incidents=n())
latinx$race<-"Latinx"
ld<-select(denoms,zip,Latinx)
names(ld)[2] <- "denom"
latinx<-merge(latinx,ld, by="zip")

all<-rbind(white,black,latinx)
all$white<-ifelse(all$race=="White",1,0)
all$black<-ifelse(all$race=="Black",1,0)
all$latinx<-ifelse(all$race=="Latinx",1,0)

# modeling to get IRRs & CIs
library(lme4)
#tot<-out %>% group_by(zip) %>% summarise(tot=sum(black_int,white_int,latinx_int))
#out2<-merge(tot,out,by="zip")

mod1<-glmer(incidents ~ as.factor(race) + offset(data=all)

