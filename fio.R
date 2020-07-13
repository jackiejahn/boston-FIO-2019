setwd ("")
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

