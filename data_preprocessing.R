require(foreign)
data_covid<-read.spss("Blinded version of Epi data for PT-26 July 2020.sav",to.data.frame = T,
                      use.value.labels = T)

df_labels<-data.frame(attr(data_covid,"variable.labels"))
data_covid_wc<-data_covid[,-5]
data_covid_wc<-data_covid_wc[,-5]
data_covid_wc<-data_covid_wc[,c(1,4,13,2,3,5,6:12,14:16)]
#### index of missing values 
indexNA_sample<-which(is.na(data_covid_wc),T)[,1][-c(9,36,40)]
data_covid_wc<-data_covid_wc[-indexNA_sample,]
data_covid_wc<-data_covid_wc[-which(data_covid_wc$Age<20),]
data_covid_wc$icugr_r
