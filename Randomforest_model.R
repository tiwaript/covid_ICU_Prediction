require(randomForest)
require(caret)
## data_covid_wc is the working data copy

index_icu<-which(data_covid_wc$icugr_r=="Yes")
sample_icu<-sample(index_icu,size=0.7*length(index_icu),replace = F)

index_noicu<-which(data_covid_wc$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

data_training<-data_covid_wc[c(sample_icu,sample_noicu),]
data_testing<-data_covid_wc[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)
## noicu: icu 2:1######
sample_noicu_2<-sample(index_noicu,size=90,replace = F)
data_covid_2_1<-data_covid_wc[c(sample_noicu_2,index_icu),]

index_icu<-which(data_covid_2_1$icugr_r=="Yes")
sample_icu<-sample(index_icu,size=0.7*length(index_icu),replace = F)

index_noicu<-which(data_covid_2_1$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

data_training<-data_covid_2_1[c(sample_icu,sample_noicu),]
data_testing<-data_covid_2_1[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)