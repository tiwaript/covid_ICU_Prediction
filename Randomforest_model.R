require(randomForest)
require(caret)
############All data ####
data_covid_NArm<-data_covid[-indexNA_sample,-c(5:6)]
data_covid_NArm<-data_covid_NArm[,c(1,4,13,2,3,5,6:12,14:16)]
data_covid_NArm$CRP1<-na.roughfix(data_covid_NArm$CRP1)
data_covid_NArm$SRFERRITIN1<-na.roughfix(data_covid_NArm$SRFERRITIN1)
data_covid_NArm$icugr_r<-factor(data_covid_NArm$icugr_r,levels=c("Yes","No"))

index_icu<-which(data_covid_NArm$icugr_r=="Yes")
sample_icu<-sample(index_icu,size=0.7*length(index_icu),replace = F)

index_noicu<-which(data_covid_NArm$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

data_training<-data_covid_NArm[c(sample_icu,sample_noicu),]
data_testing<-data_covid_NArm[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat

## data_covid_wc is the working data copy####

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
## equal sampling #####
index_noicu<-which(data_covid_wc$icugr_r=="No")
sample_noicu_60<-sample(index_noicu,size=60,replace = F)
index_icu<-which(data_covid_wc$icugr_r=="Yes")
data<-data_covid_wc[c(sample_noicu_60,index_icu),]

index_noicu<-which(data$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

index_icu<-which(data$icugr_r=="Yes")
sample_icu<-sample(index_icu,size = 0.7*length(index_icu),replace=F)

data_training<-data[c(sample_icu,sample_noicu),]
data_testing<-data[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat
############# excluding crp and ferritin
index_noicu<-which(data_covid_wc$icugr_r=="No")
sample_noicu_60<-sample(index_noicu,size=60,replace = F)
index_icu<-which(data_covid_wc$icugr_r=="Yes")
data<-data_covid_wc[c(sample_noicu_60,index_icu),]

index_noicu<-which(data$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

index_icu<-which(data$icugr_r=="Yes")
sample_icu<-sample(index_icu,size = 0.7*length(index_icu),replace=F)

data_training<-data[c(sample_icu,sample_noicu),]
data_testing<-data[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3,9:10)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3,9:10)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat
index_noicu<-which(data_covid_wc$icugr_r=="No")
sample_noicu_60<-sample(index_noicu,size=60,replace = F)
index_icu<-which(data_covid_wc$icugr_r=="Yes")
data<-data_covid_wc[c(sample_noicu_60,index_icu),]

index_noicu<-which(data$icugr_r=="No")
sample_noicu<-sample(index_noicu,size = 0.7*length(index_noicu),replace=F)

index_icu<-which(data$icugr_r=="Yes")
sample_icu<-sample(index_icu,size = 0.7*length(index_icu),replace=F)

data_training<-data[c(sample_icu,sample_noicu),]
data_testing<-data[-c(sample_icu,sample_noicu),]

rf_model<-randomForest(x=data_training[,-c(1:3)],y=data_training[,3],ntree=10000,mtry=4,data=data_training)
pred_class<-predict(rf_model,data_testing[,-c(1:3)])
obs_class<-data_testing[,3]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat
############### Validation set 300 data ############
require(foreign)
data_validation<-read.spss("Blinded version of additional 300 Epi data for PT-15 Aug 2020.sav",to.data.frame = T,
                           use.value.labels = T)
