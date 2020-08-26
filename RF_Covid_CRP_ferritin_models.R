set.seed(1000)
rf_model_2<-randomForest(x=data_discovery[,-c(1:2,6:7)],y=data_discovery[,2],
                       ntree=10000,mtry=4,data=data_discovery,sampsize = c(40,120))
pred_class<-predict(rf_model_2,data_validation[,-c(1:2,6:7)])
obs_class<-data_validation[,2]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat

set.seed(1000)
rf_model_3<-randomForest(x=data_discovery[,-c(1:2,7)],y=data_discovery[,2],
                         ntree=10000,mtry=4,data=data_discovery,sampsize = c(40,120))
pred_class<-predict(rf_model_3,data_validation[,-c(1:2,7)])
obs_class<-data_validation[,2]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat

set.seed(1000)
rf_model_4<-randomForest(x=data_discovery[,-c(1:2,6)],y=data_discovery[,2],
                         ntree=10000,mtry=4,data=data_discovery,sampsize = c(40,120))
pred_class<-predict(rf_model_4,data_validation[,-c(1:2,6)])
obs_class<-data_validation[,2]
conf_mat<-confusionMatrix(pred_class,obs_class)
conf_mat