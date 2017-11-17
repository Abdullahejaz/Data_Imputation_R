
#Scheme for producing NA values in the iris Dataset
dataset<- (iris)
summary(iris)

missing<- function(df, percentage, totalDataCount){
  allowedNAVal <- (totalDataCount * percentage)/100
  cnt <- 1
  for(i in 1:length(df$Sepal.Length)){
    if(df$Sepal.Length[i] > 4.5 &&  cnt<= allowedNAVal)
    {
      df$Sepal.Length[i]<- NA
      print(df$Sepal.Length[i])
      cnt <- cnt+1
    }#if
  }#for
  return(df)
}#fun

mm1<- missing(iris, 1, 600)
mm2<- missing(iris, 2.5, 600)
mm3<- missing(iris, 5, 600)
mm4<- missing(iris, 7.5, 600)
mm5<- missing(iris, 10, 600)
mm6<- missing(iris, 12.5, 600)

missing1<- function(df1, percentage1, totalDataCount1){
  allowedNAVal1 <- (totalDataCount1 * percentage1)/100
  cnt1 <- 1
  for(i in 1:length(df1$Petal.Length)){
    if(df1$Petal.Length[i] > 4 &&  cnt1<= allowedNAVal1)
    {
      df1$Petal.Length[i]<- NA
      print(df1$Petal.Length[i])
      cnt1 <- cnt1+1
    }#if
  }#for
  return(df1)
}

nn1<- missing1(mm1, 1, 600)
nn2<- missing1(mm2, 2.5, 600)
nn3<- missing1(mm3, 5, 600)
nn4<- missing1(mm4, 7.5, 600)
nn5<- missing1(mm5, 10, 600)
nn6<- missing1(mm6, 12.5, 600)

amelia_sch1 <- amelia(nn1, m=1, parallel = "multicore", noms = "Species")
amelia_sch2 <- amelia(nn2, m=1, parallel = "multicore", noms = "Species")
amelia_sch3 <- amelia(nn3, m=1, parallel = "multicore", noms = "Species")
amelia_sch4 <- amelia(nn4, m=1, parallel = "multicore", noms = "Species")
amelia_sch5 <- amelia(nn5, m=1, parallel = "multicore", noms = "Species")
amelia_sch6 <- amelia(nn6, m=1, parallel = "multicore", noms = "Species")

completeData_sch_am1<- amelia_sch1$imputations[[1]]
completeData_sch_am2<- amelia_sch2$imputations[[1]]
completeData_sch_am3<- amelia_sch3$imputations[[1]]
completeData_sch_am4<- amelia_sch4$imputations[[1]]
completeData_sch_am5<- amelia_sch5$imputations[[1]]
completeData_sch_am6<- amelia_sch6$imputations[[1]]

rmse_sch_am1<- rmse(completeData_sch_am1[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_am2<- rmse(completeData_sch_am2[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_am3<- rmse(completeData_sch_am3[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_am4<- rmse(completeData_sch_am4[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_am5<- rmse(completeData_sch_am5[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_am6<- rmse(completeData_sch_am6[,-5], iris[,-5], na.rm = TRUE)

rmse_sch_am1
rmse_sch_am2
rmse_sch_am3
rmse_sch_am4
rmse_sch_am5
rmse_sch_am6

tot_rmse <- c(rmse_sch_am1, rmse_sch_am2, rmse_sch_am3, rmse_sch_am4, rmse_sch_am5, rmse_sch_am6)
per_col <- c(2,5,10,15,20,25)
rmse_df <- data.frame(percentage = per_col,error = tot_rmse);
barplot(rmse_df$error,  ylab = "RMSE", main="RMSE distribution of missing data (2 to 25%)")


#For k-NN
normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))

completeData_sch_am1_normal<- as.data.frame(lapply(completeData_sch_am1[,c(1,2,3,4)], normalize))
completeData_sch_am2_normal<- as.data.frame(lapply(completeData_sch_am2[,c(1,2,3,4)], normalize))
completeData_sch_am3_normal<- as.data.frame(lapply(completeData_sch_am3[,c(1,2,3,4)], normalize))
completeData_sch_am4_normal<- as.data.frame(lapply(completeData_sch_am4[,c(1,2,3,4)], normalize))
completeData_sch_am5_normal<- as.data.frame(lapply(completeData_sch_am5[,c(1,2,3,4)], normalize))
completeData_sch_am6_normal<- as.data.frame(lapply(completeData_sch_am6[,c(1,2,3,4)], normalize))


iris_train_sch_am <- iris_normal[1:150,]
iris_train_target_sch_am <- iris[1:150, 5]
iris_test_target_sch_am <- iris[1:150, 5]

iris_test_sch_am1 <- completeData_sch_am1_normal[1:150,]
iris_test_sch_am2 <- completeData_sch_am2_normal[1:150,]
iris_test_sch_am3 <- completeData_sch_am3_normal[1:150,]
iris_test_sch_am4 <- completeData_sch_am4_normal[1:150,]
iris_test_sch_am5 <- completeData_sch_am5_normal[1:150,]
iris_test_sch_am6 <- completeData_sch_am6_normal[1:150,]

require(class)

#building the model using knn
model_sch_am1<- knn(train=iris_train_sch_am, test= iris_test_sch_am1, cl=iris_train_target_sch_am, k=13)
model_sch_am2<- knn(train=iris_train_sch_am, test= iris_test_sch_am2, cl=iris_train_target_sch_am, k=13)
model_sch_am3<- knn(train=iris_train_sch_am, test= iris_test_sch_am3, cl=iris_train_target_sch_am, k=13)
model_sch_am4<- knn(train=iris_train_sch_am, test= iris_test_sch_am4, cl=iris_train_target_sch_am, k=13)
model_sch_am5<- knn(train=iris_train_sch_am, test= iris_test_sch_am5, cl=iris_train_target_sch_am, k=13)
model_sch_am6<- knn(train=iris_train_sch_am, test= iris_test_sch_am6, cl=iris_train_target_sch_am, k=13)

tab_sch_am1<- table(iris_test_target_sch_am, model_sch_am1)
tab_sch_am2<- table(iris_test_target_sch_am, model_sch_am2)
tab_sch_am3<- table(iris_test_target_sch_am, model_sch_am3)
tab_sch_am4<- table(iris_test_target_sch_am, model_sch_am4)
tab_sch_am5<- table(iris_test_target_sch_am, model_sch_am5)
tab_sch_am6<- table(iris_test_target_sch_am, model_sch_am6)

tab_sch_am1
tab_sch_am2
tab_sch_am3
tab_sch_am4
tab_sch_am5
tab_sch_am6

confusionMatrix(tab_sch_am1)
confusionMatrix(tab_sch_am2)
confusionMatrix(tab_sch_am3)
confusionMatrix(tab_sch_am4)
confusionMatrix(tab_sch_am5)
confusionMatrix(tab_sch_am6)