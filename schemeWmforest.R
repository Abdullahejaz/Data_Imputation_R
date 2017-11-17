
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

#imputing missing values using missForest

imputed_sch_mf1 <- missForest(nn1)
imputed_sch_mf2 <- missForest(nn2)
imputed_sch_mf3 <- missForest(nn3)
imputed_sch_mf4 <- missForest(nn4)
imputed_sch_mf5 <- missForest(nn5)
imputed_sch_mf6 <- missForest(nn6)

completeData_sch_mf1<- imputed_sch_mf1$ximp
completeData_sch_mf2<- imputed_sch_mf2$ximp
completeData_sch_mf3<- imputed_sch_mf3$ximp
completeData_sch_mf4<- imputed_sch_mf4$ximp
completeData_sch_mf5<- imputed_sch_mf5$ximp
completeData_sch_mf6<- imputed_sch_mf6$ximp

rmse_sch_mf1<- rmse(completeData_sch_mf1[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_mf2<- rmse(completeData_sch_mf2[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_mf3<- rmse(completeData_sch_mf3[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_mf4<- rmse(completeData_sch_mf4[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_mf5<- rmse(completeData_sch_mf5[,-5], iris[,-5], na.rm = TRUE)
rmse_sch_mf6<- rmse(completeData_sch_mf6[,-5], iris[,-5], na.rm = TRUE)

rmse_sch_mf1
rmse_sch_mf2
rmse_sch_mf3
rmse_sch_mf4
rmse_sch_mf5
rmse_sch_mf6

tot_rmse <- c(rmse_sch_mf1, rmse_sch_mf2, rmse_sch_mf3, rmse_sch_mf4, rmse_sch_mf5, rmse_sch_mf6)
per_col <- c(2,5,10,15,20,25)
rmse_df1 <- data.frame(percentage = per_col,error = tot_rmse);
barplot(rmse_df1$error,  ylab = "RMSE", main="RMSE distribution of missing data (2 to 25%)")

#For k-NN
normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))

completeData_sch_mf1_normal<- as.data.frame(lapply(completeData_sch_mf1[,c(1,2,3,4)], normalize))
completeData_sch_mf2_normal<- as.data.frame(lapply(completeData_sch_mf2[,c(1,2,3,4)], normalize))
completeData_sch_mf3_normal<- as.data.frame(lapply(completeData_sch_mf3[,c(1,2,3,4)], normalize))
completeData_sch_mf4_normal<- as.data.frame(lapply(completeData_sch_mf4[,c(1,2,3,4)], normalize))
completeData_sch_mf5_normal<- as.data.frame(lapply(completeData_sch_mf5[,c(1,2,3,4)], normalize))
completeData_sch_mf6_normal<- as.data.frame(lapply(completeData_sch_mf6[,c(1,2,3,4)], normalize))

iris_train_sch_mf <- iris_normal[1:150,]
iris_train_target_sch_mf <- iris[1:150, 5]
iris_test_target_sch_mf <- iris[1:150, 5]

iris_test_sch_mf1 <- completeData_sch_mf1_normal[1:150,]
iris_test_sch_mf2 <- completeData_sch_mf2_normal[1:150,]
iris_test_sch_mf3 <- completeData_sch_mf3_normal[1:150,]
iris_test_sch_mf4 <- completeData_sch_mf4_normal[1:150,]
iris_test_sch_mf5 <- completeData_sch_mf5_normal[1:150,]
iris_test_sch_mf6 <- completeData_sch_mf6_normal[1:150,]

require(class)

#building the model using knn
model_sch_mf1<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf1, cl=iris_train_target_sch_mf, k=13)
model_sch_mf2<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf2, cl=iris_train_target_sch_mf, k=13)
model_sch_mf3<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf3, cl=iris_train_target_sch_mf, k=13)
model_sch_mf4<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf4, cl=iris_train_target_sch_mf, k=13)
model_sch_mf5<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf5, cl=iris_train_target_sch_mf, k=13)
model_sch_mf6<- knn(train=iris_train_sch_mf, test= iris_test_sch_mf6, cl=iris_train_target_sch_mf, k=13)

tab_sch_mf1<- table(iris_test_target_sch_mf, model_sch_mf1)
tab_sch_mf2<- table(iris_test_target_sch_mf, model_sch_mf2)
tab_sch_mf3<- table(iris_test_target_sch_mf, model_sch_mf3)
tab_sch_mf4<- table(iris_test_target_sch_mf, model_sch_mf4)
tab_sch_mf5<- table(iris_test_target_sch_mf, model_sch_mf5)
tab_sch_mf6<- table(iris_test_target_sch_mf, model_sch_mf6)

tab_sch_mf1
tab_sch_mf2
tab_sch_mf3
tab_sch_mf4
tab_sch_mf5
tab_sch_mf6

confusionMatrix(tab_sch_mf1)
confusionMatrix(tab_sch_mf2)
confusionMatrix(tab_sch_mf3)
confusionMatrix(tab_sch_mf4)
confusionMatrix(tab_sch_mf5)
confusionMatrix(tab_sch_mf6)


