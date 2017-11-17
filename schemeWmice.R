
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

imputed_scheme_pmm1<- mice(nn1, m=5, maxit =10, method= 'pmm', seed=500)
imputed_scheme_pmm2<- mice(nn2, m=5, maxit =10, method= 'pmm', seed=500)
imputed_scheme_pmm3<- mice(nn3, m=5, maxit =10, method= 'pmm', seed=500)
imputed_scheme_pmm4<- mice(nn4, m=5, maxit =10, method= 'pmm', seed=500)
imputed_scheme_pmm5<- mice(nn5, m=5, maxit =10, method= 'pmm', seed=500)
imputed_scheme_pmm6<- mice(nn6, m=5, maxit =10, method= 'pmm', seed=500)


completeData_sch_pmm1<- complete(imputed_scheme_pmm1,1)
completeData_sch_pmm2<- complete(imputed_scheme_pmm2,1)
completeData_sch_pmm3<- complete(imputed_scheme_pmm3,1)
completeData_sch_pmm4<- complete(imputed_scheme_pmm4,1)
completeData_sch_pmm5<- complete(imputed_scheme_pmm5,1)
completeData_sch_pmm6<- complete(imputed_scheme_pmm6,1)


rmse_pmmscheme1<- rmse(completeData_sch_pmm1[,-5], iris[,-5], na.rm = TRUE)
rmse_pmmscheme2<- rmse(completeData_sch_pmm2[,-5], iris[,-5], na.rm = TRUE)
rmse_pmmscheme3<- rmse(completeData_sch_pmm3[,-5], iris[,-5], na.rm = TRUE)
rmse_pmmscheme4<- rmse(completeData_sch_pmm4[,-5], iris[,-5], na.rm = TRUE)
rmse_pmmscheme5<- rmse(completeData_sch_pmm5[,-5], iris[,-5], na.rm = TRUE)
rmse_pmmscheme6<- rmse(completeData_sch_pmm6[,-5], iris[,-5], na.rm = TRUE)

rmse_pmmscheme1
rmse_pmmscheme2
rmse_pmmscheme3
rmse_pmmscheme4
rmse_pmmscheme5
rmse_pmmscheme6


rrrr <- c(rmse_pmmscheme1, rmse_pmmscheme2, rmse_pmmscheme3, rmse_pmmscheme4, rmse_pmmscheme5, rmse_pmmscheme6)
per_col <- c(2,5,10,15,20,25)
rmse_df11 <- data.frame(percentage = per_col,error = rrrr);
barplot(rmse_df11$error,  ylab = "RMSE", main="RMSE distribution of missing data (2 to 25%)")

#For k-NN
normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))
#str(iris_normal)
#summary(iris_normal)

completeData_sch_pmm1_normal<- as.data.frame(lapply(completeData_sch_pmm1[,c(1,2,3,4)], normalize))
completeData_sch_pmm2_normal<- as.data.frame(lapply(completeData_sch_pmm2[,c(1,2,3,4)], normalize))
completeData_sch_pmm3_normal<- as.data.frame(lapply(completeData_sch_pmm3[,c(1,2,3,4)], normalize))
completeData_sch_pmm4_normal<- as.data.frame(lapply(completeData_sch_pmm4[,c(1,2,3,4)], normalize))
completeData_sch_pmm5_normal<- as.data.frame(lapply(completeData_sch_pmm5[,c(1,2,3,4)], normalize))
completeData_sch_pmm6_normal<- as.data.frame(lapply(completeData_sch_pmm6[,c(1,2,3,4)], normalize))

iris_train_sch_pmm <- iris_normal[1:150,]
iris_train_target_sch_pmm <- iris[1:150, 5]
iris_test_target_sch_pmm <- iris[1:150, 5]

iris_test_sch_pmm1 <- completeData_sch_pmm1_normal[1:150,]
iris_test_sch_pmm2 <- completeData_sch_pmm2_normal[1:150,]
iris_test_sch_pmm3 <- completeData_sch_pmm3_normal[1:150,]
iris_test_sch_pmm4 <- completeData_sch_pmm4_normal[1:150,]
iris_test_sch_pmm5 <- completeData_sch_pmm5_normal[1:150,]
iris_test_sch_pmm6 <- completeData_sch_pmm6_normal[1:150,]

require(class)

#building the model using knn
model_sch_pmm1<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm1, cl=iris_train_target_sch_pmm, k=13)
model_sch_pmm2<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm2, cl=iris_train_target_sch_pmm, k=13)
model_sch_pmm3<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm3, cl=iris_train_target_sch_pmm, k=13)
model_sch_pmm4<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm4, cl=iris_train_target_sch_pmm, k=13)
model_sch_pmm5<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm5, cl=iris_train_target_sch_pmm, k=13)
model_sch_pmm6<- knn(train=iris_train_sch_pmm, test= iris_test_sch_pmm6, cl=iris_train_target_sch_pmm, k=13)

tab_sch_pmm1<- table(iris_test_target_sch_pmm, model_sch_pmm1)
tab_sch_pmm2<- table(iris_test_target_sch_pmm, model_sch_pmm2)
tab_sch_pmm3<- table(iris_test_target_sch_pmm, model_sch_pmm3)
tab_sch_pmm4<- table(iris_test_target_sch_pmm, model_sch_pmm4)
tab_sch_pmm5<- table(iris_test_target_sch_pmm, model_sch_pmm5)
tab_sch_pmm6<- table(iris_test_target_sch_pmm, model_sch_pmm6)

tab_sch_pmm1
tab_sch_pmm2
tab_sch_pmm3
tab_sch_pmm4
tab_sch_pmm5
tab_sch_pmm6

confusionMatrix(tab_sch_pmm1)
confusionMatrix(tab_sch_pmm2)
confusionMatrix(tab_sch_pmm3)
confusionMatrix(tab_sch_pmm4)
confusionMatrix(tab_sch_pmm5)
confusionMatrix(tab_sch_pmm6)
