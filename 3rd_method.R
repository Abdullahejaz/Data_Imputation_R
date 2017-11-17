install.packages("missForest")
library(missForest)
install.packages("hydroGOF")
library(hydroGOF)
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)
install.packages("mi")
library(mi)
library(xtable)

dataset<- iris

#producing 2%, 5%, 10%, 15%, 20%, 25% missing values
iris.mis_mf1<- prodNA(iris, noNA =0.02)
iris.mis_mf2<- prodNA(iris, noNA =0.05)
iris.mis_mf3<- prodNA(iris, noNA =0.1)
iris.mis_mf4<- prodNA(iris, noNA =0.15)
iris.mis_mf5<- prodNA(iris, noNA =0.2)
iris.mis_mf6<- prodNA(iris, noNA =0.25)


#tabular form of the missing values present in each variable
md.pattern(iris.mis_mf1)
md.pattern(iris.mis_mf2)
md.pattern(iris.mis_mf3)
md.pattern(iris.mis_mf4)
md.pattern(iris.mis_mf5)
md.pattern(iris.mis_mf6)

#impute missing values, using all parameters as default values
imputedSet_mf1 <- missForest(iris.mis_mf1)
imputedSet_mf2 <- missForest(iris.mis_mf2)
imputedSet_mf3 <- missForest(iris.mis_mf3)
imputedSet_mf4 <- missForest(iris.mis_mf4)
imputedSet_mf5 <- missForest(iris.mis_mf5)
imputedSet_mf6 <- missForest(iris.mis_mf6)

completeData_mf1<- imputedSet_mf1$ximp
completeData_mf2<- imputedSet_mf2$ximp
completeData_mf3<- imputedSet_mf3$ximp
completeData_mf4<- imputedSet_mf4$ximp
completeData_mf5<- imputedSet_mf5$ximp
completeData_mf6<- imputedSet_mf6$ximp

rmse_mf1<- rmse(completeData_mf1[,-5], iris[,-5], na.rm = TRUE)
rmse_mf2<- rmse(completeData_mf2[,-5], iris[,-5], na.rm = TRUE)
rmse_mf3<- rmse(completeData_mf3[,-5], iris[,-5], na.rm = TRUE)
rmse_mf4<- rmse(completeData_mf4[,-5], iris[,-5], na.rm = TRUE)
rmse_mf5<- rmse(completeData_mf5[,-5], iris[,-5], na.rm = TRUE)
rmse_mf6<- rmse(completeData_mf6[,-5], iris[,-5], na.rm = TRUE)

rmse_mf1
rmse_mf2
rmse_mf3
rmse_mf4
rmse_mf5
rmse_mf6

normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))

completeData_mf1_normal<- as.data.frame(lapply(completeData_mf1[,c(1,2,3,4)], normalize))
completeData_mf2_normal<- as.data.frame(lapply(completeData_mf2[,c(1,2,3,4)], normalize))
completeData_mf3_normal<- as.data.frame(lapply(completeData_mf3[,c(1,2,3,4)], normalize))
completeData_mf4_normal<- as.data.frame(lapply(completeData_mf4[,c(1,2,3,4)], normalize))
completeData_mf5_normal<- as.data.frame(lapply(completeData_mf5[,c(1,2,3,4)], normalize))
completeData_mf6_normal<- as.data.frame(lapply(completeData_mf6[,c(1,2,3,4)], normalize))


iris_train_mf <- iris_normal[1:150,]
iris_train_target_mf <- iris[1:150, 5]
iris_test_target_mf <- iris[1:150, 5]

iris_test_mf1 <- completeData_mf1_normal[1:150,]
iris_test_mf2 <- completeData_mf2_normal[1:150,]
iris_test_mf3 <- completeData_mf3_normal[1:150,]
iris_test_mf4 <- completeData_mf4_normal[1:150,]
iris_test_mf5 <- completeData_mf5_normal[1:150,]
iris_test_mf6 <- completeData_mf6_normal[1:150,]

require(class)

#building the model using knn
model_mf1<- knn(train=iris_train_mf, test= iris_test_mf1, cl=iris_train_target_mf, k=13)
model_mf2<- knn(train=iris_train_mf, test= iris_test_mf2, cl=iris_train_target_mf, k=13)
model_mf3<- knn(train=iris_train_mf, test= iris_test_mf3, cl=iris_train_target_mf, k=13)
model_mf4<- knn(train=iris_train_mf, test= iris_test_mf4, cl=iris_train_target_mf, k=13)
model_mf5<- knn(train=iris_train_mf, test= iris_test_mf5, cl=iris_train_target_mf, k=13)
model_mf6<- knn(train=iris_train_mf, test= iris_test_mf6, cl=iris_train_target_mf, k=13)

tab_mf1<- table(iris_test_target_mf, model_mf1)
tab_mf2<- table(iris_test_target_mf, model_mf2)
tab_mf3<- table(iris_test_target_mf, model_mf3)
tab_mf4<- table(iris_test_target_mf, model_mf4)
tab_mf5<- table(iris_test_target_mf, model_mf5)
tab_mf6<- table(iris_test_target_mf, model_mf6)

tab_mf1
tab_mf2
tab_mf3
tab_mf4
tab_mf5
tab_mf6

confusionMatrix(tab_mf1)
confusionMatrix(tab_mf2)
confusionMatrix(tab_mf3)
confusionMatrix(tab_mf4)
confusionMatrix(tab_mf5)
confusionMatrix(tab_mf6)
