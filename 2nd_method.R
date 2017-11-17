#This script is for the second imputation method

install.packages("missForest")
install.packages("mice")
install.packages("VIM")
install.packages("Amelia")
library(mice)
library(missForest)
library(VIM)
library(Amelia)

dataset<- (iris)

#producing 2%, 5%, 10%, 15%, 20%, 25% missing values
iris.mis_am1<- prodNA(iris, noNA =0.02)
iris.mis_am2<- prodNA(iris, noNA =0.05)
iris.mis_am3<- prodNA(iris, noNA =0.1)
iris.mis_am4<- prodNA(iris, noNA =0.15)
iris.mis_am5<- prodNA(iris, noNA =0.20)
iris.mis_am6<- prodNA(iris, noNA =0.25)

#tabular form of the missing values present in each variable
md.pattern(iris.mis_am1)
md.pattern(iris.mis_am2)
md.pattern(iris.mis_am3)
md.pattern(iris.mis_am4)
md.pattern(iris.mis_am5)
md.pattern(iris.mis_am6)

#for visualizing the dataset
mice_plot <- aggr(iris.mis_am1, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis_am1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

amelia1 <- amelia(iris.mis_am1, m=1, parallel = "multicore", noms = "Species")
amelia2 <- amelia(iris.mis_am2, m=1, parallel = "multicore", noms = "Species")
amelia3 <- amelia(iris.mis_am3, m=1, parallel = "multicore", noms = "Species")
amelia4 <- amelia(iris.mis_am4, m=1, parallel = "multicore", noms = "Species")
amelia5 <- amelia(iris.mis_am5, m=1, parallel = "multicore", noms = "Species")
amelia6 <- amelia(iris.mis_am6, m=1, parallel = "multicore", noms = "Species")

#printing imputed values
completeData_am1<- amelia1$imputations[[1]]
completeData_am2<- amelia2$imputations[[1]]
completeData_am3<- amelia3$imputations[[1]]
completeData_am4<- amelia4$imputations[[1]]
completeData_am5<- amelia5$imputations[[1]]
completeData_am6<- amelia6$imputations[[1]]

rmse_am1<- rmse(completeData_am1[,-5], iris[,-5], na.rm = TRUE)
rmse_am2<- rmse(completeData_am2[,-5], iris[,-5], na.rm = TRUE)
rmse_am3<- rmse(completeData_am3[,-5], iris[,-5], na.rm = TRUE)
rmse_am4<- rmse(completeData_am4[,-5], iris[,-5], na.rm = TRUE)
rmse_am5<- rmse(completeData_am5[,-5], iris[,-5], na.rm = TRUE)
rmse_am6<- rmse(completeData_am6[,-5], iris[,-5], na.rm = TRUE)

rmse_am1
rmse_am2
rmse_am3
rmse_am4
rmse_am5
rmse_am6

normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))

completeData_am1_normal<- as.data.frame(lapply(completeData_am1[,c(1,2,3,4)], normalize))
completeData_am2_normal<- as.data.frame(lapply(completeData_am2[,c(1,2,3,4)], normalize))
completeData_am3_normal<- as.data.frame(lapply(completeData_am3[,c(1,2,3,4)], normalize))
completeData_am4_normal<- as.data.frame(lapply(completeData_am4[,c(1,2,3,4)], normalize))
completeData_am5_normal<- as.data.frame(lapply(completeData_am5[,c(1,2,3,4)], normalize))
completeData_am6_normal<- as.data.frame(lapply(completeData_am6[,c(1,2,3,4)], normalize))

iris_train_am <- iris_normal[1:150,]
iris_train_target_am <- iris[1:150, 5]
iris_test_target_am <- iris[1:150, 5]

iris_test_am1 <- completeData_am1_normal[1:150,]
iris_test_am2 <- completeData_am2_normal[1:150,]
iris_test_am3 <- completeData_am3_normal[1:150,]
iris_test_am4 <- completeData_am4_normal[1:150,]
iris_test_am5 <- completeData_am5_normal[1:150,]
iris_test_am6 <- completeData_am6_normal[1:150,]

require(class)

#building the model using knn
model_am1<- knn(train=iris_train_am, test= iris_test_am1, cl=iris_train_target_am, k=13)
model_am2<- knn(train=iris_train_am, test= iris_test_am2, cl=iris_train_target_am, k=13)
model_am3<- knn(train=iris_train_am, test= iris_test_am3, cl=iris_train_target_am, k=13)
model_am4<- knn(train=iris_train_am, test= iris_test_am4, cl=iris_train_target_am, k=13)
model_am5<- knn(train=iris_train_am, test= iris_test_am5, cl=iris_train_target_am, k=13)
model_am6<- knn(train=iris_train_am, test= iris_test_am6, cl=iris_train_target_am, k=13)

tab_am1<- table(iris_test_target_am, model_am1)
tab_am2<- table(iris_test_target_am, model_am2)
tab_am3<- table(iris_test_target_am, model_am3)
tab_am4<- table(iris_test_target_am, model_am4)
tab_am5<- table(iris_test_target_am, model_am5)
tab_am6<- table(iris_test_target_am, model_am6)

tab_am1
tab_am2
tab_am3
tab_am4
tab_am5
tab_am6

confusionMatrix(tab_am1)
confusionMatrix(tab_am2)
confusionMatrix(tab_am3)
confusionMatrix(tab_am4)
confusionMatrix(tab_am5)
confusionMatrix(tab_am6)


