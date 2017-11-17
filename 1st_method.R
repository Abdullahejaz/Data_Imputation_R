#This script is for predictive mean matching method of MICE package

install.packages("missForest")
install.packages("hydroGOF")
install.packages("mice")
install.packages("VIM")
install.packages("xtable")
library(missForest)
library(hydroGOF)
library(mice)
library(VIM)
library(xtable)

dataset<- (iris.StringsAsFactor=FALSE)
summary(iris)


#producing 2%, 5%, 10%, 15%, 20%, 25% missing values
iris.mis1<- prodNA(iris, noNA =0.02)
iris.mis2<- prodNA(iris, noNA =0.05)
iris.mis3<- prodNA(iris, noNA =0.1)
iris.mis4<- prodNA(iris, noNA =0.15)
iris.mis5<- prodNA(iris, noNA =0.20)
iris.mis6<- prodNA(iris, noNA =0.25)

#check for the missing values are included in the dataset
summary(iris.mis)

#tabular form of the missing values present in each variable
md.pattern(iris.mis)

#for visualizing the dataset VIM package

mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                   labels=names(iris.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


#imputing the missing values.
imputedd1<- mice(iris.mis1, m=5, maxit =10, method= 'pmm', seed=500)
imputedd2<- mice(iris.mis2, m=5, maxit =10, method= 'pmm', seed=500)
imputedd3<- mice(iris.mis3, m=5, maxit =10, method= 'pmm', seed=500)
imputedd4<- mice(iris.mis4, m=5, maxit =10, method= 'pmm', seed=500)
imputedd5<- mice(iris.mis5, m=5, maxit =10, method= 'pmm', seed=500)
imputedd6<- mice(iris.mis6, m=5, maxit =10, method= 'pmm', seed=500)
summary(imputedd)

imputedd$imp$Sepal.Width
completeData1<- complete(imputedd1,1)
completeData2<- complete(imputedd2,1)
completeData3<- complete(imputedd3,1)
completeData4<- complete(imputedd4,1)
completeData5<- complete(imputedd5,1)
completeData6<- complete(imputedd6,1)


rmse1<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)
rmse2<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)
rmse3<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)
rmse4<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)
rmse5<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)
rmse6<- rmse(completeData[,-5], iris[,-5], na.rm = TRUE)

rmse1
rmse2
rmse3
rmse4
rmse5
rmse6

rr <- c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6)
per_col <- c(2,5,10,15,20,25)
rmse_df111 <- data.frame(percentage = per_col,error = rr);
barplot(rmse_df111$error,  ylab = "RMSE", main="RMSE distribution of missing data (2 to 25%)")

#For k-NN
normalize<- function(x) {
  return((x-min(x)) / (max(x)- min(x)))
}

iris_normal<- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))

completeData_normal1<- as.data.frame(lapply(completeData1[,c(1,2,3,4)], normalize))
completeData_normal2<- as.data.frame(lapply(completeData2[,c(1,2,3,4)], normalize))
completeData_normal3<- as.data.frame(lapply(completeData3[,c(1,2,3,4)], normalize))
completeData_normal4<- as.data.frame(lapply(completeData4[,c(1,2,3,4)], normalize))
completeData_normal5<- as.data.frame(lapply(completeData5[,c(1,2,3,4)], normalize))
completeData_normal6<- as.data.frame(lapply(completeData6[,c(1,2,3,4)], normalize))

iris_train <- iris_normal[1:150,]
iris_train_target <- iris[1:150, 5]
iris_test_target <- iris[1:150, 5]

iris_test1 <- completeData_normal1[1:150,]
iris_test2 <- completeData_normal2[1:150,]
iris_test3 <- completeData_normal3[1:150,]
iris_test4 <- completeData_normal4[1:150,]
iris_test5 <- completeData_normal5[1:150,]
iris_test6 <- completeData_normal6[1:150,]

require(class)

#building the model using knn
model1<- knn(train=iris_train, test= iris_test1, cl=iris_train_target, k=13)
model2<- knn(train=iris_train, test= iris_test2, cl=iris_train_target, k=13)
model3<- knn(train=iris_train, test= iris_test3, cl=iris_train_target, k=13)
model4<- knn(train=iris_train, test= iris_test4, cl=iris_train_target, k=13)
model5<- knn(train=iris_train, test= iris_test5, cl=iris_train_target, k=13)
model6<- knn(train=iris_train, test= iris_test6, cl=iris_train_target, k=13)

tab1<- table(iris_test_target, model1)
tab2<- table(iris_test_target, model2)
tab3<- table(iris_test_target, model3)
tab4<- table(iris_test_target, model4)
tab5<- table(iris_test_target, model5)
tab6<- table(iris_test_target, model6)

tab1
tab2
tab3
tab4
tab5
tab6

confusionMatrix(tab1)
confusionMatrix(tab2)
confusionMatrix(tab3)
confusionMatrix(tab4)
confusionMatrix(tab5)
confusionMatrix(tab6)
