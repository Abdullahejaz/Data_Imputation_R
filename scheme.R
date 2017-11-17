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


summary(nn1)
summary(nn2)
summary(nn3)
summary(nn4)
summary(nn5)
summary(nn6)

