library(magrittr)
library(shiny)
library(dplyr)
library(ggplot2)
library(gam)
library(car)
library(mgcv)
library(splines)
library(plotly)
library(rbokeh)
library(effects)
library(reshape2)
require(lubridate)


generatedata=function(time1,dim1,dim2){
  
  time1=time1*52
  driver=10
  X=matrix(rnorm(time1*dim1*dim2*driver,1,10),ncol=driver)
  
  B=c(1,2,3,4,5,6,7,8,9,10)
  true=matrix(0,dim1*dim2,length(B)+1,byrow = T)
  R = matrix(0.1,nrow=length(B),ncol=length(B))+diag(0.9,length(B), length(B))
  U = t(chol(R))
  nvars = dim(U)[1]
  X = U %*% t(X)
  newX = t(X)
  X = as.matrix(newX)
  
  
  ##################  Add time trend    ############
  rate=0.5
  X[,10]=X[,10]+rep(rate*(1:time1),dim1*dim2)
  
  
  Y=NULL
  Base=11
  k=1
  for(i in c(1:dim1))
  {
    B1=B+0.1*i
    Base1=Base+0.1*i
    for(j in c(1:dim2))
    {
      B2=B1+0.01*j
      Base2=Base1+0.01*j
      true[k,]=c(B2,Base2)
      k=k+1
      X1=X[c((1:time1)+time1*dim2*(i-1)+time1*(j-1)),]
      Y1= X1%*% B2 
      Y1=Y1+Base+rnorm(length(Y1),0,1)
      Y=c(Y,Y1)
    }
  }
  
  
  # Your starting date, plus 52 more dates at weekly intervals
  xDates <- dmy("8/3/2013") + weeks(1:time1)
  
  # A data frame of the dates, the month of the year, and the week of the month
  xYMW <- data.frame(date=(xDates), month=month(xDates))
  test=data.frame(Brand=rep(paste("B",(1:dim1)),each=dim2*time1),
                  Product=rep(rep(paste("P",(1:dim2)),each=time1),dim1),
                  date=rep((xDates),dim1*dim2), month=rep(month(xDates),dim1*dim2),
                  Intercept=rep(1,length(Y)),
                  Y,
                  X,
                  Base=rep(true[,11],each=time1))
  test$Brand=as.factor(test$Brand)
  test$Product=as.factor(test$Product)
  test %>% group_by(Brand,Product) %>% summarize(n())
  return(list(test,true))
}