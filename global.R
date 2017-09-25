library(magrittr)
library(shiny)
library(dplyr)
library(ggplot2)
library(gam)
library(mgcv)
library(splines)
library(plotly)
library(rbokeh)
library(reshape2)
require(lubridate)
require(dygraphs)
require(xts)


generatedata1=function(time1,dim1,dim2){

  time1=time1*52
  error=round(rnorm(time1*dim1*dim2,0,1),2)

  ##################  Add time trend    ############
  rate=0.5
  Base=50
  Y=error

  # Your starting date, plus 52 more dates at weekly intervals
  xDates <- dmy("8/5/2013") + weeks(1:time1)
  
  # A data frame of the dates, the month of the year, and the week of the month
  xYMW <- data.frame(date=(xDates), month=month(xDates))
  test=data.frame(Brand=rep(paste("B",(1:dim1)),each=dim2*time1),
                  Product=rep(rep(paste("P",(1:dim2)),each=time1),dim1),
                  date=rep((xDates),dim1*dim2), 
                  month=rep(month(xDates),dim1*dim2),
                  Y=Y,
                  error=error)
  test$Brand=as.factor(test$Brand)
  test$Product=as.factor(test$Product)
  return(test)
  
  }