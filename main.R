
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(shiny)



rm(list=ls(all=TRUE))



runApp('P:/SimulationTools',host="0.0.0.0",port=5050)








#install.packages('rsconnect')
library(rsconnect)
library(RcppArmadillo)
rsconnect::setAccountInfo(name='zihaozhangap', token='CCC977CDDB18ED55C68DBA2E664C2F6E', secret='/PqGH9VGjl0pqvOh6uOIHmQl3JB+lzGluzAv1Gez')
rsconnect::deployApp('P:/SimulationTools')







library(rgdal)
library(rgeos)
library(leaflet)
library(rmapshaper)
library(tidyverse)
library(lubridate)
library(zoo)
library(reshape2)
library(data.table)
library(stringr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(rgdal)
library(DT)
library(googleway)
library(ggmap)

