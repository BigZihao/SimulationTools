
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
library(timeDate)
library(lubridate)
library(forecast)
library(ggplot2)
library(reshape2)
library(lme4)
# Define server logic required to draw a histogram

function(input, output){
  
  
  
  dataInputTV <- reactive({
    TV = round(runif(input$Year*input$Brand*input$Product*52,min=input$TV[1],max=input$TV[2]),2)
    if(input$Pattern==T){
      TVP=rep(c(1,0,0,0),input$Year*input$Brand*input$Product*52/4)
      TV=TV*TVP
    }
    return(TV)
  })
  
  output$plotTV <-  renderPlotly({
    time1=input$Year*52
    ggplotly(data.frame( date = dmy("8/5/2013") + weeks(1:time1),TV=dataInputTV()[1:time1]) %>% ggplot(aes(date,TV)) +geom_bar(position = 'stack', stat = 'identity')+ylim(c(0,100)))
  }
  )
  
  
  dataInput <- reactive({
    
    seed=rnorm(1,5,100)
    set.seed(seed)
    a=generatedata1(time1=input$Year,
                            dim1=input$Brand,
                            dim2=input$Product)
    test=a
    
    if(input$Seasonal==T){
      test$seasonal=0
      r=10
      test$seasonal[test$month<=7]=r*test$month[test$month<=7]
      test$seasonal[test$month>7]=7*r-r*(test$month[test$month>7]-7)
      plot(test$date,test$seasonal)
      test$Y=test$Y+test$seasonal
      
    }
    if(input$Holiday==T){
      plusWeekends<-function(h){
        h<-as.Date(h)
        return(c(h-2,h-1,h,h+1,h+2))
      }

      holidays <- c(plusWeekends(USLaborDay(2013:(2013+input$Year))),
                  #  plusWeekends(USThanksgivingDay(2013:(2013+input$Year))),
                 #   plusWeekends(USMemorialDay(2013:(2013+input$Year))),
                    plusWeekends(USNewYearsDay(2013:(2013+input$Year))),
                    plusWeekends(USChristmasDay(2013:(2013+input$Year))))
      
      test$holiday=as.numeric(test$date %in% holidays)
      test$Y=test$Y+50*test$holiday
    }
    
    ## Canni
    Cannibalization=test$Y[test$Brand==levels(test$Brand)[1]]
    test$Y[test$Brand==levels(test$Brand)[2]]=test$Y[test$Brand==levels(test$Brand)[2]]-input$Cannibalization*Cannibalization
    test$Y[test$Brand==levels(test$Brand)[1]]=test$Y[test$Brand==levels(test$Brand)[1]]+input$Cannibalization*Cannibalization
 
    ## Halo
    Halo1=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]
    Halo2=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]
    
    test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]+ input$Halo*Halo1
    test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]+ input$Halo*Halo2
    
    
    return(test)
    
  })
  
  output$plot1 <-  renderPlotly({
    ggplotly(dataInput() %>% ggplot(aes(date,Y,colour=Product)) +geom_line()+facet_grid(Brand~.))
  }
  )
  
  dataInput2 <- reactive({
    X=dataInput()
    TV=dataInputTV()
    X$Y=X$Y+0.1*TV
    final=data.frame(X,TV)
    return(final)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    dataInput2()
  }))
  

  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("SimulatedData", '.csv', sep='') },
    content = function(file) {
      write.csv(dataInput2(), file)
    }
  )
  
  # 
  # Model1 <- reactive({
  #   test=dataInput()
  #   test$Brand=as.factor(test$Brand)
  #   test$Prod=as.factor(test$Prod)
  #   ptm <- proc.time()
  #   lm2=lm(Y~Brand:Product:(X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+Intercept)-1,test)
  #   proc.time() - ptm
  #   coefficient=data.frame(test %>% group_by(Product,Brand) %>% summarise(n()),matrix(round(coef(lm2),2),length(levels(test$Brand))*length(levels(test$Product)),10+1,byrow = F))
  #   coefficient=coefficient[order(coefficient$Brand),]
  #   return(coefficient[,-dim(coefficient)[2]])
  # })
  # 
  # 

  
  # output$table1 <- renderTable({  
  #   Model1()
  # })
  # 
  # output$table2 <- renderTable({  
  # test=dataInput()[[1]]
  # true=dataInput()[[2]]
  # true=data.frame(test %>% group_by(Product,Brand) %>% summarise(n()),true)
  # true=true[order(true$Brand),-dim(true)[2]]
  # true
  # })
  # 
  # 
  # Model2 <- reactive({
  #   
  #   test=dataInput()[[1]]
  #   test$Brand=as.factor(test$Brand)
  #   test$Prod=as.factor(test$Prod)
  #   
  #   ptm <- proc.time()
  #   lm3=lmer(Y~1+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+(1+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10|Brand:Product),test)
  #   proc.time() - ptm
  #   coeflmer=data.frame(test %>% group_by(Product,Brand) %>% summarise(n()),
  #                       matrix(unlist(coef(lm3))[-c(1:(length(levels(test$Brand))*length(levels(test$Product))))],length(levels(test$Brand))*length(levels(test$Product)),10))
  #   return(coeflmer)
  # })
  # 
  # output$table3 <- renderTable({  
  #   Model2()
  # })
  # 
  # output$plot2 <-  renderPlotly({
  #   test=dataInput()[[1]]
  #   true=dataInput()[[2]]
  #   true=data.frame(test %>% group_by(Product,Brand) %>% summarise(n()),true)
  #   true=true[order(true$Brand),-dim(true)[2]]
  #   plotdata=rbind(data.frame(true) %>% mutate(dimension=paste(Brand,Product)) %>% select(dimension,X1:X10) %>% melt(id="dimension") %>% mutate(Class="True"),
  #                  data.frame(Model1())  %>% mutate(dimension=paste(Brand,Product)) %>% select(dimension,X1:X10) %>% melt(id="dimension") %>% mutate(Class="CV"),
  #                  data.frame(Model2())  %>% mutate(dimension=paste(Brand,Product)) %>% select(dimension,X1:X10) %>% melt(id="dimension") %>% mutate(Class="Hierarchical"))
  # 
  #   ggplotly(plotdata %>% ggplot(aes(variable,value,colour=Class)) + geom_point())
  # }
  # )
  # 
  # 
  # output$plot3 <-  renderPlotly({
  # test=dataInput()[[1]]
  # true=dataInput()[[2]]
  # true=data.frame(test %>% group_by(Product,Brand) %>% summarise(n()),true)
  # true=true[order(true$Brand),-dim(true)[2]]
  # 
  # 
  # plotdata2=data.frame(Model=c(rep("CV",10),
  #                              rep("Hierarchical",10)),
  #                      Variable=rep(names(test %>% select(X1:X10)),2),
  #                      Error=c(apply(abs(Model1() %>% select(X1:X10)- true%>% select(X1:X10)),2,sum),
  #                              apply(abs(Model2() %>% select(X1:X10)- true%>% select(X1:X10)),2,sum)))
  # 
  # plotdata2$Model=as.factor(plotdata2$Model)
  # plotdata2$Variable=factor(plotdata2$Variable,levels=names(test %>% select(X1:X10)))
  # ggplotly(plotdata2 %>% ggplot(aes(Variable,Error,group=Model,fill=Model)) + geom_bar(stat = "identity",position="dodge"))
  # 
  # }
  # )
  # 
  
}


