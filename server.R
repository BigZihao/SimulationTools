
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

  
  contents <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$ImportPattern
    
    if(is.null(inFile))
      return(NULL)
    
    a= read.csv(inFile$datapath, header = FALSE, sep=",")
    return(a)
  })
  

  dataInputTV <- reactive({
    
    TV = round(runif(input$Year*input$Brand*input$Product*52,min=as.numeric(input$rangemin),
                     max=as.numeric(input$rangemax)),2)
    
    TVP = rep(1,input$Year*input$Brand*input$Product*52)

    if(is.null(contents())==FALSE){
      TVP = contents()
      TVP = rep(as.vector(as.numeric(TVP)),input$Year*input$Brand*input$Product)
    }
    TVP = as.numeric(TVP)
    
    TV=TV*TVP
    TV = data.frame(TV)
    names(TV) = paste(input$Activityname,"1")
    
    for(i in c(2:(input$variablen))){
      TV[paste(input$Activityname,i)] = round(runif(input$Year*input$Brand*input$Product*52,min=as.numeric(input$rangemin),
                                                                      max=as.numeric(input$rangemax)),2)*TVP}
    
    return(data.frame(TV))
  })
  
  output$plotTV <-  renderPlotly({
    time1=input$Year*52
    ggplotly(data.frame( date = dmy("8/5/2013") + weeks(1:time1),TV=dataInputTV()[1:time1,1]) %>% ggplot(aes(date,TV)) +geom_bar( stat = 'identity')+ylim(c(0,as.numeric(input$rangemax))))
  }
  )
  
  dataInputTVRT <- reactive({
    
    TV = dataInputTV()
    retentionrate = as.numeric(input$TVRT)
    retention = retentionrate^(c(1,2,3,4,5,6,7,8,9,10))
    TVRT = TV
    for(j in 1:dim(TVRT)[2]){
    for(i in 1:(dim(TVRT)[1]-10)){TVRT[i:(i+9),j]=TVRT[i:(i+9),j]+as.numeric(TVRT[i,j])*retention}}
    TVRT = data.frame(round(TVRT,2))
    return(data.frame(TVRT))
  })
  
  
  dataInput <- reactive({
    
    seed=rnorm(1,5,100)
    set.seed(seed)
    a=generatedata1(time1=input$Year,
                            dim1=input$Brand,
                            dim2=input$Product)
    test=a

    
    ## Canni
    #Cannibalization=test$Y[test$Brand==levels(test$Brand)[1]]
   # test$Y[test$Brand==levels(test$Brand)[2]]=test$Y[test$Brand==levels(test$Brand)[2]]-input$Cannibalization*Cannibalization
  #  test$Y[test$Brand==levels(test$Brand)[1]]=test$Y[test$Brand==levels(test$Brand)[1]]+input$Cannibalization*Cannibalization
 
    ## Halo
   # Halo1=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]
   # Halo2=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]
    
   # test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[2]]+ input$Halo*Halo1
   # test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]=test$Y[test$Brand==levels(test$Brand)[1] & test$Product==levels(test$Product)[1]]+ input$Halo*Halo2
    
    
    return(test)
    
  })
  
  output$plot1 <-  renderPlotly({
    ggplotly(dataInput() %>% ggplot(aes(date,Y,colour=Product)) +geom_line()+facet_grid(Brand~.))
  }
  )
  
  
  dataInputcategory <-reactive({

    
    rate = 0.02
    category = 20*rep(rate*(1:(52*input$Year)),input$Brand*input$Product)
    
    test = dataInput()
    
    test = data.frame(test,category=category)
    
    
    
    if(input$Seasonal==T){
      test$seasonal=0
      r=5
      test$seasonal[test$month<=7]=r*test$month[test$month<=7]
      test$seasonal[test$month>7]=7*r-r*(test$month[test$month>7]-7)
      plot(test$date,test$seasonal)
      test$category=test$category+test$seasonal
      
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
      test$category=test$category+20*test$holiday
    }
    
    
    
    test = data.frame(test)
    test$Totalcategory = test$category 
    test$category =test$category * input$Marketshare
    test$Y = test$Y + test$category
    test = test[,which(names(test) %in% c("date","Brand","Product","Y","Base","category","Totalcategory"))]

    return(test)
    
  })

  
  output$plotcategory <-  renderPlotly({
    ggplotly(dataInputcategory() %>% group_by(date) %>% summarise(category = sum(category), 
                                                                  Totalcategory =sum(Totalcategory)) %>%
               melt(id = "date") %>%
             ggplot(aes(date,value,colour= variable)) +geom_line())
  }
  )
  
  
  values <- reactiveValues(default = 0)
  
  observeEvent(input$goButton,{
    values$default <- input$goButton

  })
  
  
  
  ntext <- eventReactive(input$goButton, {

    TV=dataInputTV()
    TVRT = dataInputTVRT()
    names(TV) = input$Activityname
    Yimpact <- round(as.numeric(input$Elasticity)*apply(TVRT,1,sum),2)
    usename = c(paste(input$Activityname,1:input$variablen,sep=""))
    if( values$default==0){final <<- dataInputcategory()}
    
    return(list(data.frame(TV),data.frame(Yimpact),usename))

  })
  

  output$table <- DT::renderDataTable(DT::datatable({

    
    if( values$default==0){final <<- dataInputcategory()}
    else{
      justname = names(final)
      final <<- data.frame(final,data.frame(ntext()[1]))
      names(final) <<- c(justname,c(ntext()[[3]]))
      final$Y <<- as.numeric(final$Y) + as.numeric(unlist(ntext()[[2]]))
    }
    final
    
    
  }))

  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$Projectname,'SimulatedData', '.csv', sep='') },
    content = function(file) {
      write.csv(final, file, row.names = FALSE,na="NA")
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


