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



# Define UI for application that draws a histogram
navbarPage(
  # theme = shinytheme("lumen"),
  "Simulation Tool",
  
  

  tabPanel("Simulate Data",
           
           sidebarPanel( numericInput("Brand",
                                       "Brand:", 2,
                                       min = 2, max = 6),
                          numericInput("Product",
                                       "Product:", 3,
                                       min = 2, max = 8),
                          numericInput("Year",
                                       "Year:", 5,
                                       min = 2, max = 8),
                         
                         
       
                          checkboxInput("Seasonal", "Seasonal", value = FALSE, width = NULL),
                          checkboxInput("Holiday","Holiday", value = FALSE, width = NULL),
                         sliderInput("Cannibalization", label = h3("Cannibalization"), min = 0, max = 1, value = 0.1),
                         sliderInput("Halo", label = h3("Halo"), min = 0, max = 1, value = 0.1)),
           
           mainPanel(
           fluidRow(
             h3("Dependent Trend"),
             plotlyOutput('plot1')
           )
     
           )
  ),
  
  
  tabPanel("Marketing Activities",
           
           sidebarPanel(
             sliderInput("TV", label = h3("TV"), min = 0, max = 100, value = c(20,50)),
             checkboxInput("Pattern","Monthly Pattern", value = FALSE, width = NULL),
             sliderInput("Promotion", label = h3("Promotion"), min = 0, max = 100, value = 50),
             sliderInput("Price", label = h3("Price"), min = 0, max = 100, value = 50)
           ),
           mainPanel(
             fluidRow(
               h3("TV"),
               plotlyOutput('plotTV')
             )
             
           )
  ),

  
  
  tabPanel("Data Format and Download",
           mainPanel(
             fluidRow(
               h3("Download Data"),
               downloadLink('downloadData', 'Download') ,
               DT::dataTableOutput("table")
             )
             
           )
  )
  
#  ,
  
# tabPanel("Categorical Variable Model",
#          mainPanel(
#                    fluidRow(
#                      h4("CV model Coefficients"),
#                      tableOutput("table1"),
#                      h4("True coefficients simulating the data"),
#                      tableOutput("table2")
#                    ))),
# 
# 
# tabPanel("Hierarchical Model",
#          mainPanel(h4("Hierarchical Model results might takes up to 1 minutes to update..."),
#                    fluidRow(
#                      h4("Coefficients Range Comparison"),
#                      plotlyOutput('plot2'),
#                      h4("Error Comparison"),
#                      plotlyOutput('plot3')
#                    )))
  
  
)


