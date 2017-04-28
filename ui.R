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
                         downloadLink('downloadData', 'Download')),
           
           mainPanel(
           fluidRow(
             h3("Dependent Trend"),
             plotlyOutput('plot1'),
             DT::dataTableOutput("table")
           )
     
           )
  ),
  

tabPanel("Categorical Variable Model",
         mainPanel(
                   fluidRow(
                     h4("CV model Coefficients"),
                     tableOutput("table1"),
                     h4("True coefficients simulating the data"),
                     tableOutput("table2")
                   ))),


tabPanel("Hierarchical Model",
         mainPanel(h4("Hierarchical Model results might takes up to 1 minutes to update..."),
                   fluidRow(
                     h4("Coefficients Range Comparison"),
                     plotlyOutput('plot2'),
                     h4("Error Comparison"),
                     plotlyOutput('plot3')
                   )))
  
  
)


