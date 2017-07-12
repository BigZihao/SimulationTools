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
  
  

  tabPanel("Dimension Setup",
           
           sidebarPanel( 
             textInput("Projectname", label = h3("Project Name"), value = "Project"),
             numericInput("Brand",
                                       "Brand:", 2,
                                       min = 2, max = 6),
                          numericInput("Product",
                                       "Product:", 3,
                                       min = 2, max = 8),
                          numericInput("Year",
                                       "Year:", 3,
                                       min = 2, max = 8)
                       #  sliderInput("Cannibalization", label = h3("Cannibalization"), min = 0, max = 1, value = 0.1),
                        # sliderInput("Halo", label = h3("Halo"), min = 0, max = 1, value = 0.1)
                       ),
           mainPanel(
             fluidRow(
             
               plotlyOutput('plot1')
             )
             
           )
           
     
  ),
  
  
  
  tabPanel("Category Variable",
           
           sidebarPanel( checkboxInput("Seasonal", "Seasonal", value = FALSE, width = NULL),
                         checkboxInput("Holiday","Holiday", value = FALSE, width = NULL),
                        sliderInput("Marketshare", label = h3("Market Share"), min = 0, max = 1, value = 0.1)
                         # sliderInput("Halo", label = h3("Halo"), min = 0, max = 1, value = 0.1)
           ),
           mainPanel(
             fluidRow(
               
               plotlyOutput('plotcategory')
             )
             
           )
           
           
           
           
           
           
  ),

  
  
  tabPanel("Marketing Activities",
           
           sidebarPanel(
             textInput("Activityname", label = h3("Activity Measure Name"), value = "TV"),
             textInput("variablen", label = h3("Variable Numbers"), value = "2"),
             textInput("rangemin", label = h3("Range Min"), value = "100"),
             textInput("rangemax", label = h3("Range Max"), value = "250"),
             #sliderInput("TV", label = h3("TV GRP"), min = 100, max = 250, value = c(120,150)),
             sliderInput("TVRT", label = h3("retention rate"), min = 0, max = 1, value = 0),
             h3("Flghting Pattern"),
             #checkboxInput("Pattern","Pattern 1", value = FALSE, width = NULL),
               fileInput('ImportPattern', 'Import Pattern',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
             textInput("Elasticity", label = h3("Elasticity"), value = "1"),
             textInput("ROI", label = h3("ROI"), value = "1"),
             br(),
             actionButton("goButton", "Go!"),
             p("Click the button to update the variable in simulation data")
      
           ),
           
           mainPanel(
             fluidRow(
               plotlyOutput('plotTV'),
               h3("Marketing activities"),
               textOutput("text1")
               
             )
             
           )
  ),

  
  
  tabPanel("Data Format and Download",

           mainPanel(
             fluidRow(
               h3("Download Data"),
               downloadButton('downloadData', 'Download'),
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


