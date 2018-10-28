options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard,
               tableHTML,
               dplyr,
               data.table,
               Boruta,
               h2o,
               plotly,
               ggplot2,
               ggpubr,
               GGally,
               h2o,
               lime)

shinyUI(
  
  fluidPage(
    
    tags$style(HTML("
                    .tabs-above > .nav > li[class=active] > a {
                    background-color: #ff4500;
                    color: #698B22;
                    }")),
          
    #the below chunk of code changes the look of the sliderinput
    tags$style(make_css(list('.irs-bar',
                             c('border-top', 'border-bottom', 'background'),
                             rep('#0147FA', 3)),
                        list('.irs-bar-edge',
                             c('background', 'border'),
                             c('#0147FA', '100px !important')),
                        list('.irs-single',
                             'background',
                             'black'))),
    
    tags$style(make_css(list('.irs-bar',
                             c('border-top', 'border-bottom', 'background'),
                             rep('red', 3)),
                        list('.irs-bar-edge',
                             c('background', 'border'),
                             c('red', '0px !important')),
                        list('.irs-single',
                             'background',
                             'red'))),
    
    titlePanel(
      
      # h2("Low cost interactive diagnosis of Valvular Heart Disease",
      #    style = "color: #CD2626")
      
      fluidRow(
        
        column(10, 
               h1("miniBaymax - A low cost interactive diagonostic tool 
                  for Valvular Heart Disease detection",
                  style = "color: #ff3f34")),
        
        column(1, 
               img(height = 75, 
                   width = 200, 
                   src = "baymax.jpg")
        )
        
      )),
    
    sidebarLayout(
      
      sidebarPanel(
        tags$style(".well {background-color:#88ACE0;}"),
        
        width = 3,
        
        helpText(h5("Fill in your parameter values to diagnose VHD",
                    style = "color: #000000")),
        
        
        sliderInput("Pulse.rate",
                    "Pulse rate",
                    min = 40,
                    max = 120,
                    value = 50,
                    step = 1),
        
        sliderInput("EF.TTE",
                    "Ejection fraction(%)",
                    min = 10,
                    max = 70,
                    value = 40,
                    step = 1),
        
        selectInput("Lung.rales",
                    "Lung rales",
                    choices = c("Yes" = "Yes",
                                "No" = "No")),
        
        selectInput("Systolic.Murmur",
                    "Systolic murmur",
                    choices = c("Yes" = "Yes",
                                "No" = "No")),
        
        selectInput("Diastolic.Murmur",
                    "Diastolic murmur",
                    choices = c("Yes" = "Yes",
                                "No" = "No")),
        
        selectInput("Dyspnea",
                    "Dyspnea",
                    choices = c("Yes" = "Yes",
                                "No" = "No")),
        
        selectInput("Family.history",
                    "Family history",
                    choices = c("0" = "0",
                                "1" = "1")),
        
        selectInput("Function.Class",
                    "Function class",
                    choices = c("0" = "0",
                                "1" = "1",
                                "2" = "2",
                                "3" = "3")),
        
        helpText(h5("This web application is created by Dr. Satyakama Paul.",
                    style = "color: #551A8B"))
      ),
      
      mainPanel(
        
        width = 9,
        
        tabsetPanel(
          type = "pills",
          navbarPage(
            h5("Tabs",
               align = "left",
               style = "color: black"),
            
            navbarMenu(
              h4("Why this Application is needed?",
                 align = "left",
                 style = "color: #000000"),
              
              tabPanel("Utility",
                       img(src='foreword.jpg', 
                           align = "centre",
                           width="1200",
                           height="700"))
            ),
            
            navbarMenu(
              h4("For previous patients",
                 align = "left",
                 style = "color: #551A8B"),
              
              tabPanel("First couple of rows",
                       dataTableOutput("o.RawData")),
              
              tabPanel("Distribution of continuous predictors",
                       plotlyOutput("o.featureGraphs_conti"), 
                       height = "2000px",
                       width = "2000px"),
              
              tabPanel("Distribution of discrete predictors",
                       plotlyOutput("o.featureGraphs_discreet"), 
                       height = "2000px",
                       width = "2000px")
              
            ),
            
            navbarMenu(
              h4("How good is the AI model",
                 align = "left",
                 style = "color: #660000"),
              
              tabPanel("Decision complexity",
                       plotlyOutput("o.para_coord"), 
                       height = "2000px",
                       width = "2000px"),
              
              tabPanel("Accuracy of prediction",
                       verbatimTextOutput("o.prediction_on_test")),
              
              tabPanel("Plots to undestand the model",
                       plotOutput("o.global_plots"))
            ),
            
            navbarMenu(
              h4("For the present patient",
                 align = "left",
                 style = "color: #008000"),
              
              tabPanel("Prediction",
                       verbatimTextOutput("o.final.pred"))
              
              
              
            ),
            
            navbarMenu(
              
              h4("Explanation of the decision",
                 align = "left",
                 style = "color: #ff0000"),
              
              tabPanel("Which variables support and don't",
                       plotOutput("o.local.plot"))
              
            )
          )
        )
      )
    )
      )
    )
