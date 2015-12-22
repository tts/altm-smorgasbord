# Altmetric data viz with a Shiny app

library(shiny)
library(rCharts)
library(reshape)



shinyUI(pageWithSidebar(
  
  headerPanel(paste("Some raw (alt)metrics data of Aalto University publications 2007-2012", " (N=", nrow(aalto_all_m), ")", sep = "")),
  
  sidebarPanel(
    
    h6(textOutput("caption")),
    
    selectInput(inputId = "m", 
                label = "Choose Top10 metrics:",
                choices = allmetrics,
                selected = c("Web of Science")),

    checkboxGroupInput(inputId = "vars",
                       label = "Select what to show:",
                       choices = allmetrics,
                       selected = c("Web of Science",
                                    "Mendeley (ALT)"))
            
  ),
  
  mainPanel(
    
#    HTML("<style>
#           .impactstory-embed.impactstory-small{
#             width: 400px;
#           }
#         </style>"),  

   htmlOutput("table"),
    
   br(),
   
   showOutput("chart", "nvd3")
    
    
  )
  
))
