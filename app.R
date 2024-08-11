library(shiny)

source("mod_data_ui.R")
source("mod_data_srv.R")

options(shiny.autoreload = TRUE) 

library(readxl)
library(tidyverse)
library(DT)
library(bslib)


data <- read_excel("Student Feedback (Semester 1 2024) (Responses).xlsx", sheet = "Form Responses 1")

ui  <- tagList(
  navbarPage(
    
    theme = bs_theme(version = 5, bootswatch = "zephyr"),
    
    # Dashboard title.
    "SAP Report Generator",
    
    tabPanel(
      # Tab Title
      HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Page 1</h5>'),

      "Home page"
    ),
    
    
    tabPanel(
      # Tab Title
      HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Upload Data</h5>'),
             
       dataUploadUI("data_upload")
       
             
    ),
    tabPanel(
      
      # Tab Title
      HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Page 3</h5>'),
             
             
    "This panel is intentionally left blank")
  )
)


# ui <- fluidPage(
#   chartUI(id = "chart1"),
#   test_ui(id = "test1")
# )


server <- function(input, output, session) {
  
  # Uncomment to see the bs_themer()
  #bs_themer() #Flatly, #Spacelab, #Zephyr
  
  dataUploadServer(id = "data_upload")

  
  
  
  # chartServer(
  #   id = "chart1",
  #   x = c("Q1", "Q2", "Q3", "Q4"),
  #   y = c(505.21, 397.18, 591.44, 674.90),
  #   title = "Sales in 000 for 2023"
  # )
  # 
  # test_server(
  #   id = "test1",
  #   data = data
  # )
  
}


shinyApp(ui = ui, server = server)