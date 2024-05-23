library(shiny)
source("mod-data.R")


library(readxl)
library(tidyverse)

data <- read_excel("Student Feedback (Semester 1 2024) (Responses).xlsx", sheet = "Form Responses 1")



ui  <- tagList(
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    "SAP Report Generator",
    tabPanel("Navbar 1",
             sidebarPanel(
               fileInput("file", "File input:"),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Tab 2", "This panel is intentionally left blank"),
                 tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
    ),
    
    
    tabPanel("Data", 
             
       dataUploadUI("data_upload")
       
             
    ),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)


# ui <- fluidPage(
#   chartUI(id = "chart1"),
#   test_ui(id = "test1")
# )


server <- function(input, output, session) {
  
  
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