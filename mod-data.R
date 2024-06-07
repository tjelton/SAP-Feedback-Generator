library(formattable)
library(shiny)
library(tidyverse)
library(DT)
library(bslib)



dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(
      HTML("<center>"),
      h4("Step 1: Data Upload"),
      HTML("<br>"),
      p("General information here... Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce pharetra et nibh ac varius. Quisque dapibus consectetur ex. Fusce sit amet dui erat. Sed tellus elit, tempor vel egestas a, viverra nec erat. Nullam nec felis posuere, faucibus justo eu, luctus eros. Cras consequat mauris sed ante lacinia, ut lobortis lorem dignissim. Nunc elementum rhoncus ex, nec pulvinar ex ullamcorper et. Praesent sodales, lorem nec fermentum pretium, tortor purus vestibulum arcu, eu egestas turpis est nec dui."),
      HTML("</center>"),
      style = "color:black; background:#c8cacc"
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "navpill_styles.css")
    ),
    
    card(
      
      tabsetPanel(
        
        type = "pills",
        
        ### Page to upload the data. ###
        # This page will include a file input button.
        # When the file is uploaded, a data table viewer of the original data.
        tabPanel("Step 1) Data Upload",
                 
                 HTML("<br>"),
                 p("Some information will be placed here about how to upload the data set. Possible more instructions... Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce pharetra et nibh ac varius. Quisque dapibus consectetur ex. Fusce sit amet dui erat. Sed tellus elit, tempor vel egestas a, viverra nec erat. Nullam nec felis posuere, faucibus justo eu, luctus eros. Cras consequat mauris sed ante lacinia, ut lobortis lorem dignissim. Nunc elementum rhoncus ex, nec pulvinar ex ullamcorper et. Praesent sodales, lorem nec fermentum pretium, tortor purus vestibulum arcu, eu egestas turpis est nec dui."),
                 HTML("<br>"),
                 sidebarPanel(
                   fileInput(ns("file"), "File input:")
                 ),
                 mainPanel(
                   tableOutput(ns("table"))
                 )
                 
        ),
        
        ### Page to start cleaning the data ###
        # Option to select a variable that appears in the data set.
        # Then can change the classification of the variable and do some data filtering/cleaning.
        tabPanel("Step 2) Data Cleaning",
                 HTML("<br>"),
                 p("Some information here on data cleaning... Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce pharetra et nibh ac varius. Quisque dapibus consectetur ex. Fusce sit amet dui erat. Sed tellus elit, tempor vel egestas a, viverra nec erat. Nullam nec felis posuere, faucibus justo eu, luctus eros. Cras consequat mauris sed ante lacinia, ut lobortis lorem dignissim. Nunc elementum rhoncus ex, nec pulvinar ex ullamcorper et. Praesent sodales, lorem nec fermentum pretium, tortor purus vestibulum arcu, eu egestas turpis est nec dui."),
                 HTML("<br>"),
                 
                 
                 fluidRow(
                   
                   # Side bar with buttons.
                   column(4, 
                          h3("Column 1"),
                          p("This is column 1."),
                          bslib::card(
                            card_header("Card header"),
                            "Card body"
                          )
                   ),
                   
                   
                   column(8, 
                          h3("Column 2"),
                          p("This is column 2")
                   ),
                   
                 ),
                 
                 # sidebarPanel(
                 #   uiOutput(outputId = ns("variable_names_button")),
                 #   uiOutput(outputId = ns("column_data_type"))
                 # ),
                 # mainPanel(
                 #   DTOutput(outputId = ns("column_output"))
                 # )
        )
      ),
      
      style = "color:black; background:#FFFFFF"
    ),


  )
}


dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to read the uploaded file
    original_data <- reactive({
      req(input$file)  # Ensure a file is uploaded
      infile <- input$file
      data <- read_excel(infile$datapath, sheet = "Form Responses 1")
      return(data)
    })
    
    # Data sore for data that can be modified.
    modified_data <- reactive({
      req(original_data())
      return(original_data())
    })
    
    # Render the processed data as a table
    output$table <- renderTable({
      req(original_data())  # Ensure processed data is available
      original_data()
    })
    
    # Create button when the data is uploaded.
    # Widget gallery: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
    output$variable_names_button <- renderUI({
      
      # Check that data has been set/processed.
      req(original_data())
      
      # Button to choose the different variables.
      options = colnames(original_data())
      input_btn = selectInput(
        ns("column_select"), 
        label = "Question Selection", 
        choices = options
      )
     
      return(input_btn)
    })
    
    
    # Button for column data type.
    output$column_data_type <- renderUI({
      
      req(modified_data())
      req(input$column_select)
      
      radioButtons(
        ns("column_data_type"),
        label = "Data Type",
        choices = list("Numeric" = 1, "Sentence" = 2),
        selected = 1
      )
      
    })
    
    
    
    # Display data for the selected variable.
    output$column_output <- renderDT({
      
      req(modified_data())
      req(input$column_select)
      
      # Only extract the column that was selected by the user.
      data = modified_data() %>%
        select(input$column_select)
      
      
      return(datatable(data, filter="top",options = list(lengthChange = FALSE)))
    })
    
  })
}

