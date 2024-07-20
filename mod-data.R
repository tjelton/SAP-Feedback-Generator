library(formattable)
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(stringr)
library(bsicons)
library(shinyWidgets)

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
           HTML('<hr style="border: 0; border-top: 2px solid #232324; margin: 10px 0 15px 0;">'),
           
           # Info for how to upload the file and upload button.
           fluidRow(
             
             # Information
             column(8,
                card(
                  height = 270,
                  HTML("
                    <p>File upload requirements:<br>
                    <ul>
                      <li>Only excel files (extension .xlsx) are currently supported.</li>
                      <li>The excel file must have the survey data on a tab named \"Form Responses 1\".</li>
                      <li>The first row of the spreadsheet must contain the column names, and each row underneath represents the responses from
                      a single person.</li>
                      <li>More file upload types coming soon...</li>
                      <li>Once uploaded, the file contents will be displayed in the table below. This is the original data as you uploaded it (it will
                      not contains the updates that you make in the following stages).</li>
                    </ul></p>"
                  ),
                )
             ),
             
             # Upload
             column(4,
                br(),
                br(),
                card(
                  HTML("<p><b><u>File Input</u></b></p>"),
                  fileInput(ns("data_file"), ""),
                  style = "background:#cce4fc" # Light blue colour
                )
             )
             
           ),
           
           conditionalPanel(
             
             condition = paste0("output['", ns('data_uploaded_flag'), "']"),
             h5("Uploaded Data:"),
             textOutput("character_tooltip_message"),
             DT::dataTableOutput(ns("original_data_table"))
             
           ),
           

           
                 
        ),
        
        ### Page to start cleaning the data ###
        # Option to select a variable that appears in the data set.
        # Then can change the classification of the variable and do some data filtering/cleaning.
        tabPanel("Step 2) Data Cleaning",
           HTML('<hr style="border: 0; border-top: 2px solid #232324; margin: 10px 0 15px 0;">'),
                 
           HTML("<br>"),
           p("Some information here on data cleaning... Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce pharetra et nibh ac varius. Quisque dapibus consectetur ex. Fusce sit amet dui erat. Sed tellus elit, tempor vel egestas a, viverra nec erat. Nullam nec felis posuere, faucibus justo eu, luctus eros. Cras consequat mauris sed ante lacinia, ut lobortis lorem dignissim. Nunc elementum rhoncus ex, nec pulvinar ex ullamcorper et. Praesent sodales, lorem nec fermentum pretium, tortor purus vestibulum arcu, eu egestas turpis est nec dui."),
           HTML("<br>"),
           
           fluidRow(
             
             # Side bar with data cleaning button choices
             column(4,
                card(
                  HTML("<p><b><u><center>Cleaning Options</center></u></b></p>"),
                  
                  uiOutput(outputId = ns("variable_names_button")),
                  uiOutput(outputId = ns("keep_data_column_toggle")),
                  uiOutput(outputId = ns("column_data_type")),
                  uiOutput(outputId = ns("numeric_filter")),
                  uiOutput(outputId = ns("set_as_categorical")),
                  
                  
                  style = "background:#cce4fc" # Light blue colour
                )
             ),
             
             # Column Output
             column(8,
                DTOutput(outputId = ns("column_output"))
             )
             
           ),
           
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
      if(is.null(input$data_file)) return(NULL)
      req(input$data_file)  # Ensure a file is uploaded
      infile <- input$data_file
      data <- read_excel(infile$datapath, sheet = "Form Responses 1")
      
      # Change NA values in character columns to the empty string.
      data <- data %>%
        mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))
      
      return(data)
    })
    
    # Data store for data that can be modified.
    modified_data <- reactive({
      req(original_data())
      return(original_data())
    })
    
    # Flag for when data has been uploaded.
    output$data_uploaded_flag <- reactive({
      val <- !(is.null(input$data_file))
    })
    # To make the flag output reactive to be accessed from the ui.
    outputOptions(output, 'data_uploaded_flag', suspendWhenHidden=FALSE)
    
    # Render the original uploaded data as a table.
    output$original_data_table <- renderDT({
      req(original_data())
      
      # Find all columns that are character type.
      data = original_data()
      character_type_column_indices = c()
      for (i in seq_along(data)) {
        if (is.character(data[[i]])) {
          character_type_column_indices = c(character_type_column_indices, i)
        }
      }
      
      datatable(data, options = list(scrollX = TRUE, columnDefs = list(list(
        targets = character_type_column_indices,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 100 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
          "}")
      ))), callback = JS('table.page(3).draw(false);'))
    })
    
    # Cleaned data.
    # Data frame where we will be saving the changes of the data cleaning.
    # This is to retain a copy of the original uncleaned data.
    cleaned_data <- reactiveVal()
    observe({
      req(original_data())
      cleaned_data(original_data())
    })
    
    # Data cleaning options data frame:
    # Essentially, this is a data frame where the first column is all of the different columns/questions in the original
    # data set. The next few rows will correspond to the input buttons provided in the data cleaning phase. This data construct exists 
    # for two main reasons; (1) to make it easier to quickly process data later on, (2) because we are reactively creating this input,
    # and we want a way to save the data states.
    data_cleaning_input_options <- reactiveVal()
    observe({
      # Check that data has been uploaded.
      req(original_data())
      
      # Collect all questions into a single column
      df = data.frame(questions = colnames(original_data()))
      
      data_cleaning_input_options(df)
    })
    
    # Create select button when the data is uploaded.
    # Select button allows user to choose the data column they wish to manipulate.
    # Widget gallery: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
    output$variable_names_button <- renderUI({
      
      # Check that data has been uploaded.
      req(cleaned_data())
      
      # Button to choose the different variables.
      options = colnames(cleaned_data())
      
      # Button prelude + tooltip.
      text <- span(
        "Select Column",
        tooltip(
          bs_icon("info-circle"),
          "From the drop-down, select the survey question (data column) you would like to data clean next.",
          placement = "right"
        )
      )
      
      input_btn = selectInput(
        ns("column_select"), 
        label = NULL,
        choices = options
      )
      
      return(
        tagList(
          text,
          input_btn
        )
      )
      
    })
    
    # Button to indicate whether to include this column or remove from later analysis.
    # If the user selects yes to removing this column from the remainder of the anlaysis, all other options will dissapear.
    output$keep_data_column_toggle <- renderUI({
      
      req(cleaned_data())

      # Button prelude + tooltip.
      text <- span(
        "Exclude from data analysis:",
        tooltip(
          bs_icon("info-circle"),
          "Activate the switch if you wish to exclude the selected question/data column from further analysis. This is
            useful if you want to minmise the columns you are analysing in later stages.",
          placement = "right"
        )
      )
      
      # Switch.
      switch = materialSwitch(
        inputId = ns("exclude_toggle"), 
        label = NULL,
        status = "danger"
      )

      return(
        tagList(
          text,
          switch
        )
      )
      
    })

    # Button to change the data type of the selected column.
    output$column_data_type <- renderUI({
      
      req(cleaned_data())
      req(input$column_select)

      # When the exclude column button is activated, hide all other input options.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        return(NULL)
      }
      
      # Check that there are less that 10 unique strings/elements.
      # Arbitrary filter here as later graphics can become quite confusing with many different categories.
      categories_theshold <- 10
      unique_count <- length(unique(cleaned_data()[[input$column_select]]))
      choices_ls = list("Numeric" = 1, "Sentence" = 2)
      if (unique_count < categories_theshold) {
        choices_ls = append(choices_ls, list("Categorical" = 3))
      }
      
      button = radioButtons(
        ns("column_data_type"),
        label = "Data Type",
        choices = choices_ls,
        selected = 1
      )
      return(button)
    })
    
    # Output to see if the user wishes to treat this column/variable as categorical.
    # Recommend to the user not to store as categorical if the variable is a float or there are many categories.
    output$set_as_categorical <- renderUI({
      
      req(cleaned_data())
      req(input$column_select)
      
      data = cleaned_data()[[input$column_select]]
      
      # Check that there are less that 10 unique strings/elements.
      # Arbitrary filter here as later graphics can become quite confusing with many different categories.
      recommended_string = TRUE
      categories_theshold <- 10
      unique_count <- length(unique(data))
      if (unique_count > categories_theshold) {
        recommended_string = FALSE
      }

      # Set label for the button
      label = "Treat as Categorical"
      if (recommended_string == FALSE) {
        label = paste0(label, "(not recommended for theis variable):")
      }

      # Button prelude + tooltip.
      text <- span(
        label,
        tooltip(
          bs_icon("info-circle"),
          "Activate the switch if you wish to treat this variable as a categorical variable somewhere in your analysis.
          Note that numeric data can be jointly considered as both numeric and categorical.
          This option is not recommended where it would result in greater than 10 categories.",
          placement = "right"
        )
      )
      
      # Switch.
      switch = switchInput(
        inputId = ns("categorical_toggle"), 
        label = NULL
      )
      
      return(
        tagList(
          text,
          switch
        )
      )

    })

    
    output$numeric_filter <- renderUI({
      
      req(cleaned_data())
      req(input$column_select)
      req(input$column_data_type)
      
      if (input$column_data_type == 3) {
        # Temporarily convert the data to a numeric value to find the min and max value.
        temp_data = cleaned_data() %>%
          select(input$column_select) %>%
          mutate(temp = as.numeric(input$column_select))
        
        
      }
      
      return(NULL)
      
      
    })
    
    

    # Display data for the selected variable in the cleaning phase.
    output$column_output <- renderDT({
      
      req(cleaned_data())
      req(input$column_select)
      
      # Only extract the column that was selected by the user.
      data = cleaned_data() %>%
        select(input$column_select)
      
      dt = datatable(data, 
                     filter="none",
                     options = list(lengthChange = FALSE),
                     rownames = TRUE,
                     editable = TRUE)
      
      return(dt)
    })
    
  })
}

