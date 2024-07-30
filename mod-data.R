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
                 HTML("<br>"),
                 
                 # Info for how to upload the file and upload button.
                 fluidRow(
                   
                   # Information
                   column(8,
                          card(
                            height = 270,
                            HTML("
                              <p><b>File upload requirements:</b><br>
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
                            #fileInput(ns("data_file"), ""),
                            style = "background:#cce4fc" # Light blue colour
                          )
                   )
                   
                 ),
                 
                 conditionalPanel(
                     condition = paste0("output['", ns('data_uploaded_flag'), "']"),
                     navset_underline(
                       nav_panel(
                          "Uploaded Data",
                          HTML("<br>"),
                          textOutput("character_tooltip_message"),
                          DT::dataTableOutput(ns("original_data_table"))
                       )
                     ),
                 ),
        ),
        
        ### Page to start cleaning the data ###
        # Option to select a variable that appears in the data set.
        # Then can change the classification of the variable and do some data filtering/cleaning.
        tabPanel("Step 2) Data Cleaning",
                 HTML('<hr style="border: 0; border-top: 2px solid #232324; margin: 10px 0 15px 0;">'),
                 HTML("<br>"),
                 
                 fluidRow(
                   
                   # Info 
                   column(6,
                        card(
                            height = 270,
                            HTML("
                                <p><b>Data Cleaning Steps:</b><br>
                                   <ul>
                                     <li>For each column, go through and ensure that you are happy with the automatics data classifications.</li>
                                     <li>Once satisfied, ensure to click the green \"Save + Appply\" button, even if you made no manual changes.</li>
                                     <li>There are more advanced data cleaning options that you can choose.</li>
                                     <li>Don't worry if you make a mistake! You can reset a given column back to the data that your originally uploaded.</li>
                                   </ul>
                                </p>"),
                        ),   
                   ),
                   
                   # Columns that need to be cleaned still.
                   column(6,
                      uiOutput(outputId = ns("need_cleaning_card")),
                   ),
                   
                 ),
                 
                 # Deliberately left blank.
                 # Reason for inclusion: formats nicely, and looks consistent with other data cleaning pages.
                 navset_underline(
                   nav_panel(
                     "Cleaning Controls",
                     HTML("<br>"),
                   )
                 ),
                 
                 fluidRow(
                   
                   # Side bar with data cleaning button choices
                   column(4,
                          card(
                            HTML("<p><b><u><center>Cleaning Options</center></u></b></p>"),
                            
                            uiOutput(outputId = ns("variable_names_button")),
                            uiOutput(outputId = ns("keep_data_column_toggle")),
                            uiOutput(outputId = ns("column_data_type")),
                            uiOutput(outputId = ns("set_as_categorical")),
                            uiOutput(outputId = ns("numeric_filter")),
                            uiOutput(outputId = ns("edit_data_table_toggle")),
                            
                            # Button to save and apply the data changes.
                            actionButton(
                              ns("save_data_changes"), 
                              "Save + Apply",
                              class = "btn-success"
                            ) %>% 
                              tooltip("Click to save the data cleaning changes."),
                            
                            # Button to reset the current data column back to the original data.
                            actionButton(
                              ns("reset_changes"),
                              "Reset",
                              class = "btn-danger"
                            ) %>%
                              tooltip("Click to reset the current selected data column to the original data"),    
                            
                            style = "background:#cce4fc" # Light blue colour
                          )
                   ),
                   
                   # Column Output
                   column(8,
                          DTOutput(outputId = ns("column_output"))
                   )
                 ),
        ),
        
        # Page for the user to check that they are happy with the data cleaning before progressing to the analysis stage.
        # The user will have to press a check mark to indicate that they are happy before continuing.
        tabPanel("Step 3) Finalise Data",
           HTML('<hr style="border: 0; border-top: 2px solid #232324; margin: 10px 0 15px 0;">'),
           HTML("<br>"),
           
           fluidRow(
               # Instructions 
               column(8,
                    card(
                      height = 270,
                      HTML("
                        <p><b>Finalise Data Cleaning:</b><br>
                           <ul>
                             <li>Point 1</li>
                             <li>Point 2</li>
                             <li>Point 3</li>
                             <li>Point 4</li>
                           </ul>
                        </p>"),
                    ),   
               ),
               
               # Button to finalise decision.
               column(4,
                    card(
                      HTML("<p><b><u>Button</u></b></p>"),
                      style = "background:#cce4fc" # Light blue colour
                    )     
               ),
           ),
           
           # Cleaned data table display.
           conditionalPanel(
               condition = paste0("output['", ns('data_uploaded_flag'), "']"),
               
               navset_underline(
                 
                 nav_panel(
                   "Cleaned Data",
                   HTML("<br>"),
                   DT::dataTableOutput(ns("cleaned_data_table"))
                 ),
                 
                 nav_panel(
                   "Data Classifications",
                   HTML("<br>"),
                   h5("TEST"),
                 ),
                 
               ),

           ),
        ),
        
      ),

      style = "color:black; background:#FFFFFF"
    ),
    
   
    
  )
}

convert_non_numeric_to_character <- function(column) {
  if (is.numeric(column) || is.character(column)) {
    return(column)
  } else {
    return(as.character(column))
  }
}

dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to read the uploaded file
    original_data <- reactive({
      # if(is.null(input$data_file)) return(NULL)
      # req(input$data_file)  # Ensure a file is uploaded
      # infile <- input$data_file
      #data <- read_excel(infile$datapath, sheet = "Form Responses 1")
      data <- read_excel("Student Feedback (Semester 1 2024) (Responses).xlsx", sheet = "Form Responses 1")
      
      data <- data %>%
        # Change NA values in character columns to the empty string.
        mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))) %>%
        
        # Convert all not standard data types (for columns) to character.
        mutate(across(everything(), convert_non_numeric_to_character))
      
      return(data)
    })
    
    # Data store for data that can be modified.
    modified_data <- reactive({
      req(original_data())
      return(original_data())
    })
    
    # Flag for when data has been uploaded.
    output$data_uploaded_flag <- reactive({
      #val <- !(is.null(input$data_file)) <FIX WHEN NOT TESTING ANYMORE>
      val <- TRUE
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
      
      data_original <- original_data()
      
      # Collect all questions into a single column.
      df = data.frame(questions = colnames(data_original))
      
      # Add columns corresponding to the different cleaning options.
      df = df %>%
        mutate(
          cleaned = FALSE,
          exclude_from_analysis = FALSE,
          data_type = 1,
          treat_as_categorical = FALSE,
          filter_min_value = NA,
          filter_max_value = NA,
          edit_data_table = FALSE,
        )
      
      # If the data type is numeric in the original uploaded data, reflect this here.
      for (question in df$questions) {
        if (is.numeric(data_original[[question]])) {
          df$data_type[df$questions == question] <- 2 # 2 represents numeric
        }
      }
      
      data_cleaning_input_options(df)
    })
    
    # Create select button when the data is uploaded.
    # Select button allows user to choose the data column they wish to manipulate.
    output$variable_names_button <- renderUI({
      
      # Check that data has been uploaded.
      req(original_data())
      
      # Button to choose the different variables.
      options = colnames(original_data())
      
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
      
      req(input$column_select)
      
      current_value <- data_cleaning_input_options()$exclude_from_analysis[data_cleaning_input_options()$questions == input$column_select]
      
      # Button prelude + tooltip.
      text <- span(
        "Exclude From Data Analysis",
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
        status = "danger",
        value = current_value
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
      
      req(input$column_select)
      
      # When the exclude column button is activated, hide all other input options.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        return(NULL)
      }
      
      current_value <- data_cleaning_input_options()$data_type[data_cleaning_input_options()$questions == input$column_select]

      # Button prelude + tooltip.
      text <- span(
        "Data Type",
        tooltip(
          bs_icon("info-circle"),
          "Select whether your data is numeric or contains words (sentences). This will effect the types of analysis you can
          perform on this variable.",
          placement = "right"
        )
      )
      
      # Temporarily convert data to numeric to see if numeric is a valid data type for this variable.
      # We will claim that it is "potentially" valid if greater than 5% of the data is numeric.
      col_name <- input$column_select
      original_num_rows = nrow(cleaned_data())
      temporary <- cleaned_data() %>%
        mutate(!!sym(col_name) := as.numeric(!!sym(col_name))) %>%
        select(col_name)
      na_count <- sum(is.na(temporary))
      original_num_rows = nrow(cleaned_data())
      
      choices_radio = list("Sentence" = 1, "Numeric" = 2)
      # See if greater than 80% is numeric
      if (na_count / original_num_rows >= 0.8) {
        choices_radio = list("Sentence" = 1)
        current_value = 1
      }
      
      button = radioButtons(
        ns("column_data_type"),
        label = NULL,
        choices = choices_radio,
        selected = current_value
      )
      
      return(
        tagList(
          text,
          button
        )
      )
    })
    
    # Output to see if the user wishes to treat this column/variable as categorical.
    # Recommend to the user not to store as categorical if the variable is a float or there are many categories.
    output$set_as_categorical <- renderUI({
      
      req(input$column_select)
      
      # When the exclude column button is activated, hide all other input options.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        return(NULL)
      }
      
      current_value <- data_cleaning_input_options()$treat_as_categorical[data_cleaning_input_options()$questions == input$column_select]

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
        label = paste0(label, " (not recommended for this variable)")
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
        label = NULL,
        value = current_value
      )
      
      return(
        tagList(
          text,
          switch,
        )
      )
      
    })
    
    # If the data has been classified as numeric, create slider to allow the values to be filtered.
    output$numeric_filter <- renderUI({
      
      req(input$column_select)
      req(input$column_data_type)
      
      # When the exclude column button is activated, hide all other input options.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        return(NULL)
      }
      
      # The integer 2 represents the numeric data type.
      if (input$column_data_type == 2) {
        
        # Get current min and max values
        min_slider_value <- data_cleaning_input_options()$filter_min_value[data_cleaning_input_options()$questions == input$column_select]
        max_slider_value <- data_cleaning_input_options()$filter_max_value[data_cleaning_input_options()$questions == input$column_select]
        
        # Temporarily convert the data to a numeric value to find the min and max value.
        col_name = input$column_select 
        temp_data = cleaned_data() %>%
          select(col_name) %>%
          mutate(!!sym(col_name) := as.numeric(!!sym(col_name)))
        min_value = min(temp_data %>% select(col_name), na.rm = TRUE)
        max_value = max(temp_data %>% select(col_name), na.rm = TRUE)
        
        # Check if the min and max sliders values have been set previously (they are not NA).
        # If they are NA, set to default values.
        if (is.na(min_slider_value)) {
          min_slider_value = min_value
        }
        if (is.na(max_slider_value)) {
          max_slider_value = max_value
        }
        
        # Button prelude + tooltip.
        text <- span(
          "Filter Values",
          tooltip(
            bs_icon("info-circle"),
            "Adjust the slider to filter out numeric values that do not lie withing the slider's range.",
            placement = "right"
          )
        )
        
        # Slider input button.
        slider <- sliderInput(
          ns("numeric_filter_slider"),
          NULL,
          min = min_value, 
          max = max_value,
          value = c(min_slider_value, max_slider_value)
        )
        
        return(
          tagList(
            text,
            slider
          )
        )
        
        # Return nothing if variable is not classified as numeric.
      } else {
        return(NULL)
      }
    })
    
    # Toggle which when switched on, allows the user to be able to edit the data table.
    output$edit_data_table_toggle <- renderUI({
      
      req(input$column_select)
      
      # When the exclude column button is activated, hide all other input options.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        return(NULL)
      }
      
      current_value <- data_cleaning_input_options()$edit_data_table[data_cleaning_input_options()$questions == input$column_select]
      
      # Button prelude + tooltip.
      text <- span(
        "Edit Data Table Values",
        tooltip(
          bs_icon("info-circle"),
          "When activated, you can double click the value of a particular row to enable edit access to it.
          This is helpful for simple data cleaning.",
          placement = "right"
        )
      )
      
      # Switch.
      switch = switchInput(
        inputId = ns("edit_data_table_toggle"), 
        label = NULL,
        value = current_value
      )
      
      return(
        tagList(
          text,
          switch
        )
      )
    })
    
    # Get data for the selected variable in the cleaning phase.
    current_data_column <- reactiveVal()
    observe({
      req(cleaned_data())
      req(input$column_select)
      
      column = cleaned_data() %>%
        select(input$column_select)
      
      current_data_column(column)
    })
    
    # Display data for the selected variable in the cleaning phase.
    output$column_output <- renderDT({
      
      req(input$column_select)
      
      flag = FALSE
      # If the edit data table toggle is activated, we want the data table to be put in edit mode.
      if (!is.null(input$edit_data_table_toggle) && input$edit_data_table_toggle == TRUE) {
        flag = TRUE
      } 
      
      dt = datatable(
        current_data_column(), 
        filter="none",
        options = list(lengthChange = FALSE),
        rownames = FALSE,
        editable = flag
      )
      
      return(dt)
    })
    
    # Observe the cell edit event
    observeEvent(input$column_output_cell_edit, {
      table_edit = input$column_output_cell_edit
      value = table_edit$value
      column_data = current_data_column()

      # If the data type is numeric, you can only except numeric edits.
      if (is.numeric(column_data[[1]])) {
        value = suppressWarnings(as.numeric(table_edit$value))
      } 
      
      column_data[table_edit$row, 1] = value
      current_data_column(column_data)

    })
    
    # When Save + Apply button is clicked:
    # 1) update the data_cleaning_input_options df to reflect the changes for the given column selection.
    # 2) update the data column to have the steps set in the filter applied.
    observeEvent(input$save_data_changes, {
      
      ########## Update data_cleaning_input_options df.
      # Update fields for given column.
      original_input_options <- data_cleaning_input_options()
      
      # Set to true as data has been cleaned by the user.
      original_input_options$cleaned[original_input_options$questions == input$column_select] <- TRUE
      # Check if the exclude toggle button has been activated.
      if (!is.null(input$exclude_toggle) && input$exclude_toggle == TRUE) {
        original_input_options$exclude_from_analysis[original_input_options$questions == input$column_select] <- TRUE
        # No need to update other columns.
        data_cleaning_input_options(original_input_options)
        return()
      }
      
      original_input_options$exclude_from_analysis[original_input_options$questions == input$column_select] <- FALSE
      original_input_options$data_type[original_input_options$questions == input$column_select] <- input$column_data_type
      
      # Note: Unlike the exclude toggle (which is a materialSwitch), the set as categorical switch is different (it is a switchInput object).
      #       This type of switch is not set to NULL, just TRUE and FALSE (meaning there is no need for the complex if statment).
      original_input_options$treat_as_categorical[original_input_options$questions == input$column_select] <- input$categorical_toggle

      # Set the slider's min and max values (only if the data type is numeric).
      lower_bound = input$numeric_filter_slider[1]
      upper_bound = input$numeric_filter_slider[2]
      if (input$column_data_type == 2) {
        original_input_options$filter_min_value[original_input_options$questions == input$column_select] <- input$numeric_filter_slider[1]
        original_input_options$filter_max_value[original_input_options$questions == input$column_select] <- input$numeric_filter_slider[2]
      }
      
      original_input_options$edit_data_table[original_input_options$questions == input$column_select] <- input$edit_data_table_toggle
      
      # Save input options changes
      data_cleaning_input_options(original_input_options)
      
      ########## Update the data column.
      data_column <- current_data_column()
      data <- cleaned_data()
      data[[input$column_select]] = data_column[[1]]
      cleaned_data(data)
      
      # Change data type to fit with user choice.
      data_type <- input$column_data_type
      if (data_type == 1) {
        # 1 means character.
        data = data %>%
          mutate(!!sym(input$column_select) := as.character(!!sym(input$column_select)))
      } else {
        # 2 means numeric.
        data = data %>%
          mutate(!!sym(input$column_select) := as.numeric(!!sym(input$column_select))) %>%
          # Replace values outside the numeric range to be NA.
          mutate(!!sym(input$column_select) := ifelse(!!sym(input$column_select) < lower_bound | !!sym(input$column_select) > upper_bound, 
                                                     NA, !!sym(input$column_select)))

      }
      
      cleaned_data(data)
    })
    
    # When "Reset" button is clicked: set the data for the column to be the same as that originally input.
    observeEvent(input$reset_changes, {
      
      # Get original data.
      original_data <- original_data() %>%
        select(input$column_select)
      
      # If the original data was numeric, reset data cleaning options.
      if (is.numeric(original_data[[1]])) {
        temp = data_cleaning_input_options()
        temp$data_type[temp$questions == input$column_select] = 2
        temp$filter_min_value[temp$questions == input$column_select] = NA
        temp$filter_max_value[temp$questions == input$column_select] = NA
        data_cleaning_input_options(temp)
      }
      
      # Update data.
      data <- cleaned_data()
      data[[input$column_select]] = original_data[[1]]
      cleaned_data(data)
      
    })
    
    # Values box showing the rows that still need to be cleaned
    output$need_cleaning_card <- renderUI({
      
      req(data_cleaning_input_options)
      
      # Get the data columns that have not been cleaned yet.
      not_cleaned <- data_cleaning_input_options() %>%
        filter(cleaned == FALSE) %>%
        select(questions)
      not_cleaned <- not_cleaned[[1]]
      
      n_not_cleaned = length(not_cleaned)
      
      # If all columns cleaned, make card green with message indicating all columns are cleaned.
      if (n_not_cleaned == 0) {
        return(
          card(
            height = 270,
            HTML("<br><br><br><p><b><center>All columns have been cleaned!</center><b><p>"),
            style = "background:#9fff9c" # Light green colour
          )
        )
      }

      first_string = paste("<p>Currently, <b>", as.character(n_not_cleaned), " data columns</b> are <b>not cleaned</b>. These include:</p>")
      if (n_not_cleaned == 1) {
        first_string = "<p>Currently, <b>1 data column</b> is <b>not cleaned</b>. This is:</p>"
      }
      
      # Return the first five columns to the user as a list.
      second_string = "<p><ul>"
      counter = 1
      while (counter < 5 && counter <= n_not_cleaned) {
        second_string = paste(second_string, "<li>", not_cleaned[counter], "</li>")
        counter = counter + 1
      }
      # Add ellipses if there are more than five columns needing cleaning.
      if (counter < n_not_cleaned) {
        second_string = paste(second_string, "<li>...</li>")
      }
      second_string = paste(second_string, "</ul></p>")

      output = card(
        height = 270,
        HTML(first_string),
        HTML(second_string),
        style = "background:#ffec82" # Light yellow colour
      )
      
      return(output)
    })
    
    # Render the cleaned data
    output$cleaned_data_table <- renderDT({
      req(cleaned_data())
      
      # Find all columns that are character type.
      data = cleaned_data()
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
    
  })
}

