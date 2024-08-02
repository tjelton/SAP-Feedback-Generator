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
    
    # Data lock conditional panel (for after the user clicks to lock the data).
    conditionalPanel(
      condition = paste0("output['", ns('data_locked'), "']"),
      HTML("<br><br>"),
      card(
        HTML("<center><b><p>Data Lock</p></b></center>"),
        HTML("<p>You have previously completed your data cleaning, and locked in your data. You are able to 
             redo the data cleaning, but this will <b>reset</b> all analysis you have previously completed.<p>"),
        HTML("<p><b><i>Do you want to unlock the data (this will clear any analysis)?</i></b></p>"),
        actionButton(ns("unlcok_data"), "Unlock Data", class = "btn-danger", width = "20%"),
        style = "background:#fccccc" # Light red colour.
      ),
      HTML("<br><br>")
    ),
    
    uiOutput(outputId = ns("value_box_steps")),
    
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
                            HTML("<p><b><u><center>File Input</center></u></b></p>"),
                            fileInput(ns("data_file"), ""),
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
                 
                 # Message prompting the user to update data before proceeding.
                 conditionalPanel(
                   condition = paste0("output['", ns('data_uploaded_flag'), "'] !== true"),
                   card(
                     HTML("<br><br><center><b>No data uploaded. Proceed to \"Step 1) Data Upload\"</b></center><br><br>"),
                     style = "background:#fccccc" # Light red colour.
                   )
                 ),
                 
                 # Display the remainder of the page once the data has been uploaded.
                 conditionalPanel(
                   condition = paste0("output['", ns('data_uploaded_flag'), "'] == true"),
                   
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
        ),
        
        # Page for the user to check that they are happy with the data cleaning before progressing to the analysis stage.
        # The user will have to press a check mark to indicate that they are happy before continuing.
        tabPanel("Step 3) Finalise Data",
                 HTML('<hr style="border: 0; border-top: 2px solid #232324; margin: 10px 0 15px 0;">'),
                 HTML("<br>"),
                 
                 # Message prompting the user to update data before proceeding.
                 conditionalPanel(
                   condition = paste0("output['", ns('data_uploaded_flag'), "'] !== true"),
                   card(
                     HTML("<br><br><center><b>No data uploaded. Proceed to \"Step 1) Data Upload\"</b></center><br><br>"),
                     style = "background:#fccccc" # Light red colour.
                   )
                 ),
                 
                 conditionalPanel(
                   condition = paste0("output['", ns('data_uploaded_flag'), "'] == true"),
                   
                   fluidRow(
                     # Instructions 
                     column(8,
                            card(
                              height = 300,
                              HTML("
                              <p><b>Finalise Data Cleaning:</b><br>
                                 <ul>
                                   <li>The \"Cleaned Data\" table below contains the final cleaned data alligned with your choices in step 2. You can also
                                   check the \"Data Classifications\" table as a quick check that you are happy with the current data classifactions.</li>
                                   <li>Once you are satisfied that the data is cleaned to your preference, proceed to press the \"Lock Data\" 
                                   button. Note that clicking this button cannot be undone without clearing your future data analysis.</li>
                                   <li>A warning message will appear if you have not yet manually checked each of the data columns. However, you can still proceed
                                   to press the \"Complete Data Cleaning\" button if you are satisfied with the current data state.</li>
                                 </ul>
                              </p>"),
                            ),   
                     ),
                     
                     # Button to finalise decision.
                     column(4,
                            card(
                              HTML("<p><b><u><center>Finalise Data</center></u></b></p>"),
                              uiOutput(outputId = ns("warning_step_3_not_all_cleaned")),
                              HTML("<p><center>Before pressing \"Lock Data\", check you are happy with the cleaned data below.</center></p><br>"),
                              actionButton(ns("lock_data"), "Lock Data", class = "btn-danger"),
                              style = "background:#cce4fc" # Light blue colour
                            ),
                     ),
                     
                     # Cleaned data table display.
                     navset_underline(
                       # Output the entire cleaned data set.
                       nav_panel(
                         "Cleaned Data",
                         HTML("<br>"),
                         DT::dataTableOutput(ns("cleaned_data_table"))
                       ),
                       
                       # Output the classification of each variable.
                       nav_panel(
                         "Data Classifications",
                         HTML("<br>"),
                         DT::dataTableOutput(ns("cleaned_data_types"))
                       ),
                     ),
                     
                   ),
                 ),
                 
        ),
      ),
      
      style = "color:black; background:#FFFFFF"
    ),
    
  )
}