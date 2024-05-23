
dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
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
      tabPanel("Step 2) Data Cleaning",
               HTML("<br>"),
               p("Some information here on data cleaning... Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce pharetra et nibh ac varius. Quisque dapibus consectetur ex. Fusce sit amet dui erat. Sed tellus elit, tempor vel egestas a, viverra nec erat. Nullam nec felis posuere, faucibus justo eu, luctus eros. Cras consequat mauris sed ante lacinia, ut lobortis lorem dignissim. Nunc elementum rhoncus ex, nec pulvinar ex ullamcorper et. Praesent sodales, lorem nec fermentum pretium, tortor purus vestibulum arcu, eu egestas turpis est nec dui."),
               HTML("<br>"),
               sidebarPanel(
                 p("TEST"),
                 uiOutput(outputId = ns("button_test"))
                 
               ),
               mainPanel(
                 h1("TEST")
               )
      )
    )
  )
}


dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to read the uploaded file
    data <- reactive({
      req(input$file)  # Ensure a file is uploaded
      infile <- input$file
      data <- read_excel(infile$datapath, sheet = "Form Responses 1")
      return(data)
    })
    
    
    # Render the processed data as a table
    output$table <- renderTable({
      req(data())  # Ensure processed data is available
      data()
    })
    
    # Create button when the data is uploaded.
    # Widget gallery: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
    output$button_test <- renderUI({
      req(data())
      options = colnames(data())
      input_btn = selectInput(ns("select"), label = "Select box", 
                  choices = options)
      print(options)
      print(data())
      print("REACHED LINE 65")
      return(input_btn)
    })
  })
}

