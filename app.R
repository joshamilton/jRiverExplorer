################################################################################
### app.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

library(DT)
library(shiny)
source('R/functions.R')

# Two panes, LHS has options, RHS has visualizations
# Use a tabPanel to construct separate visualizations
# Tabs 1: data table showing entire dataframe
### YOU ARE HERE
#         Enable filtering based on: genre, composer, conductor, orchestra, etc year recorded (range)
# Tab 2: completeness of tagging
# Tab 3: distributions:
#         Number of genres, composers, works, etc
#         Distributions of: number of works by genre, works per orchestra/conductor, number of recordings per work

ui = fluidPage(

  navbarPage('Explore the JRiver Media Center Library'),

  tabsetPanel(

    # Upload the file.
    # Sidebar for file upload/download, options to select particular columns
    # Right-hand side will display the entire dataframe
    tabPanel('Library Import and Export',

      # Sidebar
      sidebarPanel(
        # Input: XML file
        fileInput(inputId = 'upload', label = 'Select JRiver Library XML file', buttonLabel = 'Upload'),

        # Select columns to include
        selectInput(inputId = 'fields', label = 'Select fields to retain', multiple = TRUE, choices = NULL),

        # Allow reset
        actionButton(inputId = 'reset', 'Reset'),

        # Download:
        downloadButton(outputId = 'download', label = 'Download as CSV')
        ),

      # Main panel
      mainPanel(
        # Output: dataframe generated from XML file
        DT::dataTableOutput(outputId = 'dataframe')
      ),
    ),

    tabPanel('Tag Summary'),

    tabPanel('Visualizations'),
  )
)

server = function(input, output, session) {

  # Generate dataframe from selected XML file
  server_xml_to_dataframe = reactive({
    # wait for file upload
    req(input$upload)
    # Check it's an XML file
    file_extension = tools::file_ext(input$upload$name)
    if (file_extension != 'xml') {
      validate('Please upload an XML file')
    }
    # Convert XML to dataframe
    xml_to_dataframe(input$upload$datapath)
    })
  # Retrieve dataframe fields (columns) for the UI selectInput
  observeEvent(input$upload,
               updateSelectInput(inputId = 'fields', choices = colnames(server_xml_to_dataframe()),
                                 selected = c('Composer', 'Orchestra', 'Genre', 'Work', 'Year Recorded', 'Album'))
  )
  server_filter_dataframe = reactive({
    # Wait for column selection
    req(input$fields)
    # Filter
    filter_dataframe(server_xml_to_dataframe(), input$fields)
  })

  # Render
  output$dataframe = DT::renderDataTable(server_filter_dataframe())

  # Reset choice of fields
  observeEvent(input$reset, {
    updateSelectInput(inputId = 'fields', choices = colnames(server_xml_to_dataframe()))
  })

  # Download dataframe
  output$download = downloadHandler(
    filename = 'tags.csv',
    content = function(file) {
      write.csv(server_filter_dataframe(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
