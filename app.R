################################################################################
### app.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

library(shiny)
source('R/functions.R')

# Two panes, LHS has options, RHS has visualizations
# Use a tabPanel to construct separate visualizations
# Tabs: data table showing entire dataframe
#       completeness of tagging
#       distributions:
#         Number of genres, composers, works, etc
#         Distributions of: number of works by genre, works per orchestra/conductor, number of recordings per work
# Enable filtering based on: genre, composer, conductor, orchestra, etc year recorded (range)

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
        # Download:
        downloadButton(outputId = 'download', label = 'Download as CSV')
        ),

      # Main panel
      mainPanel(
        # Output: dataframe generated from XML file
        tableOutput(outputId = 'dataframe')
      ),
    ),

    tabPanel('Tag Summary'),

    tabPanel('Visualizations'),
  )
)

server = function(input, output) {

  # Generate dataframe from selected XML file
  get_dataframe = reactive({
    # wait for file upload
    req(input$upload)

    # Check it's an XML file
    file_extension = tools::file_ext(input$upload$name)
    if (file_extension != 'xml') {
      validate('Please upload an XML file')
    }

    xml_to_dataframe(input$upload$datapath)
    })
  output$dataframe = renderTable(get_dataframe())

  # Download dataframe
  output$download = downloadHandler(
    filename = 'tags.csv',
    content = function(file) {
      write.csv(get_dataframe(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
