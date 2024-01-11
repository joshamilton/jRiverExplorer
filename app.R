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

  titlePanel('Explore the JRiver Media Center Library'),

  tabsetPanel(

    # Upload the file.
    # Sidebar for file upload/download, options to select particular columns
    # Right-hand side will display the entire dataframe
    tabPanel('Library Import and Export',

      ### Sidebar
      sidebarPanel(
        # Input: XML file
        fileInput(inputId = 'upload', label = 'Select JRiver Library XML file', buttonLabel = 'Upload'),

        # Select columns to include
        selectInput(inputId = 'fields', label = 'Select additional fields to retain', multiple = TRUE, choices = NULL),

        # Allow reset
        actionButton(inputId = 'reset', 'Reset'),

        # Download:
        downloadButton(outputId = 'download', label = 'Download as CSV')
        ),

      ### Main panel
      mainPanel(
        # Output: dataframe generated from XML file
        h4('Selected Fields'),
        DT::dataTableOutput(outputId = 'dataframe')
      ),
    ),

    # Inspect completeness of tags, and unique tag values
    # For now, uses same options as previous tab
    # Right-hand side has plots and reactive dataframes
    tabPanel('Tagging QC',
             ### Sidebar
             sidebarPanel('Visit the "Library Import and Export" tab to upload your library and select fields to visualize'),

             ### Main panel
             mainPanel(
               ### Completeness of Library Tagging
               fluidRow(h4('Completeness of Library Tagging'),
                        p('Click on a bar to view the values of the untagged items'),
                        plotOutput('completeness_plot',
                                   click = 'completeness_plot_click'),
                        p('Items missing Tag ', textOutput('completeness_click_field')),
                        DT::dataTableOutput(outputId = 'completeness_table')
                        ),
               ### Composition of Music Library
               fluidRow(h4('Unique Tag Values within Music Library'),
                        p('Click on a bar to view the values of the unique items'),
                        plotOutput('uniqueness_plot',
                                   click = 'uniqueness_plot_click'),
                        p('Unique values for Tag ', textOutput('uniqueness_plot_click')),
                        DT::dataTableOutput(outputId = 'uniqueness_table')
                        ),

               )
             ),

    tabPanel('Distributions'),
  )
)

server = function(input, output, session) {

  ### Library Import and Export
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
                                 selected = c('Composer', 'Orchestra', 'Genre', 'Work', 'Year Recorded', 'Album', 'Conductor', 'Soloists'))
  )
  server_filter_dataframe = reactive({
    # Filter
    filter_dataframe(server_xml_to_dataframe(), input$fields)
  })

  # Render
  output$dataframe = DT::renderDataTable(server_filter_dataframe())

  # Reset choice of fields
  observeEvent(input$reset, {
    updateSelectInput(inputId = 'fields', choices = colnames(server_xml_to_dataframe()),
                      selected = c('Composer', 'Orchestra', 'Genre', 'Work', 'Year Recorded', 'Album', 'Conductor', 'Soloists'))
  })

  # Download dataframe
  output$download = downloadHandler(
    filename = 'tags.csv',
    content = function(file) {
      write.csv(server_filter_dataframe(), file, row.names = FALSE)
    }
  )

  ### Completeness of Library Tagging

  ### Missing Tag Values
  output$completeness_plot = renderPlot({
    plot_tag_completeness(
      server_filter_dataframe(), colnames(server_xml_to_dataframe()))
  })

  # Determine the field based on the x position of the click
  output$completeness_click_field = renderPrint({
    # Wait for column selection
    req(input$completeness_plot_click)
    # Filter based on user-specified frame
    field_values = levels(make_long_tag_df(make_tag_completeness_df(server_filter_dataframe(), colnames(server_xml_to_dataframe())))$Field)
    field_values[round(input$completeness_plot_click$x)]
  })

  ### YOU ARE HERE
  ### Fails when user selects non-default columns
  ### server_xml_to_dataframe returns the entire tag_df
  ### Problem is with the filter_datarame function - for tag completeness, it may need to return a default set of columns for the output to make sense
  # Print the untagged values for the selected bar
  server_completeness_table = reactive({
    # Wait for column selection
    req(input$completeness_plot_click)
    # Filter based on user-specified frame
    field_values = levels(make_long_tag_df(make_tag_completeness_df(server_filter_dataframe(), colnames(server_xml_to_dataframe())))$Field)
    field = field_values[round(input$completeness_plot_click$x)]
    # Expand dataframe to include Orchestra and Soloists
    tag_df = server_filter_dataframe() %>%
      dplyr::mutate(`Orchestra or Soloist` = ifelse(!is.na(Orchestra) | !is.na(Soloists), 1, NA))
    tag_df %>% dplyr::filter(is.na(.data[[field]]))
  })

  output$completeness_table = DT::renderDataTable(
    server_completeness_table()
  )

  ### Unique Tag Values
  output$uniqueness_plot = renderPlot({
    plot_tag_uniqueness(
      server_filter_dataframe())
      })

  # Determine the field based on the x position of the click
  output$uniqueness_click_field = renderPrint({
    req(input$uniqueness_plot_click)
    field_values = levels(make_long_tag_df(server_filter_dataframe())$Field)
    field_values[round(input$uniqueness_plot_click$x)]
  })

  # Print the unique values for the selected bar
  server_uniqueness_table = reactive({
    # Wait for column selection
    req(input$uniqueness_plot_click)
    # Filter based on user-specified frame
    field_values = levels(make_long_tag_df(server_filter_dataframe())$Field)
    field = field_values[round(input$uniqueness_plot_click$x)]
    make_long_tag_df(server_filter_dataframe()) %>%
      dplyr::filter(Field == field) %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::arrange(Value) %>%
      dplyr::select(Value)
  })

  output$uniqueness_table = DT::renderDataTable(
    server_uniqueness_table()
  )
}

shinyApp(ui, server)
