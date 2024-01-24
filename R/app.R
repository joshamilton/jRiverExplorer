################################################################################
### app.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

#' jRiverExplorer
#' Wrapper function for Shiny app to explore JRiver Media Center Library
#' @import shiny
#' @export
jRiverExplorer = function() {
  # Two panes, LHS has options, RHS has visualizations
  # Use a tabPanel to construct separate visualizations
  # Tabs 1: data table showing entire dataframe
  #         Enable filtering based on: genre, composer, conductor, orchestra, etc year recorded (range)
  # Tab 2: completeness of tagging
  #         Use same tags as selected in Tab 1
  #         Tables and bar plots showing: number of unique tags; number of works with missing tags
  # Tab 3: distributions:
  #         Number of genres, composers, works, etc
  #         Distributions of: number of works by genre, works per orchestra/conductor, number of recordings per work

  ui = fluidPage(
    # Title
    titlePanel('Explore the JRiver Media Center Library'),
    # Tabs
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
                        p('Unique values for Tag ', textOutput('uniqueness_click_field')),
                        DT::dataTableOutput(outputId = 'uniqueness_table')
                        ),
                 )
               ),
      tabPanel('Distributions'),
    )
  )

  server = function(input, output, session) {

    ### Server functions
    # Generate dataframe from selected XML file
    server_xml_to_dataframe = reactive({
      # Wait for file upload
      req(input$upload)
      # Check that it's an XML
      check_xml(input$upload$datapath)
      # Convert XML to dataframe
      xml_to_dataframe(input$upload$datapath)
    })
    # Reset selected tags (fields)
    reset_selected_tags = function() {
      updateSelectInput(inputId = 'fields', choices = colnames(server_xml_to_dataframe()),
                        selected = c('Composer', 'Orchestra', 'Genre', 'Work', 'Year Recorded', 'Album', 'Conductor', 'Soloists'))
    }
    # Determine field to be used for completeness analysis
    field_for_completeness_analysis = reactive({
      # Wait for column selection
      req(input$completeness_plot_click)
      # Filter based on user-specified frame
      field_values = field_values_from_completeness_df(server_filter_dataframe())
      field_values[round(input$completeness_plot_click$x)]
    })
    # Determine field to be used for uniqueness analysis
    field_for_uniqueness_analysis = reactive({
      # Wait for column selection
      req(input$uniqueness_plot_click)
      # Filter based on user-specified frame
      field_values = field_values_from_uniqueness_df(server_filter_dataframe())
      field_values[round(input$uniqueness_plot_click$x)]
    })
    ### Library Import and Export
    # Retrieve dataframe fields (columns) for the UI selectInput
    # All tracks are expected to contain:
    # Composer Album Orchestra Genre Work Year Recorded
    # Some tracks may contain:
    # Conductor	Soloists
    # Use these as defaults
    observeEvent(input$upload,
                 reset_selected_tags()
    )
    # Filter
    server_filter_dataframe = reactive({
      filter_dataframe(server_xml_to_dataframe(), input$fields)
    })
    # Render
    output$dataframe = DT::renderDataTable(server_filter_dataframe())
    # Reset choice of fields
    observeEvent(input$reset, {
      reset_selected_tags()
    })
    # Download dataframe
    output$download = downloadHandler(
      filename = 'tags.csv',
      content = function(file) {
        utils::write.csv(server_filter_dataframe(), file, row.names = FALSE)
      }
    )

    ### Completeness of Library Tagging
    # Missing Tag Values
    output$completeness_plot = renderPlot({
      plot_tag_completeness(
        server_filter_dataframe(), colnames(server_xml_to_dataframe()))
    })
    # Determine the field based on the x position of the click
    output$completeness_click_field = renderPrint({
      field_for_completeness_analysis()
    })
    # Print the untagged values for the selected bar
    server_completeness_table = reactive({
      field = field_for_completeness_analysis()
      # Expand dataframe to include Orchestra and Soloists
      tag_df = expand_df(server_filter_dataframe())
      # Filter to only include untagged values
      select_untagged_values(tag_df, field)
  #    tag_df %>% dplyr::filter(is.na(.data[[field]]))
    })
    # Display the untagged values for the selected bar
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
      field_for_uniqueness_analysis()
    })
    # Print the unique values for the selected bar
    server_uniqueness_table = reactive({
      field = field_for_uniqueness_analysis()
      select_unique_tag_values(server_filter_dataframe(), field)
    })
    # Display the unique values for the selected bar
    output$uniqueness_table = DT::renderDataTable(
      server_uniqueness_table()
    )
  }

  shinyApp(ui, server)
}
