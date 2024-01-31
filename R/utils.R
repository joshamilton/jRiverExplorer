################################################################################
### utils.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Helper functions for app.R
################################################################################

# Currently only checks file extension
#' Validate the XML file
#'
#' Validate the file is an XML file by check the file extension
#' Validate the XML file has the expected structure: <Item> and <Field> tags
#' with <Field> having the attribute "Name"
#' The following Field Names are present: 'Genre', 'Composer', 'Work',
#' 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists'
#' @param name A string containing the name of the file to load
#'
#' @return For XML files, returns nothing. For non-XML files, returns an error.
#' The error will get captured by the reactive server_xml_to_dataframe in app.R
#' and displayed to the user by Shiny.
#'
#' @export
#'
#' @examples
#' check_xml('test/library.xml')
check_xml = function(name) {
  file_extension = tolower(tools::file_ext(name))
  if (file_extension != 'xml') {
    validate('Please upload an XML file')
  }
}

# Read in a JRiver Media Center Library file
# Convert from XML to dataframe
# Each track is stored as an "Item" with numerous Fields, and not all tracks
# contain the same fields.

#' Extract fields and values for each item
#'
#' @param item An XML node containing the item to extract
#'
#' @return A named vector containing the fields and values for the item
#' @export
#'
#' @examples
#' \dontrun{
#' item = xml2::read_xml('test/library.xml') %>% xml2::xml_find_all("//Item") %>% .[[1]]
#' extract_tags(item)
#' }
extract_tags = function(item) {
  cur_fields = xml2::xml_children(item)
  cur_field_names = xml2::xml_attr(cur_fields, "Name")
  cur_values = xml2::xml_text(cur_fields)
  stats::setNames(cur_values, cur_field_names)
}

#' Convert XML file to dataframe
#'
#' @param file A string containing the name of the file to load
#'
#' @return A dataframe containing the fields and values for each item. The
#' dataframe will have one row per item. Columns will be the field names and
#' values will be the field values.
#' @export
#'
#' @examples
#' \dontrun{
#' xml_to_dataframe('test/library.xml')
#' }
xml_to_dataframe = function(file) {
  # Read the XML file and find all Items and Fields
  xml_file = xml2::read_xml(file)
  items = xml2::xml_find_all(xml_file, "//Item")
  all_field_names = unique(xml2::xml_find_all(xml_file, "//Item/Field") %>% xml2::xml_attr("Name"))
  # Create a dataframe
  tag_df = as.data.frame(matrix(nrow = length(items), ncol = length(all_field_names)))
  colnames(tag_df) = all_field_names
  tag_df = dplyr::bind_rows(lapply(items, extract_tags))

}
