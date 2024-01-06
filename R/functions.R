################################################################################
### functions.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

library(magrittr)
library(xml2)

# Read in a JRiver Media Center Library file
# Convert from XML to dataframe
# Each track is stored as an "Item" with numerous Fields, and not all tracks
# contain the same fields. Allow user to specify which fields to retain
# All tracks are expected to contain:
# Composer Album Orchestra Genre Work Year Recorded
# Use these as defaults
# Some tracks may contain:
# Conductor	Soloists

# Extract fields and values for each item
extract_tags = function(item) {
  cur_fields = xml_children(item)
  cur_field_names = xml_attr(cur_fields, "Name")
  cur_values = xml_text(cur_fields)
  setNames(cur_values, cur_field_names)
}

# Convert XML file to dataframe
xml_to_dataframe = function(file) {

  # Read the XML file and find all Items and Fields
  xml_file = read_xml(file)
  items = xml_find_all(xml_file, "//Item")
  all_field_names = unique(xml_find_all(xml_file, "//Item/Field") %>% xml_attr("Name"))

  # Create a dataframe
  tag_df = as.data.frame(matrix(nrow = length(items), ncol = length(all_field_names)))
  colnames(tag_df) = all_field_names
  tag_df = dplyr::bind_rows(lapply(items, extract_tags))

}

# Filter dataframe and simplify to contain unique entries
filter_dataframe = function(dataframe, columns_to_retain) {
  tag_df = dataframe %>%
    dplyr::select(all_of(columns_to_retain)) %>%
    dplyr::distinct()
}

# Informal test code
# tag_df should have 799 obs. of 5 variables
# file = 'test/library.xml'
# columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded')
# tag_df = xml_to_dataframe(file)
# tag_df = filter_dataframe(tag_df, columns_to_retain)
