################################################################################
### utils.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Helper functions for app.R
################################################################################

# Load Library XML file
check_xml = function(name) {
  file_extension = tools::file_ext(name)
  # VA
  if (file_extension != 'xml') {
    validate('Please upload an XML file')
  }
}

# Read in a JRiver Media Center Library file
# Convert from XML to dataframe
# Each track is stored as an "Item" with numerous Fields, and not all tracks
# contain the same fields.

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

# Validate Library XML file
# Need to write a function to confirm the XML file has the expected fields
