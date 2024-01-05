################################################################################
### functions.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

library(magrittr)
library(xml2)

# Read in a JRiver Media Center Library file
# Convert from XML to dataframe
# Each track is stored as an "Item" with numerous fields, and not all tracks
# contain the same fields.
# Convert into a dataframe with the following fields retained as columns:
#   Genre
#   Composer
#   Work
#   Orchestra
#   Year Recorded
# Every track should have these five fields
# At some point, may want different fields. So the function will read all fields
# and filter once the dataframe has been constructed.

# Read the XML file and find all Items and Fields
xml_file = read_xml('test/library.xml')
items = xml_find_all(xml_file, "//Item")
all_field_names = unique(xml_find_all(xml_file, "//Item/Field") %>% xml_attr("Name"))

# Create a dataframe
tag_df = as.data.frame(matrix(nrow = length(items), ncol = length(all_field_names)))
colnames(tag_df) = all_field_names

# Function to extract values for each item
extract_tags <- function(item) {
  cur_fields <- xml_children(item)
  cur_field_names <- xml_attr(cur_fields, "Name")
  cur_values <- xml_text(cur_fields)
  setNames(cur_values, cur_field_names)
}

# Create dataframe
tag_df = dplyr::bind_rows(lapply(items, extract_tags))

# Filter dataframe and simplify to contain unique entries
columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded')
tag_df = tag_df %>%
  dplyr::select(all_of(columns_to_retain)) %>%
  dplyr::distinct()

