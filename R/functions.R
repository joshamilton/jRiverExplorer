################################################################################
### functions.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
################################################################################

library(ggplot2)
theme_set(theme_minimal())
library(magrittr)
library(xml2)

# Read in a JRiver Media Center Library file
# Convert from XML to dataframe
# Each track is stored as an "Item" with numerous Fields, and not all tracks
# contain the same fields. Allow user to specify which fields to retain
# All tracks are expected to contain:
# Composer Album Orchestra Genre Work Year Recorded
# Some tracks may contain:
# Conductor	Soloists
# Use these as defaults

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
filter_dataframe = function(dataframe,
                            columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')) {
  tag_df = dataframe %>%
    dplyr::select(all_of(columns_to_retain)) %>%
    dplyr::distinct()
}

filter_dataframe_by_field = function(dataframe, field) {
  tidy_tag_df %>% dplyr::filter(Field == field) %>% dplyr::select(Value)
}

# Convert the tag_df to tidy (long) format
make_long_tag_df = function(tag_df) {
  tidy_tag_df = tag_df %>%
    tidyr:: pivot_longer(cols = everything(), names_to = 'Field', values_to = 'Value') %>%
    dplyr::distinct() %>%
    dplyr::mutate(Field = as.factor(Field))
}

# Plot tag uniqueness
plot_tag_uniqueness = function(tag_df) {
  tidy_tag_df = make_long_tag_df(tag_df)
  ggplot(tidy_tag_df, aes(x = Field)) +
    geom_bar(stat = 'count') +
    geom_label(aes(label = after_stat(count)), stat = 'count') +
    labs(title = 'Composition of Music Library', x = 'Field', y = 'Unique Values')
}

# Plot tag completeness
make_tag_completeness_df = function(tag_df,
                                    columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')){
  tag_fraction_df = tag_df %>%
    dplyr::mutate(`Orchestra or Soloist` = ifelse(!is.na(Orchestra) | !is.na(Soloists), 1, NA)) %>%
    dplyr::summarize(across(everything(), ~ sum(!is.na(.)) / dplyr::n(), .names = '{.col}'))
}

plot_tag_completeness = function(tag_df,
                                 columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')) {
  tag_completeness_df = make_tag_completeness_df(tag_df, columns_to_retain)
  tidy_fraction_tag_df = make_long_tag_df(tag_completeness_df)
  ggplot(tidy_fraction_tag_df, aes(x = Field, y = Value)) +
    geom_bar(stat = 'identity') +
    geom_label(aes(label = scales::number(Value, accuracy = 0.0001))) +
    labs(title = 'Fraction of Items with Tag', x = 'Field', y = 'Unique Values')
}

# Informal test code

# tag_df should have 799 obs. of 5 variables
#file = 'test/library.xml'
#columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')
#tag_df = xml_to_dataframe(file)
#tag_df = filter_dataframe(tag_df, columns_to_retain)
