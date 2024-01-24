################################################################################
### tables.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Functions for working with tag dataframe
################################################################################

# Filter dataframe and simplify to contain unique entries
filter_dataframe = function(dataframe,
                            columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')) {
  dataframe %>%
    dplyr::select(dplyr::all_of(columns_to_retain)) %>%
    dplyr::distinct()
}

# Convert the tag_df to tidy (long) format
#' @importFrom rlang .data
make_long_tag_df = function(tag_df) {
  tag_df %>%
    tidyr:: pivot_longer(cols = dplyr::everything(), names_to = 'Field', values_to = 'Value') %>%
    dplyr::filter(!is.na(.data$Value)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Field = as.factor(.data$Field))
}

# Expand dataframe to include 'Orchestra or Soloist' field
#' @importFrom rlang .data
expand_df = function(tag_df) {
  tag_df %>% dplyr::mutate(`Orchestra or Soloist` = ifelse(!is.na(.data$Orchestra) | !is.na(.data$Soloists), 1, NA))
}

# Convert dataframe of unique tag values to dataframe of tag uniqueness
make_tag_completeness_df = function(tag_df){
  expand_df(tag_df) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ sum(!is.na(.)) / dplyr::n(), .names = '{.col}'))
}

# Get unique values from the completeness dataframe
field_values_from_completeness_df = function(tag_df) {
  levels(make_long_tag_df(
    make_tag_completeness_df(tag_df))
    $Field)
}

# Get unique values from the uniqueness dataframe
field_values_from_uniqueness_df = function(tag_df) {
  levels(make_long_tag_df((tag_df))$Field)
}

# Retrieve items missing a tag value
select_untagged_values = function(tag_df, field) {
  tag_df %>% dplyr::filter(is.na(.data[[field]]))
}

# Retrieve unique values for a tag
select_unique_tag_values = function(tag_df, field) {
  make_long_tag_df(tag_df) %>%
    dplyr::filter(.data$Field == field) %>%
    dplyr::filter(!is.na(.data$Value)) %>%
    dplyr::arrange(.data$Value) %>%
    dplyr::select(.data$Value)
}

# Informal test code
# tag_df should have 799 obs. of 5 variables
#file = 'test/library.xml'
#columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')
# tag_df = xml_to_dataframe(file)
# tag_df = filter_dataframe(tag_df, columns_to_retain)
