################################################################################
### plots.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Functions to plot the JRiver Media Center Library dataframe
################################################################################

# Set plot theme
ggplot2::theme_set(ggplot2::theme_minimal())

# Plot tag uniqueness
plot_tag_uniqueness = function(tag_df) {
  tidy_tag_df = make_long_tag_df(tag_df)
  ggplot2::ggplot(tidy_tag_df, ggplot2::aes(x = .data$Field)) +
    ggplot2::geom_bar(stat = 'count') +
    ggplot2::geom_label(ggplot2::aes(label = ggplot2::after_stat(.data$count)), stat = 'count') +
    ggplot2::labs(title = 'Composition of Music Library', x = 'Field', y = 'Unique Values')
}

# Plot tag completeness
#' @importFrom rlang .data
plot_tag_completeness = function(tag_df,
                                 columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')) {
  tag_completeness_df = make_tag_completeness_df(tag_df)
  tidy_fraction_tag_df = make_long_tag_df(tag_completeness_df)
  ggplot2::ggplot(tidy_fraction_tag_df, ggplot2::aes(x = .data$Field, y = .data$Value)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_label(ggplot2::aes(label = scales::number(.data$Value, accuracy = 0.0001))) +
    ggplot2::labs(title = 'Fraction of Items with Tag', x = 'Field', y = 'Unique Values')
}
