################################################################################
### plots.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Functions to plot the JRiver Media Center Library dataframe
################################################################################

# Plot tag uniqueness
plot_tag_uniqueness = function(tag_df) {
  tidy_tag_df = make_long_tag_df(tag_df)
  ggplot(tidy_tag_df, aes(x = Field)) +
    geom_bar(stat = 'count') +
    geom_label(aes(label = after_stat(count)), stat = 'count') +
    labs(title = 'Composition of Music Library', x = 'Field', y = 'Unique Values')
}

# Plot tag completeness
plot_tag_completeness = function(tag_df,
                                 columns_to_retain = c('Genre', 'Composer', 'Work', 'Orchestra', 'Year Recorded', 'Album', 'Conductor', 'Soloists')) {
  tag_completeness_df = make_tag_completeness_df(tag_df)
  tidy_fraction_tag_df = make_long_tag_df(tag_completeness_df)
  ggplot(tidy_fraction_tag_df, aes(x = Field, y = Value)) +
    geom_bar(stat = 'identity') +
    geom_label(aes(label = scales::number(Value, accuracy = 0.0001))) +
    labs(title = 'Fraction of Items with Tag', x = 'Field', y = 'Unique Values')
}
