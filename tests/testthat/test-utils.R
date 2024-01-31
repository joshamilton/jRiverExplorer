################################################################################
### test-utils.R
### Copyright (c) Joshua Hamilton, 2024
### Email: joshamilton@gmail.com
### Testing of helper functions for app.R
################################################################################

# Test that check_xml returns an error when reading a non-XML file
test_that('check_xml returns an error when reading a non-XML file',{
  expect_error(check_xml('library.csv'), 'Please upload an XML file')
})

# Test that extract_tags returns a named vector with the proper names
test_that('extract_tags returns a named vector with the proper names',{
  item = xml2::read_xml('library-utils-positive.xml') %>% xml2::xml_find_all('//Item') %>% .[[1]]
  expected_names = c('Name', 'Album', 'Genre', 'Track #', 'Disc #', 'Composer',
                     'Conductor', 'Orchestra', 'Work', 'Total Tracks', 'Total Discs',
                     'Year Recorded', 'Year Released', 'Year Written', 'Record Label')
  expect_named(extract_tags(item), expected_names)
})

# Test that extract_tags returns the proper values
test_that('extract_tags returns a named vector with the proper values',{
  item = xml2::read_xml('library-utils-positive.xml') %>% xml2::xml_find_all('//Item') %>% .[[1]]
  expected_values = c('Canzon XVII', 'Music for Brass', 'Renaissance', '1', '1',
                      'Gabrieli, Giovanni', 'Crees, Eric',
                      'London Symphony Orchestra Brass', 'Canzon XVII', '43', '3',
                      '1996', '1997', '1615','Naxos')
  expect_setequal(extract_tags(item), expected_values)
})

# Test that xml_to_dataframe returns the proper dataframe
test_that('extract_tags returns a named vector with the proper values',{
  tag_df = xml_to_dataframe('library-utils-positive.xml')
  expected_tibble = tibble::tibble(
    Name = c('Canzon XVII', 'String Quintet No 1 in B-flat, K 174 - I. Allegro moderato'),
    Album = c('Music for Brass', 'The String Quintets'),
    Genre = c('Renaissance', 'Classical'),
    `Track #` = c('1', '1'),
    `Disc #` = c('1', '1'),
    Composer = c('Gabrieli, Giovanni', 'Mozart, Wolfgang Amadeus'),
    Conductor = c('Crees, Eric', NA),
    Orchestra = c('London Symphony Orchestra Brass', 'Amadeus Quartet'),
    Work = c('Canzon XVII', 'String Quintet No 1'),
    `Total Tracks` = c('43', '25'),
    `Total Discs` = c('3', '3'),
    `Year Recorded` = c('1996', '1974'),
    `Year Released` = c('1997', NA),
    `Year Written` = c('1615', NA),
    `Record Label` = c('Naxos', NA),
    `Catalog #` = c(NA, 'K 174'),
    Soloists = c(NA, 'Aronowitz, Cecil'),
    Movement = c(NA, 'I. Allegro moderato'),
    `Initial Key` = c(NA, 'B-flat')
  )
  expect_identical(tag_df, expected_tibble)
})
