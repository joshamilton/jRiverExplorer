# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(jRiverExplorer)

test_check("jRiverExplorer")

# Thoughts on test library.xml files
# For the positive test, two entries: one with a soloist, one with an orchestra
# For the negative test, one entry with one or more required fields missing
# Both tests should contain optional and required fields
# For testing the tables and plots functions, use the complete library.xml file
#  modified to remove 'Gustav Leonhardt' as a soloist, so the 'Orchestra or
#  Soloist' condition can be checked
