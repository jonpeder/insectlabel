# Packages
require(devtools)
require(roxygen2)

# set up package test
usethis::use_testthat()
# This will:
# 1. Create a tests/testthat directory.
# 2. Add testthat to the Suggests field in the DESCRIPTION.
# 3. Create a file tests/testthat.R that runs all your tests when R CMD check runs.

# Create a test file. Assign a name starting with "test", e.g. test_test1.R
# Test the package by running differnt operations within the "test_that" function.
# Within the test_that function, add an expectation, e.g:

testthat::test_that("multiplication works", {
  testthat::expect_equal(4*4, 16)
})

# or

testthat::test_that("output is correct", {
  output <- data.frame(rep("t",3), rep("e",3), rep("f",3))
  testthat::expect_output(str(output), "3 obs. of 3 variables")
})


