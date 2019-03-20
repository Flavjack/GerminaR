context("infrastructure")

# 
# td <- tempdir()
# unlink(file.path(td, "R"))

test_that("check evalDays function", {
  expect_that( evalDays(evalName = NULL, data = NULL),  throws_error())  
})

test_that("check evalDays function", {
  expect_that( evalDays(evalName = NA, data = GerminaR),  throws_error())  
})

test_that("test data structure", {

check_data.frame <- function(datos) {
  expect_is(datos, "data.frame")
  }
})



