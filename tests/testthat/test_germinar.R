context("infrastructure")

# 
# td <- tempdir()
# unlink(file.path(td, "R"))

test_that("check evalDays function", {
  expect_that( evalDays(evalName = NULL, data = NULL),  throws_error())  
})
