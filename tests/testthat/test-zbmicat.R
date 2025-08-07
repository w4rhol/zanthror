test_that("zbmicat_stata works with basic input", {
  # Basic test to ensure package loads and function exists
  expect_true(exists("zbmicat_stata"))

  # Test with simple valid input
  result <- zbmicat_stata(
    bmi = c(18.5, 25.0),
    age = c(10, 12),
    gender = c(1, 2)
  )

  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(!is.na(result)))
})

test_that("zbmicat_lms works when sitar is available", {
  skip_if_not_installed("sitar")

  # Basic test for LMS function
  expect_true(exists("zbmicat_lms"))

  result <- zbmicat_lms(
    bmi = c(18.5, 25.0),
    age = c(10, 12),
    gender = c(1, 2)
  )

  expect_type(result, "character")
  expect_length(result, 2)
})
