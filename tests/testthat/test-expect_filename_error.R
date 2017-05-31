test_that("Survey exists to read from disk", {
  # Call getsurvey
  expect_error(qualtRics::readSurvey("/users/jasper/desktop/error.csv"),
               "does not exist")
})
