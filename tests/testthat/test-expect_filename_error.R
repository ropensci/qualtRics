test_that("Survey exists to read from disk", {
  expect_error(qualtRics::readSurvey("/users/julia/desktop/error.csv"),
               "does not exist")
})
