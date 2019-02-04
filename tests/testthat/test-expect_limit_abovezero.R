test_that("Limit cannot be less than one", {
  expect_error(qualtRics::getSurvey("1234", limit = 0),
               "Limit parameter should be at least 1")
})
