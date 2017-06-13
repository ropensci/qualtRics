test_that("Limit cannot be less than one", {
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="abcd")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", limit = 0),
               "Limit parameter should be at least 1")
})
