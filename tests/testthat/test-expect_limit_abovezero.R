test_that("Limit cannot be less than one", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", root_url="test", limit = 0),
               "Limit parameter should be at least 1")
})
