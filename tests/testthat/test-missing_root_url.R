test_that("Missing root url", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Call
  expect_error(qualtRics::getSurvey("1234", 'argument "root_url" is missing, with no default'))
})
