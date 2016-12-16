test_that("getSurvey() throws an error", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # This should fail in 'do.call'
  expect_error(qualtRics::getSurveys(), "second argument must be a list")
})
