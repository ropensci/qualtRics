test_that("getSurveys() throws an error", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # This should fail in 'do.call'
  expect_error(qualtRics::getSurveys(),
               "you may not have the required authorization. Please check your API key and root url.")
})
