test_that("getSurveyQuestions() throws an error", {
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="https://yourdatacenterid.qualtrics.com")
  # This should fail in 'do.call'
  expect_error(qualtRics::getSurveyQuestions("1234"),
               "you may not have the required authorization")
})
