test_that("getSurveyQuestions() throws an error", {
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234",
                            base_url = "https://yourdatacenterid.qualtrics.com")
  # This should fail in 'do.call'
  expect_error(qualtRics::getSurveyQuestions("1234"),
               "you may not have the\nrequired authorization")
})
