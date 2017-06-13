test_that("getSurvey() throws error", {
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="https://yourdatacenterid.qualtrics.com")
  # Query fake ID with generic root url
  expect_error(qualtRics::getSurvey("1234"),
               'you may not have the required authorization. Please check your API key and root url.')
})
