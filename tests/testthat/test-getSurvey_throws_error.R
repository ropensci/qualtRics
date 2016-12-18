test_that("getSurvey() throws error", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Query fake ID with generic root url
  expect_error(qualtRics::getSurvey("1234", "https://yourdatacenterid.qualtrics.com"),
               'you may not have the required authorization. Please check your API key and root url.')
})
