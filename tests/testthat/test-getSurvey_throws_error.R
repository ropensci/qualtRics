test_that("getSurvey() throws error", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Query fake ID with generic root url
  qualtRics::getSurvey("1234", "https://yourdatacenterid.qualtrics.com")
})
