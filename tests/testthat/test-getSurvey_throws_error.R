test_that("getSurvey() throws error", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Query fake ID with generic root url
  expect_error(qualtRics::getSurvey("1234", "https://yourdatacenterid.qualtrics.com"),
               'Query returned status "401 - Unauthorized". Please check your code.')
})
