test_that("seenUnansweredRecode is a string", {
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="https://yourdatacenterid.qualtrics.com")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", seenUnansweredRecode = 123),
               "seenUnansweredRecode is not a string")
})
