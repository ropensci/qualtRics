test_that("seenUnansweredRecode is a string", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", root_url="test", seenUnansweredRecode = 123),
               "seenUnansweredRecode is not a string")
})
