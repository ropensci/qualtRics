test_that("seenUnansweredRecode is a string", {
  qualtrics_api_credentials(api_key = "1234",
                            base_url = "yourdatacenterid.qualtrics.com")
  # Call getsurvey
  expect_error(getSurvey("1234",
                         seenUnansweredRecode = 123),
               "seenUnansweredRecode is not a string")
})
