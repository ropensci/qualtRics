test_that("arguments are properly checked", {
  # This should fail because we can't connect to the qualtRics API
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "yourdatacenterid.qualtrics.com"
  )
  # Query with all options
  expect_error(
    getSurvey("1234",
      lastResponseId = "1234",
      startDate = "2017-01-01",
      endDate = "2017-01-31",
      seenUnansweredRecode = "UA",
      includedQuestionIds = c("QID1", "QID2"),
      saveDir = tempdir()
    ),
    "you may not have the\nrequired authorization"
  )
})
