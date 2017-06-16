test_that("Arguments are properly checked by checkParams()", {
  # This should fail because we can't connect to the qualtRics API
  # Store dummy key
  qualtRics::registerOptions(api_token="1234",
                             root_url="https://yourdatacenterid.qualtrics.com")
  # Query with all options
  expect_error(qualtRics::getSurvey("1234",
                                    lastResponseId = "1234",
                                    startDate="2017-01-01",
                                    endDate="2017-01-31",
                                    seenUnansweredRecode = "UA",
                                    includedQuestionIds = c("QID1", "QID2"),
                                    save_dir=tempdir()),
               "you may not have the required authorization")
})
