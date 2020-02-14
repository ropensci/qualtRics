test_that("arguments are properly checked", {
  # This should fail because we can't connect to the qualtRics API
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "yourdatacenterid.qualtrics.com"
  )
  # Query with all options
  expect_error(
    fetch_survey("1234",
                 last_response = "1234",
                 start_date = "2017-01-01",
                 end_date = "2017-01-31",
                 unanswer_recode = 999,
                 unanswer_recode_multi = 999,
                 include_questions = c("QID1", "QID2"),
                 save_dir = tempdir()
    ),
    "you may not have the\nrequired authorization"
  )
})
