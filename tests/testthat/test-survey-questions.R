context("Get survey questions for a survey")

test_that("survey_questions() makes a request with expected structure, and parses response", {

  vcr::use_cassette("survey_questions", {
    x <- survey_questions("SV_3gbwq8aJgqPwQDP")
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_named(x, c("qid", "qname", "question", "force_resp"))
  expect_equal(nrow(x), 6)
  expect_type(x$qid, "character")
  expect_type(x$qname, "character")
  expect_type(x$question, "character")
  expect_type(x$force_resp, "logical")

})

# Change credentials to
qualtrics_api_credentials(api_key = holder_API, base_url = "t.qualtrics.com")

test_that("survey_questions() throws an error when URL is bad", {

  expect_error(
    qualtRics::survey_questions("1234"),
    "you may not have the\nrequired authorization"
  )
})

# Restore the credentials for other tests:
qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)

