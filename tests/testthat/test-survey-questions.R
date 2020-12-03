
context("Get survey questions for a survey")

qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")

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

test_that("survey_questions() throws an error", {

  expect_error(
    qualtRics::survey_questions("1234"),
    "you may not have the\nrequired authorization"
  )
})
