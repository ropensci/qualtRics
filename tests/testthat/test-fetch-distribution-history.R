context("Fetch data for a survey distribution")

test_that("fetch_distribution_history() returns a tbl_df with expected column names and types", {

  skip_on_cran()

  vcr::use_cassette("fetch_distribution_history", {
    x <- fetch_distribution_history("EMD_RmjB8Gfpjlki6ms")
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$responseCompletedAt,  c("POSIXct", "POSIXt"))
  expect_s3_class(x$sentAt,  c("POSIXct", "POSIXt"))
  expect_s3_class(x$openedAt,  c("POSIXct", "POSIXt"))
  expect_s3_class(x$responseStartedAt,  c("POSIXct", "POSIXt"))

  expect_type(x$contactId, "character")
  expect_type(x$contactLookupId, "character")
  expect_type(x$distributionId, "character")
  expect_type(x$status, "character")
  expect_type(x$surveyLink, "character")
  expect_type(x$contactFrequencyRuleId, "character")
  expect_type(x$responseId, "character")
  expect_type(x$surveySessionId, "character")

})

qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)
qq<-qualtRics:::qualtrics_api_request("GET", url = Sys.getenv("QUALTRICS_BASE_URL"))
