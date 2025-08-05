skip_on_cran()
skip_on_ci()

test_that("fetch_distribution_history() returns a tbl_df with expected column names and types", {
  local_mocked_bindings(
    glue_api_v3 = function(base_url)
      "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60919"
  )

  x <- fetch_distribution_history("EMD_abcdef123456789")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$responseCompletedAt, c("POSIXct", "POSIXt"))
  expect_s3_class(x$sentAt, c("POSIXct", "POSIXt"))
  expect_s3_class(x$openedAt, c("POSIXct", "POSIXt"))
  expect_s3_class(x$responseStartedAt, c("POSIXct", "POSIXt"))

  expect_type(x$contactId, "character")
  expect_type(x$contactLookupId, "character")
  expect_type(x$distributionId, "character")
  expect_type(x$status, "character")
  expect_type(x$surveyLink, "character")
  expect_type(x$contactFrequencyRuleId, "character")
  expect_type(x$responseId, "character")
  expect_type(x$surveySessionId, "character")
})
