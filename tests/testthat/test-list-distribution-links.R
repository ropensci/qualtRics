skip_on_cran()
## mock server is returning 500 on 2024-08-16:
skip_on_ci()

test_that("list_distribution_links() returns a tbl_df with expected column names and types", {
  local_mocked_bindings(
    glue_api_v3 = function(base_url)
      "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60919"
  )

  x <- list_distribution_links("EMD_abcdef123456789", "SV_abcdef123456789")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$linkExpiration, c("POSIXct", "POSIXt"))

  expect_type(x$contactId, "character")
  expect_type(x$transactionId, "character")
  expect_type(x$link, "character")
  expect_type(x$exceededContactFrequency, "logical")
  expect_type(x$status, "character")
  expect_type(x$lastName, "character")
  expect_type(x$firstName, "character")
  expect_type(x$externalDataReference, "character")
  expect_type(x$email, "character")
  expect_type(x$unsubscribed, "logical")
})
