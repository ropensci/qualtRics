skip_on_cran()
skip_on_ci()

test_that("fetch_mailinglist returns a tbl_df with expected column names and types", {
  local_mocked_bindings(glue_api_v3 = function(base_url) "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60928")
  local_mocked_bindings(paginate_api_request = function(fetch_url) qualtrics_api_request("GET", url = fetch_url)$result$elements)

  x <- fetch_mailinglist("ML_abcdef123456789")
  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_true(all(c("id", "firstName", "lastName", "email",
                    "externalDataReference", "language",
                    "unsubscribed")
                  %in% names(x)))
  expect_type(x$id, "character")
  expect_type(x$firstName, "character")
  expect_type(x$lastName, "character")
  expect_type(x$email, "character")
  expect_type(x$externalDataReference, "character")
  expect_type(x$language, "character")
  expect_type(x$unsubscribed, "logical")

})
