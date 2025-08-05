skip_on_cran()
skip_on_ci()

test_that("all_surveys() sends the proper request to Qualtrics", {
  local_mocked_bindings(
    glue_api_v3 = function(base_url)
      "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60937"
  )

  x <- all_surveys()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(
    x,
    c("id", "name", "ownerId", "lastModified", "creationDate", "isActive")
  )
  expect_type(x$id, "character")
  expect_type(x$name, "character")
  expect_type(x$ownerId, "character")
  expect_type(x$lastModified, "character")
  expect_type(x$creationDate, "character")
  expect_type(x$isActive, "logical")
})

test_that("all_surveys() throws an error when URL & key is bad", {
  withr::with_envvar(
    new = c(
      "QUALTRICS_API_KEY" = "abcdef",
      "QUALTRICS_BASE_URL" = "t.qualtrics.com"
    ),
    expect_snapshot_error(all_surveys())
  )
})
