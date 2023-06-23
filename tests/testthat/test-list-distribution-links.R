test_that("list_distribution_links() returns a tbl_df with expected column names and types", {

  skip_on_cran()

  vcr::use_cassette("list_distribution_links", {
    x <- list_distribution_links("EMD_RmjB8Gfpjlki6ms", "SV_06PSFx0kpItCz0q")
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$linkExpiration,  c("POSIXct", "POSIXt"))

  expect_type(x$contactId, "character")
  expect_type(x$transactionId, "character")
  expect_type(x$link, "character")
  expect_type(x$exceededContactFrequency, "character")
  expect_type(x$status, "character")
  expect_type(x$lastName, "character")
  expect_type(x$firstName, "character")
  expect_type(x$externalDataReference, "character")
  expect_type(x$email, "character")
  expect_type(x$unsubscribed, "character")

})
