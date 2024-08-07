skip_on_cran()

test_that("all_mailinglists returns a tbl_df with expected column names and types", {
  local_mocked_bindings(glue_api_v3 = function(base_url) "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60928")

  x <- all_mailinglists()
  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_named(x, c("libraryId", "id", "name", "category", "folder"))
  expect_type(x$libraryId, "character")
  expect_type(x$id, "character")
  expect_type(x$name, "character")
  expect_type(x$category, "character")
  expect_type(x$folder, "character")

})
