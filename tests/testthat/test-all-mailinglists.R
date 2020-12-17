context("All mailing lists that the user has access to on Qualtrics")

qualtrics_api_credentials(api_key = "1234", base_url = "www.qualtrics.com")

test_that("all_mailinglists returns a tbl_df with expected column names and types", {

  vcr::use_cassette("all_mailinglists", {
    x <- all_mailinglists()
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_named(x, c("libraryId", "id", "name", "category", "folder"))
  expect_type(x$libraryId, "character")
  expect_type(x$id, "character")
  expect_type(x$name, "character")
  expect_type(x$category, "character")
  expect_type(x$folder, "character")

})
