context("Column mapping for one survey")

test_that("column_map() retrieves survey column mapping", {

  qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")

  vcr::use_cassette("column_map", {
    x <- column_map("SV_5BJRo2RGHajIlOB")
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_named(x, c("qname", "qid", "choice", "textEntry"))
  expect_type(x$qname, "character")
  expect_type(x$qid, "character")
  expect_type(x$choice, "character")
  expect_type(x$textEntry, "character")

})
