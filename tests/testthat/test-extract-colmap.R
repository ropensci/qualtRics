context("Column mapping for one survey from response download (v3 approach)")

qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")

test_that("extract_colmap() retrieves an appropriate column map generated within read_survey", {

  vcr::use_cassette("extract_colmap", {
    x <- fetch_survey("SV_6s93xhVtm1e4j3v", force_request = TRUE, add_column_map = TRUE)
  })

  cm <- extract_colmap(x)

  expect_s3_class(cm, c("tbl_df","tbl","data.frame"))
  expect_named(cm, c("qname", "description", "main", "sub", "ImportId", "timeZone", "choiceId"))
  expect_type(cm$qname, "character")
  expect_type(cm$description, "character")
  expect_type(cm$main, "character")
  expect_type(cm$sub, "character")
  expect_type(cm$ImportId, "character")
  expect_type(cm$timeZone, "character")
  expect_type(cm$choiceId, "character")

})
