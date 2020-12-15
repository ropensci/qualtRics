context("All surveys that the user has access to on Qualtrics")

qualtrics_api_credentials(api_key = "1234", base_url = "www.qualtrics.com")

test_that("all_surveys() sends the proper request to Qualtrics", {

  vcr::use_cassette("all_surveys", {
    x <- all_surveys()
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_named(x, c("id", "name", "ownerId", "lastModified",
                    "creationDate", "isActive"))
  expect_type(x$id, "character")
  expect_type(x$name, "character")
  expect_type(x$ownerId, "character")
  expect_type(x$lastModified, "character")
  expect_type(x$creationDate, "character")
  expect_type(x$isActive, "logical")

})

test_that("all_surveys() throws an error", {

  expect_error(
    all_surveys(),
    "you may not have the\nrequired authorization"
  )
})
