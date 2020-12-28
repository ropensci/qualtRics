context("All surveys that the user has access to on Qualtrics")

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


# Set to bad URL:
qualtrics_api_credentials(api_key = holder_API,
                          base_url = "t.qualtrics.com")

test_that("all_surveys() throws an error when URL is bad", {

  expect_error(
    all_surveys(),
    "you may not have the\nrequired authorization"
  )
})

# Reset the URL:
qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)

