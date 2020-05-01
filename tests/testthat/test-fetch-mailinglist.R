
test_that("fetch_mailinglist returns a tbl_df with expected column names and types", {

  vcr::use_cassette("fetch_mailinglist", {
    x <- fetch_mailinglist()
  })

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
  expect_type(x$unsubscribed, "character")

})
