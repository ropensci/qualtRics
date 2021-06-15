context("List distribution links of survey by ID")

test_that("list_distribution_links returns a tbl_df with expected column names and types", {

  vcr::use_cassette("list_distribution_links", {
    x <- list_distribution_links(distributionID = "",
                                 surveyID = "")
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_true(all(c("contactId", "link", "exceededContactFrequency", "linkExpiration",
                    "lastName", "firstName", "externalDataReference", "email", "unsubscribed")
                  %in% names(x)))
  expect_type(x$contactId, "character")
  expect_type(x$link, "character")
  expect_type(x$exceededContactFrequency, "character")
  expect_type(x$linkExpiration, "character")
  expect_type(x$lastName, "character")
  expect_type(x$firstName, "character")
  expect_type(x$externalDataReference, "character")
  expect_type(x$email, "character")
  expect_type(x$unsubscribed, "character")

})
