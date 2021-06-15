context("Fetch distribution history of survey distribution by ID")

test_that("fetch_distribution_history returns a tbl_df with expected column names and types", {

  vcr::use_cassette("fetch_distribution_history", {
    x <- fetch_distribution_history(distributionID = "",
                                    surveyID = "")
  })

  expect_s3_class(x, c("tbl_df","tbl","data.frame"))
  expect_true(all(c("contactId", "contactLookupId", "distributionID", "status",
                    "surveyLink", "contactFrequencyRuleId", "responseId", "responseCompletedAt",
                    "sentAt", "openedAt", "responseStartedAt", "surveySessionId", "email",
                    "linkExpiration", "lastName", "firstName", "externalDataReference", "unsubscribed"
                  %in% names(x))))
  expect_type(x$contactId, "character")
  expect_type(x$contactLookupId, "character")
  expect_type(x$distributionID, "character")
  expect_type(x$status, "character")
  expect_type(x$surveyLink, "character")
  expect_type(x$contactFrequencyRuleId, "character")
  expect_type(x$responseId, "character")
  expect_type(x$responseCompletedAt, "character")
  expect_type(x$sentAt, "character")
  expect_type(x$openedAt, "character")
  expect_type(x$responseStartedAt, "character")
  expect_type(x$surveySessionId, "character")
  expect_type(x$email, "character")
  expect_type(x$linkExpiration, "character")
  expect_type(x$lastName, "character")
  expect_type(x$firstName, "character")
  expect_type(x$externalDataReference, "character")
  expect_type(x$unsubscribed, "character")

})
