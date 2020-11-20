context("Download a survey from qualtRics and pull it into R using the fetch_survey() function")

qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")

test_that("fetch_survey() returns survey with default params", {

  skip_on_cran()

  vcr::use_cassette("fetch_survey", {
    x <- fetch_survey("SV_3gbwq8aJgqPwQDP")
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_named(x, c("StartDate", "EndDate", "Status", "IPAddress", "Progress",
                    "Duration (in seconds)", "Finished", "RecordedDate",
                    "ResponseId", "RecipientLastName", "RecipientFirstName",
                    "RecipientEmail", "ExternalReference", "LocationLatitude",
                    "LocationLongitude", "DistributionChannel", "UserLanguage",
                    "Q63", "Q16", "Q17", "Q18", "Q19", "Q22", "SolutionRevision"))
  expect_type(x$StartDate, "double")
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_type(x$Q63, "integer")
  expect_type(x$Q22, "character")

})

test_that("fetch_survey() returns survey with custom params", {

  skip_on_cran()

  vcr::use_cassette("fetch_survey_custom", {
    x <- fetch_survey(
      "SV_56icaa9YAafpAqx",
      force_request = TRUE,
      start_date = "2020-01-01",
      end_date = "2020-03-01",
      unanswer_recode = 999,
      limit = 15,
      include_questions = c("QID9", "QID21"),
      breakout_sets = FALSE,
      col_types = readr::cols(EndDate = readr::col_character()),
    )
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_equal(nrow(x), 15)
  expect_named(x, c("StartDate", "EndDate", "Status",
                    "Progress", "Duration (in seconds)", "Finished",
                    "RecordedDate", "ResponseId", "DistributionChannel",
                    "UserLanguage", "Q3.2", "Q4.1",
                    "SolutionRevision",
                    "Q3.8 - Parent Topics",
                    "Q3.8 - Sentiment Polarity",
                    "Q3.8 - Sentiment Score",
                    "Q3.8 - Sentiment",
                    "Q3.8 - Topic Sentiment Label",
                    "Q3.8 - Topic Sentiment Score",
                    "Q3.8 - Topics"))
  expect_type(x$StartDate, "double")
  expect_type(x$EndDate, "character")
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_type(x$`Q3.2`, "integer")
  expect_type(x$`Q4.1`, "integer")

})

test_that("fetch_survey() returns survey with only one QID", {

  skip_on_cran()

  vcr::use_cassette("fetch_one_qid", {
    x <- fetch_survey(
      "SV_56icaa9YAafpAqx",
      force_request = TRUE,
      limit = 15,
      include_questions = c("QID9"),
      breakout_sets = FALSE
    )
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_equal(nrow(x), 15)
  expect_named(x, c("StartDate", "EndDate", "Status",
                    "Progress", "Duration (in seconds)", "Finished",
                    "RecordedDate", "ResponseId", "DistributionChannel",
                    "UserLanguage", "Q3.2",
                    "SolutionRevision",
                    "Q3.8 - Parent Topics",
                    "Q3.8 - Sentiment Polarity",
                    "Q3.8 - Sentiment Score",
                    "Q3.8 - Sentiment",
                    "Q3.8 - Topic Sentiment Label",
                    "Q3.8 - Topic Sentiment Score",
                    "Q3.8 - Topics"))
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_s3_class(x$`Q3.2`, "factor")

})



test_that("fetch_survey() reads a stored survey in temporary directory if exists", {
  # Store RDS file
  data <- "SUCCESS"
  curr.wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(curr.wd))
  saveRDS(data, "surveyID.rds")
  # Query with all options
  expect_message(
    qualtRics::fetch_survey("surveyID"),
    "Found an earlier download for survey with id surveyID. Loading this file.
Set 'force_request' to TRUE if you want to override this"
  )
})

test_that("Save directory exists for fetch_survey()", {
  expect_error(
    qualtRics::fetch_survey(
      "1234",
      save_dir = "/users/jasper/desktop/idonotexist"
    ),
    "does not exist."
  )
})

test_that("Limit cannot be less than one", {
  expect_error(
    qualtRics::fetch_survey("1234", limit = 0),
    "Limit parameter should be at least 1"
  )
})

test_that("Handle convert and label conditions", {
  expect_error(
    fetch_survey("1234", label = FALSE),
    "To convert to factors, we need the Qualtrics labels."
  )
})

test_that("unanswer_recode is integer-ish", {
  # Call fetch_survey
  expect_error(
    fetch_survey("1234", unanswer_recode = "hello"),
    "unanswer_recode must be an integer-like scalar"
  )
})

test_that("using fetch_survey() with a base URL that doesn't end with '.qualtrics.com' fails", {

  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  # This should error on check_params()
  expect_error(
    qualtRics::fetch_survey(),
    "The Qualtrics base URL must end with"
  ) # nolint
})
