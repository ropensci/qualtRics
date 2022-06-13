context("Download in progress responses from qualtRics and pull it into R using the fetch_in_progress() function")

test_that("fetch_in_progress() returns survey with default params", {

  skip_on_cran()

  vcr::use_cassette("fetch_in_progress", {
    x <- fetch_in_progress("SV_3FdDswpcO88rSxE")
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_type(x$StartDate, "double")
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")

})

test_that("fetch_in_progress() returns survey with custom params", {

  skip_on_cran()

  vcr::use_cassette("fetch_in_progress_custom", {
    x <- fetch_in_progress(
      "SV_3FdDswpcO88rSxE",
      force_request = TRUE,
      start_date = "2022-06-10",
      end_date = "2022-06-12",
      unanswer_recode = 999,
      limit = 15,
      breakout_sets = FALSE,
      col_types = readr::cols(EndDate = readr::col_character()),
    )
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_equal(nrow(x), 15)
  expect_type(x$StartDate, "double")
  expect_type(x$EndDate, "character")
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_type(x$demGender, "integer")

})

test_that("fetch_in_progress() returns survey with only one QID", {

  skip_on_cran()

  vcr::use_cassette("fetch_in_progress_one_qid", {
    x <- fetch_in_progress(
      "SV_3FdDswpcO88rSxE",
      force_request = TRUE,
      limit = 15,
      include_questions = c("QID23"),
      breakout_sets = FALSE
    )
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df","tbl","data.frame"))
  expect_equal(nrow(x), 15)
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_type(x$demGender, "integer")

})

test_that("fetch_in_progress() reads a stored survey in temporary directory if exists", {
  # Store RDS file
  data <- "SUCCESS"
  curr.wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(curr.wd))
  saveRDS(data, "surveyID_in_progress.rds")
  # Query with all options
  expect_message(
    qualtRics::fetch_in_progress("surveyID"),
    "Found an earlier download for survey with id surveyID. Loading this file.
Set 'force_request' to TRUE if you want to override this"
  )
})

test_that("Save directory exists for fetch_in_progress()", {
  expect_error(
    qualtRics::fetch_in_progress(
      "1234",
      save_dir = "/Users/jamesmartherus/desktop/idonotexist"
    ),
    "does not exist."
  )
})

test_that("Limit cannot be less than one", {
  expect_error(
    qualtRics::fetch_in_progress("1234", limit = 0),
    "Limit parameter should be at least 1"
  )
})

test_that("Handle convert and label conditions", {
  expect_error(
    fetch_in_progress("1234", label = FALSE),
    "To convert to factors, we need the Qualtrics labels."
  )
})

test_that("unanswer_recode is integer-ish", {
  # Call fetch_in_progress
  expect_error(
    fetch_in_progress("1234", unanswer_recode = "hello"),
    "unanswer_recode must be an integer-like scalar"
  )
})

test_that("using fetch_in_progress() with a base URL that doesn't end with '.qualtrics.com' fails", {

  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  # This should error on check_params()
  expect_error(
    qualtRics::fetch_in_progress(),
    "The Qualtrics base URL must end with"
  ) # nolint


})

# Restore the credentials for other tests:
qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)

