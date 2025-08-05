test_that("fetch_survey() returns survey with default params", {
  skip_on_cran()

  vcr::use_cassette("fetch_survey", {
  x <- fetch_survey("SV_3gbwq8aJgqPwQDP")
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_named(
    x,
    c(
      "StartDate",
      "EndDate",
      "Status",
      "IPAddress",
      "Progress",
      "Duration (in seconds)",
      "Finished",
      "RecordedDate",
      "ResponseId",
      "RecipientLastName",
      "RecipientFirstName",
      "RecipientEmail",
      "ExternalReference",
      "LocationLatitude",
      "LocationLongitude",
      "DistributionChannel",
      "UserLanguage",
      "Q63",
      "Q16",
      "Q17",
      "Q18",
      "Q19",
      "Q22",
      "SolutionRevision"
    )
  )
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
    x <-
      fetch_survey("SV_0pK7FIIGNNM0sNn",
    start_date = "2015-01-01",
    end_date = "2022-06-02 18:40:53",
    unanswer_recode = 999,
    limit = 15,
    include_questions = c("QID16", "QID17"),
    include_metadata = c("StartDate", "EndDate", "ResponseId"),
    breakout_sets = FALSE,
    col_types = readr::cols(EndDate = readr::col_character())
  )
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 15)
  expect_named(
    x,
    c("StartDate", "EndDate", "ResponseId", "Q16", "Q17", "SolutionRevision")
  )
  expect_type(x$StartDate, "double")
  expect_type(x$EndDate, "character")
  expect_type(x$ResponseId, "character")
  expect_type(x$Q16, "integer")
  expect_type(x$Q17, "character")
  expect_type(x$SolutionRevision, "double")
})

test_that("fetch_survey() excludes variable classes when requested", {
  skip_on_cran()

  vcr::use_cassette("fetch_survey_exclude", {
    x <-
      fetch_survey("SV_0pK7FIIGNNM0sNn",
    start_date = "2015-01-01",
    end_date = "2022-06-02 18:40:53",
    unanswer_recode = 999,
    limit = 15,
    include_questions = NA,
    include_metadata = NA,
    breakout_sets = FALSE
  )
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 15)
  expect_named(x, c("SolutionRevision"))
  expect_type(x$SolutionRevision, "double")
})

test_that("fetch_survey() returns survey with only one QID", {
  skip_on_cran()

  vcr::use_cassette("fetch_one_qid", {
  x <- fetch_survey(
    "SV_3gbwq8aJgqPwQDP",
    limit = 15,
    include_questions = c("QID17"),
    breakout_sets = FALSE
  )
  })

  expect_s3_class(x, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 15)
  expect_named(
    x,
    c(
      "StartDate",
      "EndDate",
      "Status",
      "IPAddress",
      "Progress",
      "Duration (in seconds)",
      "Finished",
      "RecordedDate",
      "ResponseId",
      "RecipientLastName",
      "RecipientFirstName",
      "RecipientEmail",
      "ExternalReference",
      "LocationLatitude",
      "LocationLongitude",
      "DistributionChannel",
      "UserLanguage",
      "Q17",
      "SolutionRevision"
    )
  )
  expect_type(x$Status, "character")
  expect_type(x$`Duration (in seconds)`, "double")
  expect_type(x$Finished, "logical")
  expect_type(x$ResponseId, "character")
  expect_type(x$Q17, "character")
})

test_that("Limit cannot be less than one", {
  expect_error(
    qualtRics::fetch_survey("1234", limit = 0),
    "1 or greater"
  )
})

test_that("Handle convert and label conditions", {
  expect_error(
    fetch_survey("1234", label = FALSE),
    "Error in arguments `convert` & `label`:"
  )
})

test_that("unanswer_recode is integer-ish", {
  # Call fetch_survey
  expect_error(
    fetch_survey("1234", unanswer_recode = "hello"),
    "must be a single integer"
  )
})

test_that("correct error for deprecated args", {
  skip_on_cran()

  expect_snapshot(
    fetch_survey("1234", force_request = TRUE),
    error = TRUE
  )
  expect_snapshot(
    fetch_survey("1234", save_dir = "~/Desktop"),
    error = TRUE
  )
})

test_that("error if bad temporary directory", {
  skip_on_cran()

  expect_error(
    fetch_survey("1234", tmp_dir = "/unrealistictempdirectory/"),
    "not an existing directory"
  )
})
