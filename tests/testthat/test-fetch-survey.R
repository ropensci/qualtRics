context("Download a survey from qualtRics and pull it into R using the fetch_survey() function") # nolint

# Test - use mock API from httptest package
with_mock_api({
  test_that("fetch_survey() returns a survey", {
    testthat::skip_on_cran()
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    # Get survey
    surveys <- fetch_survey("SV_5u9zu8zHnHaGml7")
    # TESTS
  })
})

test_that("fetch_survey() throws error", {
  # Store dummy key
  qualtRics::registerOptions(
    api_token = "1234",
    root_url = "https://yourdatacenterid.qualtrics.com"
  )
  # Query fake ID with generic root url
  expect_error(
    qualtRics::fetch_survey("1234"),
    "you may not have the\nrequired authorization"
  )
})

test_that("using fetch_survey() with a base URL that doesn't end with '.qualtrics.com' fails", { # nolint
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  # This should error on check_params()
  expect_error(
    qualtRics::fetch_survey(),
    "The Qualtrics base URL must end with"
  ) # nolint
})

test_that("fetch_survey() reads a stored survey in temporary directory if exists", { # nolint
  # Register dummy key
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "https://yourdatacenterid.qualtrics.com"
  )
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
  ) # nolint
})

test_that("Save directory exists for fetch_survey()", {
  expect_error(
    qualtRics::fetch_survey("1234",
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

test_that("unanswer_recode is a string", {
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "yourdatacenterid.qualtrics.com"
  )
  # Call fetch_survey
  expect_error(
    fetch_survey("1234",
      unanswer_recode = 123
    ),
    "unanswer_recode is not a string"
  )
})
