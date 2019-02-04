test_that("using getSurvey() with a base URL that doesn't end with '.qualtrics.com' fails", { # nolint
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  # This should error on checkParams
  expect_error(qualtRics::getSurveys(),
               "The Qualtrics base URL must end with") # nolint
})
