test_that("qualtRicsConfigFile() cats a message to console", {
  # Call getsurvey
  expect_output(qualtRics::qualtRicsConfigFile(),
                 "Copy-paste the lines between the dashes into a new plain text file, replace the values for the api_token and root_url") # nolint
})
