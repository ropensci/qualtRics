test_that("absence of API key or base URL raises an error", {
  Sys.setenv("QUALTRICS_API_KEY" = "")
  Sys.setenv("QUALTRICS_BASE_URL" = "")
  expect_error(
    check_credentials(),
    "Qualtrics API key and/or base URL need registering"
  )
})

test_that("can store and access credentials", {
  # Store dummy credentials in environment

  expect_error(
    qualtrics_api_credentials(api_key = "1234", base_url = "abcd"),
    "`base_url` must be of the form"
  )

  # Removes protocol with message
  expect_message(
    qualtrics_api_credentials(api_key = "1234",
                              base_url = "https://abcd.qualtrics.com"),
    "Protocol"
  )
  expect_false(
    startsWith(Sys.getenv("QUALTRICS_BASE_URL" ), "https://")
  )

  # Removes the trailing slash:
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd.qualtrics.com/")
  # Now expect this to no longer have slash
  expect_false(
    endsWith(Sys.getenv("QUALTRICS_BASE_URL" ), "/")
  )

  # Checks pass with correct credentials:
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd.qualtrics.com")
  # Now expect this to be NULL
  expect_null(
    check_credentials()
  )
})

test_that("qualtRicsConfigFile() gives a warning", {
  expect_warning(qualtRics::qualtRicsConfigFile(), "deprecated")
})

# Restore the credentials for other tests:
qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)
