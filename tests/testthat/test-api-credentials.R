context("API credentials")


test_that("absence of API key or base URL raises an error", {
  Sys.setenv("QUALTRICS_API_KEY" = "")
  Sys.setenv("QUALTRICS_BASE_URL" = "")
  expect_error(
    assert_api_key(),
    "You need to register your Qualtrics API key"
  )
  expect_error(
    assert_base_url(),
    "You need to register your Qualtrics API key"
  )
})

test_that("can store and access credentials", {
  # Store dummy credentials in environment
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  expect_error(
    assert_base_url(),
    "The Qualtrics base URL must end with"
  )

  qualtrics_api_credentials(api_key = "1234", base_url = "https://abcd.qualtrics.com")
  expect_error(
    assert_base_url(),
    "The Qualtrics base URL must not include"
  )

  qualtrics_api_credentials(api_key = "1234", base_url = "abcd.qualtrics.com")
  # Now expect this to be true
  expect_true(assert_api_key())
  expect_true(assert_base_url())
})

test_that("qualtRicsConfigFile() gives a warning", {
  expect_warning(qualtRics::qualtRicsConfigFile(), "deprecated") # nolint
})

# Restore the credentials for other tests:
qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)
