test_that("RegisterApiKey returns TRUE", {
  # This should fail because no key was stored
  Sys.setenv("QUALTRICS_API_KEY"="")
  expect_error(assert_apikey_stored(), "You need to register your qualtrics API key")
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="abcd")
  # Now expect this to be true
  expect_true(assert_apikey_stored())
})
