test_that("RegisterApiKey returns TRUE", {
  # This should fail because no key was stored
  expect_error(assert_apikey_stored(tempdir()), "register your qualtrics API key")
  # Store dummy key
  qualtRics::registerApiKey("ABCD")
  # Now expect this to be true
  expect_true(assert_apikey_stored(tempdir()))
})
