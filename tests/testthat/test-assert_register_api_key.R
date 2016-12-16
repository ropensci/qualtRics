# Test if

test_that("RegisterApiKey returns TRUE", {
  expect_error(assert_apikey_stored(), "register your qualtrics API key")
})
