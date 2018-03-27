test_that("Registering root url that doesn't end with '.qualtrics.com' fails.", { # nolint
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", base_url="abcd")
  # This should error on checkParams
  expect_error(qualtRics::getSurveys(),
               "The qualtrics root url must end with '.qualtrics.com'. Your root url looks like this: 'abcd'.\nPlease visit https://api.qualtrics.com/docs/root-url for instructions about the qualtrics root url.") # nolint
})
