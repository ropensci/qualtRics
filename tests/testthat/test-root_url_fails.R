test_that("Registering root url that doesn't end with '.qualtrics.com' fails.", {
  # Store dummy key
  expect_error(qualtRics::registerOptions(api_token="1234", root_url="abcd"),
               "The qualtrics root url must end with '.qualtrics.com'. Your root url looks like this: 'abcd'. Please visit https://api.qualtrics.com/docs/root-url for instructions about the qualtrics root url.")
})
