test_that("Save directory exists for getSurvey()", {
  # Store dummy key
  qualtRics::registerOptions(api_token="1234", root_url="abcd.qualtrics.com")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", save_dir="/users/jasper/desktop/idonotexist"),
               "does not exist.")
})
