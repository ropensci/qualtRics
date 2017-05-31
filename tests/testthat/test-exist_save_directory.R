test_that("Save directory exists for getSurvey()", {
  # Register dud API key
  qualtRics::registerApiKey("ABCD")
  # Call getsurvey
  expect_error(qualtRics::getSurvey("1234", root_url="test", save_dir="/users/jasper/desktop/idonotexist"),
               "does not exist.")
})
