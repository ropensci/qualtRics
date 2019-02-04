test_that("Save directory exists for getSurvey()", {
  expect_error(qualtRics::getSurvey("1234",
                                    save_dir="/users/jasper/desktop/idonotexist"),
               "does not exist.")
})
