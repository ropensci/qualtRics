context("Get survey questions for a survey")

# Test - use mock API from httptest package
with_mock_api({
  test_that("survey_questions() returns survey questions", {
    testthat::skip_on_cran()
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    # Get survey questions
    surveys <- survey_questions("SV_5u9zu8zHnHaGml7")
    # TESTS
  })
})

test_that("survey_questions() throws an error", {
  # Store dummy key
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "https://yourdatacenterid.qualtrics.com"
  )
  # This should fail in 'do.call'
  expect_error(
    qualtRics::survey_questions("1234"),
    "you may not have the\nrequired authorization"
  )
})

with_mock_api({
  test_that("getSurveyQuestions() throws a warning", {
    testthat::skip_on_cran()
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    expect_warning(getSurveyQuestions("SV_5u9zu8zHnHaGml7"),
      "deprecated",
      fixed = FALSE
    )
  })
})
