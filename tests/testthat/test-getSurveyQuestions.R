context("Get survey questions for a survey")

# Test - use mock API from httptest package
with_mock_api({
  test_that("getSurveyQuestions() returns survey questions", {
    testthat::skip_on_cran()
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE)
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    # Get survey
    surveys <- getSurveyQuestions("SV_5u9zu8zHnHaGml7")
    # TESTS
  })
})
