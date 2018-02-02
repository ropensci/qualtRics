context("Get survey questions for a survey")

# Test - use mock API from httptest package
with_mock_api({
  test_that("getSurveyQuestions() returns survey questions", {
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE)
    registerOptions(root_url="noleadtrialau1.au1.qualtrics.com", api_token="1234")
    # Get survey
    surveys <- getSurveyQuestions("SV_5u9zu8zHnHaGml7")
    # TESTS
  })
})
