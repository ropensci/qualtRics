context("Get a survey from qualtRics and pull it into R using the getSurvey() function") # nolint

# Test - use mock API from httptest package
with_mock_api({
  test_that("getSurvey() returns a survey", {
    testthat::skip_on_cran()
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE)
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    # Get survey
    surveys <- getSurvey("SV_5u9zu8zHnHaGml7")
    # TESTS
  })
})
