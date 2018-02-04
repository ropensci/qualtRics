context("Get a list of surveys that the user has access to on Qualtrics")

# Test - use mock API from httptest package
with_mock_api({
  test_that("getSurveys() returns a data frame with columns 'id', 'name', 'ownerId', 'lastModified', 'isActive' columns", {
    testthat::skip_on_cran()
    registerOptions(root_url="t.qualtrics.com", api_token="1234")
    # Get survey
    surveys <- getSurveys()
    # TESTS
    expect_that(names(surveys),
                is_identical_to(c("id","name","ownerId","lastModified","isActive")))
    expect_equal(nrow(surveys), 1)
    expect_type(surveys, "list")
  })
})
