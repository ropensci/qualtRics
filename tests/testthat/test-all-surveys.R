context("Get a list of all surveys that the user has access to on Qualtrics")

# Test - use mock API from httptest package
with_mock_api({
  test_that("getSurveys() returns a data frame with columns 'id', 'name', 'ownerId', 'lastModified', 'isActive' columns", { # nolint
    testthat::skip_on_cran()
    qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")
    # Get survey
    surveys <- all_surveys()
    # TESTS
    expect_that(names(surveys),
                is_identical_to(c("id","name","ownerId","lastModified","isActive"))) # nolint
    expect_equal(nrow(surveys), 1)
    expect_type(surveys, "list")
  })
})

test_that("getSurveys() throws an error", {
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234", base_url = "yourdatacenterid.qualtrics.com")
  # This should fail in 'do.call'
  expect_error(all_surveys(),
               "you may not have the\nrequired authorization")
})
