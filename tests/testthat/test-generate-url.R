test_that("all surveys URL", {
  Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")
  expect_equal(
    generate_url("allsurveys"),
    "https://www.qualtrics.com/API/v3/surveys/"
  )
})


test_that("fetch surveys URL", {
  Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")
  expect_equal(
    generate_url("exportresponses", surveyID = "abcdefg"),
    "https://www.qualtrics.com/API/v3/surveys/abcdefg/export-responses/"
  )
})

test_that("fetch distributions URL", {
  Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")
  expect_equal(
    generate_url("fetchdistributions", surveyID = "potato"),
    "https://www.qualtrics.com/API/v3/distributions?surveyId=potato"
  )
})

test_that("error on non-existent URL", {
  Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")
  expect_error(
    generate_url("potatoURL", surveyID = "potato"),
    "Internal error: invalid URL generation query"
  )
})
