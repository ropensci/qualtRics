test_that("generate survey URLs", {
  withr::with_envvar(
    new = c(
      "QUALTRICS_API_KEY" = "1234",
      "QUALTRICS_BASE_URL" = "www.qualtrics.com"
    ),
    {
      expect_equal(
        generate_url("allsurveys"),
        "https://www.qualtrics.com/API/v3/surveys"
      )
      expect_equal(
        generate_url("exportresponses", surveyID = "abcdefg"),
        "https://www.qualtrics.com/API/v3/surveys/abcdefg/export-responses/"
      )
      expect_equal(
        generate_url("fetchdistributions", surveyID = "potato"),
        "https://www.qualtrics.com/API/v3/distributions?surveyId=potato"
      )
      expect_equal(
        generate_url("fetchsurveyversions", surveyID = "abcdefg"),
        "https://www.qualtrics.com/API/v3/survey-definitions/abcdefg/versions"
      )
      expect_equal(
        generate_url(
          "fetchdescription_version",
          surveyID = "abcdefg",
          versionID = "123456"
        ),
        "https://www.qualtrics.com/API/v3/survey-definitions/abcdefg/versions/123456"
      )
      expect_error(
        generate_url("potatoURL", surveyID = "potato"),
        "Internal error: invalid URL generation query"
      )
    }
  )
})
