library(webmockr)

context("Get survey questions for a survey")

survey_questions_factory <- list(
  response = list(
    OK = TRUE,
    content = list(
      result = list(
        questions = list(
          QID1 = list(
            questionName = "questionName1",
            questionText = "questionText1",
            validation   = list(
              doesForceResponse = FALSE
            )
          ),
          QID2 = list(
            questionName = "questionName2",
            questionText = "questionText2",
            validation   = list(
              doesForceResponse = FALSE
            )
          )
        )
      )
    )
  )
)

test_that("survey_questions() makes a request with expected structure, and parses response", { # nolint
  webmockr::enable()
  # define mocks since we are only testing request
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` =
               function(resp) survey_questions_factory$response)

  # Define expectations
  expected <- list(
    verb     = "GET",
    host     = "https://test.qualtrics.com",
    path     = "/API/v3/surveys/",
    id       = "sv1",
    api_key  = "1234",
    headers  = list(
      'X-API-TOKEN' = "1234",
      'Content-Type' = 'application/json',
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )
  )

  qualtrics_api_credentials(expected$api_key, expected$host)

  stub_request(
    expected$verb, paste0(expected$host, expected$path, expected$id)
   ) %>%
    wi_th(headers = expected$headers) %>%
    to_return(status=200)

  # Call will fail test if request does not match expectations
  expect_is(survey_questions(expected$id), "tbl_df")

  webmockr::stub_registry_clear()
  webmockr::disable()
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

test_that("getSurveyQuestions() throws a warning", {
  testthat::skip_on_cran()
  Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
  webmockr::enable()
  # define mocks since we are only testing for warning
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` =
               function(resp) survey_questions_factory$response)

  # Define expectations
  expected <- list(
    verb     = "GET",
    host     = "https://test.qualtrics.com",
    path     = "/API/v3/surveys/",
    id       = "sv1",
    api_key  = "1234",
    headers  = list(
      'X-API-TOKEN' = "1234",
      'Content-Type' = 'application/json',
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )
  )

  qualtrics_api_credentials(expected$api_key, expected$host)

  stub_request(
    expected$verb, paste0(expected$host, expected$path, expected$id)
  ) %>%
    wi_th(headers = expected$headers) %>%
    to_return(status=200)

  expect_warning(getSurveyQuestions(expected$id),
                 "deprecated",
                 fixed = FALSE
  )

  webmockr::stub_registry_clear()
  webmockr::disable()
})
