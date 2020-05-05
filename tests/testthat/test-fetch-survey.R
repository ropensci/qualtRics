library(webmockr)

context("Download a survey from qualtRics and pull it into R using the fetch_survey() function") # nolint

test_that("fetch_survey() should make proper request with default params to start response export", { # nolint
  webmockr::enable()
  # We mock dependent functions since we are only testing the requests
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::download_qualtrics_export` =
               function(url, id, verbose) tempdir())
  local_mock(`qualtRics::read_survey` =
               function(path="", import_id="", time_zone="") NULL)
  local_mock(`qualtRics::infer_data_types` = function(data, id) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` = function(res) list(
    OK = TRUE,
    content = list(
      result = list(
        progressId = '1'
      )
    )
  ))


  mock_key <- 'api_key_123'
  mock_host <- 'test.qualtrics.com'
  mock_id <- '1234'
  expected_path <- sprintf('/API/v3/surveys/%s/export-responses/', mock_id)
  qualtrics_api_credentials(mock_key, mock_host)
  # Stub our request to check if expected request was made
  stub_request('POST', paste0(mock_host, expected_path)) %>%
    wi_th(
      headers = list('X-API-TOKEN' = mock_key,
                     'Content-Type' = 'application/json',
                     'Accept' = '*/*',
                     'accept-encoding' = 'gzip, deflate'),
      body = list(
        useLabels=TRUE,
        includeDisplayOrder=TRUE,
        breakoutSets=TRUE,
        format="csv")
    )

  # Will throw error if expected request not made
  reqIsSuccessful <-
    suppressWarnings(fetch_survey(mock_id, force_request = TRUE)) %>%
    is.null()

  expect_true(reqIsSuccessful)

  stub_registry_clear()
  webmockr::disable()
})

test_that("fetch_survey() should send the proper payload with custom params", {
  webmockr::enable()
  # We mock dependent functions since we are only testing the requests
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::download_qualtrics_export` =
               function(url, id, verbose) tempdir())
  local_mock(`qualtRics::read_survey` =
               function(path="", import_id="", time_zone="") NULL)
  local_mock(`qualtRics::infer_data_types` = function(data, id) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` = function(res) list(
    OK = TRUE,
    content = list(
      result = list(
        progressId = '1'
      )
    )
  ))


  mock_key <- 'api_key_123'
  mock_host <- 'test.qualtrics.com'
  mock_id <- '1234'
  expected_path <- sprintf('/API/v3/surveys/%s/export-responses/', mock_id)
  qualtrics_api_credentials(mock_key, mock_host)
  # Stub our request to check if expected request was made correctly
  stub_request('POST', paste0(mock_host, expected_path)) %>%
    wi_th(
      headers = list('X-API-TOKEN' = mock_key,
                     'Content-Type' = 'application/json',
                     'Accept' = '*/*',
                     'accept-encoding' = 'gzip, deflate'),
      body = list(
        useLabels=TRUE,
        format="csv",
        startDate = "2019-01-01T00:00:00Z",
        endDate = "2020-01-01T00:00:00Z",
        limit = 100,
        breakoutSets=FALSE,
        includeDisplayOrder=TRUE,
        seenUnansweredRecode=-999,
        multiselectSeenUnansweredRecode=-888,
        questionIds=c('QID1')
      )
    )

  # Will throw error if expected request not made as specified
  reqIsSuccessful <-
    suppressWarnings(fetch_survey(
      mock_id,
      force_request = TRUE,
      label=TRUE,
      start_date="2019-01-01",
      end_date="2020-01-01",
      limit = 100,
      breakout_sets = FALSE,
      unanswer_recode=-999,
      unanswer_recode_multi=-888,
      include_questions=c('QID1')
      )) %>%
    is.null()

  expect_true(reqIsSuccessful)

  stub_registry_clear()
  webmockr::disable()
})

test_that("using fetch_survey() with a base URL that doesn't end with '.qualtrics.com' fails", { # nolint
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
  # This should error on check_params()
  expect_error(
    qualtRics::fetch_survey(),
    "The Qualtrics base URL must end with"
  ) # nolint
})

test_that("fetch_survey() reads a stored survey in temporary directory if exists", { # nolint
  # Register dummy key
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "https://yourdatacenterid.qualtrics.com"
  )
  # Store RDS file
  data <- "SUCCESS"
  curr.wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(curr.wd))
  saveRDS(data, "surveyID.rds")
  # Query with all options
  expect_message(
    qualtRics::fetch_survey("surveyID"),
    "Found an earlier download for survey with id surveyID. Loading this file.
Set 'force_request' to TRUE if you want to override this"
  ) # nolint
})

test_that("Save directory exists for fetch_survey()", {
  expect_error(
    qualtRics::fetch_survey("1234",
      save_dir = "/users/jasper/desktop/idonotexist"
    ),
    "does not exist."
  )
})

test_that("Limit cannot be less than one", {
  expect_error(
    qualtRics::fetch_survey("1234", limit = 0),
    "Limit parameter should be at least 1"
  )
})

test_that("Handle convert and label conditions", {
  expect_error(
    fetch_survey("1234", label = FALSE),
    "To convert to factors, we need the Qualtrics labels."
  )
})

test_that("unanswer_recode is integer-ish", {
  qualtrics_api_credentials(
    api_key = "1234",
    base_url = "yourdatacenterid.qualtrics.com"
  )
  # Call fetch_survey
  expect_error(
    fetch_survey("1234", unanswer_recode = "hello"),
    "unanswer_recode must be an integer-like scalar"
  )
})
