context("download_qualtrics_export")

test_that("it should make the proper export progress and download file requests", { # nolint
  webmockr::enable()
  # define mocks
  mock_file_id <- 'fileId123'
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  # mock qualtrics_response_codes to return expected data for export progress
  local_mock(`qualtRics::qualtrics_response_codes` =
               function(resp, raw=FALSE) list(
                 content = list(
                   result = list(
                     percentComplete = 100,
                     fileId = mock_file_id
                   ),
                   meta = list(
                     httpStatus = "200 - OK",
                     requestId = "555b3857-db1e-492d-9288-47652c3cc6b0"
                   )
                 ),
                 OK = TRUE
               ))
  local_mock(writeBin = function(object, con) NULL)
  mock_host <- 'https://env.qualtrics.com'
  mock_key <- 'api_key'
  mock_survey_id <- '1234'
  mock_export_id <- 'export123'
  fetch_url <- sprintf(
    '%s/API/v3/surveys/%s/export-responses/',
    mock_host,
    mock_survey_id
  )

  expected_progress_url <- paste0(fetch_url, mock_export_id)
  expected_download_url <- paste0(fetch_url, mock_file_id, "/file")
  expected_header <- list(
    'X-API-TOKEN' = mock_key,
    'Content-Type' = 'application/json',
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )

  # Set credentials for stub
  qualtrics_api_credentials(mock_key, mock_host)
  # Export Progress Stub
  stub_request("get", expected_progress_url) %>%
    wi_th(headers = expected_header)
  # Download File Stub
  stub_request("get", expected_download_url) %>%
    wi_th(headers = expected_header)

  # If stubs not hit, error will be thrown
  stubbedRequestsHit <-
    suppressWarnings(download_qualtrics_export(fetch_url, mock_export_id)) %>%
    length() == 0
  expect_true(stubbedRequestsHit)

  webmockr::stub_registry_clear()
  webmockr::disable()
})
