library(webmockr)

context("qualtrics-api-request")

test_that("it should make an http request with verb, url, and api-key", {
  webmockr::enable()

  mock_url <- 'testUrl.com'
  verb <- 'GET'
  mock_api_key <- '1234'
  qualtrics_api_credentials(mock_api_key, mock_url)

  headers <- list('X-API-TOKEN' = mock_api_key,
                  'Content-Type' = 'application/json',
                  'Accept' = '*/*',
                  'accept-encoding' = 'gzip, deflate')

  stub_request("GET", mock_url) %>%
    wi_th(headers = headers) %>% to_return(status=200)

  with_mock(
    `qualtRics::check_for_warnings` = function(resp) NULL,
    expect_is(qualtrics_api_request("GET", mock_url), "raw")
  )
  stub_registry_clear()


  webmockr::disable()
})

test_that("it should throw an error after certain 400 and 500 status codes", {
  webmockr::enable()
  mock_url <- 'testUrl.com'
  verb <- 'GET'
  mock_api_key <- '1234'
  qualtrics_api_credentials(mock_api_key, mock_url)
  headers <- list('X-API-TOKEN' = mock_api_key,
                  'Content-Type' = 'application/json',
                  'Accept' = '*/*',
                  'accept-encoding' = 'gzip, deflate')
  stub_req <- stub_request(verb, mock_url) %>% wi_th(headers = headers)



  stub_req %>% to_return(status=401)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=404)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=403)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=500)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=503)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=413)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  stub_req %>% to_return(status=429)
  expect_error(qualtrics_api_request(verb, mock_url))
  stub_registry_clear()

  webmockr::disable()
})

