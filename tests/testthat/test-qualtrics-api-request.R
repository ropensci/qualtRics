library(webmockr)

test_that("it should throw an error after certain 400 and 500 status codes", {
  skip_on_cran()

  withr::with_envvar(
    new = c(
      "QUALTRICS_API_KEY" = "1234",
      "QUALTRICS_BASE_URL" = "www.qualtrics.com"
    ),
    {
      webmockr::enable()
      mock_url <- 'https://testUrl.com'
      verb <- 'GET'
      headers <- list(
        'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_KEY"),
        'Content-Type' = 'application/json',
        'Accept' = '*/*',
        'accept-encoding' = 'gzip, deflate'
      )
      stub_req <- stub_request(verb, mock_url) %>% wi_th(headers = headers)

      stub_req %>% to_return(status = 401)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 404)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 403)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 500)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 503)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 413)
      expect_error(qualtrics_api_request(verb, mock_url))

      stub_req %>% to_return(status = 429)
      expect_error(qualtrics_api_request(verb, mock_url))

      webmockr::disable()
    }
  )
})
