library(webmockr)

context("Get a list of all surveys that the user has access to on Qualtrics")

test_that("all_surveys() sends the proper request to Qualtrics", {
  webmockr::enable()
  # define mocks since we are only testing request
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` =
               function(resp) list(
                 content = list(
                   result = list(
                     elements = c(list(
                       id = "SV_0D54a3emdOh7bBH",
                       name = "Imported Survey",
                       ownerId = "UR_8CywXqaSNzzu1Bb",
                       lastModified = "2018-10-22T20:12:33Z",
                       creationDate = "2017-08-24T00:12:26Z",
                       isActive = TRUE
                     )),
                     nextPage = NULL
                   ),
                   meta = list(
                     httpStatus = "200 - OK",
                     requestId = "9272ddb5-3a34-41fe-805f-8b4e66a11e80"
                   )
                 ),
                 OK = TRUE
               ))

  # Define expectations
  expected <- list(
    verb     = "GET",
    base_url = "https://test.qualtrics.com",
    url      = "https://test.qualtrics.com/API/v3/surveys/",
    api_key  = "1234",
    headers  = list(
      'X-API-TOKEN' = "1234",
      'Content-Type' = 'application/json',
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )
  )

  qualtrics_api_credentials(expected$api_key, expected$base_url)

  stub_request(expected$verb, expected$url) %>%
    wi_th(headers = expected$headers)

  # Call will fail test if request does not match expectations
  expect_is(all_surveys(), "tbl_df")

  webmockr::stub_registry_clear()
  webmockr::disable()
})


test_that("all_surveys() returns the correct data format", {
  qualtrics_api_credentials(api_key = "1234", base_url = "yourdatacenterid.qualtrics.com")

  resp <- with_mock(
    `qualtRics::qualtrics_api_request` =
      function(verb, url, body=NULL) list(
        result = list(
          elements = c(list(
            id = "SV_0D54a3emdOh7bBH",
            name = "Imported Survey",
            ownerId = "UR_8CywXqaSNzzu1Bb",
            lastModified = "2018-10-22T20:12:33Z",
            creationDate = "2017-08-24T00:12:26Z",
            isActive = TRUE
          )),
          nextPage = NULL
        ),
        meta = list(
          httpStatus = "200 - OK",
          requestId = "9272ddb5-3a34-41fe-805f-8b4e66a11e80"
        )
      ),
      all_surveys()
  )

  expected_names <- c("id", "name", "ownerId", "lastModified",
                     "creationDate", "isActive")

  names_match <- all(names(resp) %in% expected_names) &
    all(expected_names %in% names(resp))
  rows_match <- nrow(resp) == 1

  expect_true(names_match)
  expect_true(rows_match)
})

test_that("all_surveys() throws an error", {
  # Store dummy key
  qualtrics_api_credentials(api_key = "1234", base_url = "yourdatacenterid.qualtrics.com")
  # This should fail in 'do.call'
  expect_error(
    all_surveys(),
    "you may not have the\nrequired authorization"
  )
})
