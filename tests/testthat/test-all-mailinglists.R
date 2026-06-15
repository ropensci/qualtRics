skip_on_cran()

test_that("all_mailinglists returns a tbl_df with expected column names and types", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = "POOL_abcdef123456789"
  ))
  local_mocked_bindings(
    glue_api_v3 = function(base_url)
      "https://stoplight.io/mocks/qualtricsv2/publicapidocs/60928",
    paginate_api_request = function(fetch_url) {
      list(list(
        contactCount     = 5L,
        mailingListId    = "CG_abc123",
        name             = "Test List",
        lastModifiedDate = 1700000000000,
        creationDate     = 1700000000000,
        ownerId          = "GR_abc123"
      ))
    }
  )

  x <- all_mailinglists()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("contactCount", "mailingListId", "name", "lastModifiedDate", "creationDate", "ownerId"))
  expect_type(x$contactCount, "integer")
  expect_type(x$mailingListId, "character")
  expect_type(x$name, "character")
  expect_type(x$ownerId, "character")
})

test_that("all_mailinglists uses XM Directory URL", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = "POOL_abcdef123456789"
  ))
  captured_url <- NULL
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) {
      captured_url <<- fetch_url
      list()
    }
  )

  suppressMessages(try(all_mailinglists(), silent = TRUE))
  expect_match(captured_url, "/directories/POOL_abcdef123456789/mailinglists")
})
