skip_on_cran()
skip_on_ci()

test_that("fetch_mailinglist returns a tbl_df with expected column names and types", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = "POOL_abcdef123456789"
  ))
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) {
      list(list(
        contactId               = "CID_abc123",
        contactLookupId         = "CGC_abc123",
        email                   = "jane@example.com",
        extRef                  = "ext123",
        firstName               = "Jane",
        language                = "EN",
        lastName                = "Doe",
        mailingListUnsubscribed = "false",
        phone                   = NA,
        unsubscribed            = FALSE
      ))
    }
  )

  x <- fetch_mailinglist("ML_abcdef123456789")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_true(
    all(
      c(
        "contactId",
        "firstName",
        "lastName",
        "email",
        "extRef",
        "language",
        "unsubscribed"
      ) %in%
        names(x)
    )
  )
  expect_type(x$contactId, "character")
  expect_type(x$firstName, "character")
  expect_type(x$lastName, "character")
  expect_type(x$email, "character")
  expect_type(x$extRef, "character")
  expect_type(x$language, "character")
  expect_type(x$unsubscribed, "logical")
})

test_that("fetch_mailinglist uses XM Directory URL", {
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

  suppressMessages(try(fetch_mailinglist("ML_abcdef123456789"), silent = TRUE))
  expect_match(captured_url, "/directories/POOL_abcdef123456789/mailinglists/ML_abcdef123456789/contacts")
})
