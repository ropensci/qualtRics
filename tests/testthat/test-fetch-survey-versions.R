skip_on_cran()

test_that("fetch_survey_versions() returns a tbl_df with expected column names and types", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) mock_versions()
  )

  x <- fetch_survey_versions("SV_abcdef123456789")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(
    x,
    c("surveyID", "versionID", "versionNumber", "description", "userID",
      "creationDate", "published", "wasPublished", "publishEvents")
  )
  expect_equal(nrow(x), 3)
  expect_type(x$surveyID, "character")
  expect_type(x$versionID, "character")
  expect_type(x$versionNumber, "integer")
  expect_type(x$description, "character")
  expect_type(x$userID, "character")
  expect_s3_class(x$creationDate, c("POSIXct", "POSIXt"))
  expect_type(x$published, "logical")
  expect_type(x$wasPublished, "logical")
  expect_type(x$publishEvents, "list")
  expect_equal(x$versionNumber, c(3L, 2L, 1L))
  expect_equal(x$published, c(FALSE, TRUE, FALSE))
  expect_null(x$publishEvents[[1]])
  expect_length(x$publishEvents[[2]], 1)
})

test_that("fetch_survey_versions() handles a survey with no versions", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) list()
  )

  x <- fetch_survey_versions("SV_abcdef123456789")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 0)
  expect_equal(ncol(x), 9)
})

test_that("fetch_survey_versions() uses the survey-definitions versions URL", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  captured_url <- NULL
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) {
      captured_url <<- fetch_url
      list()
    }
  )

  fetch_survey_versions("SV_abcdef123456789")
  expect_equal(
    as.character(captured_url),
    "https://t.qualtrics.com/API/v3/survey-definitions/SV_abcdef123456789/versions"
  )
})

test_that("fetch_survey_versions() follows nextPage", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  versions <- mock_versions()
  local_mocked_bindings(
    qualtrics_api_request = function(verb, url, ...) {
      if (grepl("page2", url)) {
        list(result = list(elements = versions[3], nextPage = NULL))
      } else {
        list(result = list(elements = versions[1:2],
                           nextPage = paste0(url, "?page2")))
      }
    }
  )

  x <- fetch_survey_versions("SV_abcdef123456789")
  expect_equal(x$versionNumber, c(3L, 2L, 1L))
})

test_that("fetch_survey_versions() requires a single string surveyID", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  expect_error(fetch_survey_versions(c("SV_1", "SV_2")), "single string")
  expect_error(fetch_survey_versions(NULL), "single string")
})
