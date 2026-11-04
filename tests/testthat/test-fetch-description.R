skip_on_cran()

test_that("fetch_description() retrieves an appropriate survey description", {
  vcr::use_cassette("fetch_description", {
    x <- fetch_description("SV_3gbwq8aJgqPwQDP")
  })

  expect_type(x, "list")
  expect_named(
    x,
    c(
      "metadata",
      "surveyoptions",
      "flow",
      "blocks",
      "questions",
      "responsesets",
      "scoring"
    )
  )
  expect_s3_class(x$metadata, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$surveyoptions, c("tbl_df", "tbl", "data.frame"))
  expect_type(x$flow, "list")
  expect_type(x$blocks, "list")
  expect_type(x$questions, "list")
  expect_type(x$responsesets, "list")
  expect_type(x$scoring, "list")
})

test_that("fetch_description() with elements set works", {
  vcr::use_cassette("fetch_description", {
    x <- fetch_description(
      "SV_3gbwq8aJgqPwQDP",
      elements = c("surveyoptions", "flow", "blocks")
    )
  })

  expect_type(x, "list")
  expect_named(x, c("surveyoptions", "flow", "blocks"))
})

test_that("fetch_description() with legacy = TRUE reverts to metadata()", {
  vcr::use_cassette("fetch_description_legacy", {
    x <- fetch_description("SV_3gbwq8aJgqPwQDP", legacy = TRUE)
  })

  expect_type(x, "list")
  expect_named(x, c("metadata", "questions", "responsecounts"))
  expect_s3_class(x$metadata, "data.frame")
  expect_type(x$questions, "list")
  expect_s3_class(x$responsecounts, "data.frame")
})

test_that("setting elements works with legacy = TRUE ", {
  vcr::use_cassette("fetch_description_legacy", {
    x <- fetch_description(
      "SV_3gbwq8aJgqPwQDP",
      legacy = TRUE,
      elements = c(
        "metadata",
        "questions",
        "responsecounts",
        "blocks",
        "flow",
        "embedded_data",
        "comments"
      )
    )
  })

  expect_type(x, "list")
  expect_named(
    x,
    c(
      "metadata",
      "questions",
      "responsecounts",
      "blocks",
      "flow",
      "embedded_data",
      "comments"
    )
  )
  expect_s3_class(x$metadata, "data.frame")
  expect_type(x$questions, "list")
  expect_s3_class(x$responsecounts, "data.frame")
  expect_type(x$blocks, "list")
  expect_type(x$flow, "list")
  expect_type(x$embedded_data, "list")
  expect_type(x$comments, "list")
})

test_that("passing a list to elements when legacy = TRUE succeeds with warning ", {
  element_list <-
    list(
      "metadata" = TRUE,
      "questions" = FALSE,
      "responsecounts" = TRUE,
      "blocks" = FALSE,
      "flow" = TRUE,
      "embedded_data" = FALSE,
      "comments" = TRUE
    )

  expect_warning(
    vcr::use_cassette("fetch_description_legacy", {
      x <- fetch_description(
        "SV_3gbwq8aJgqPwQDP",
        legacy = TRUE,
        elements = element_list
      )
    }),
    "Use of logical lists"
  )

  expect_type(x, "list")
  expect_named(x, c("metadata", "responsecounts", "flow", "comments"))
})

# version argument ----------------------------------------------------------

test_that("fetch_description() with version = NULL uses the current definition URL", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  captured_url <- NULL
  local_mocked_bindings(
    qualtrics_api_request = function(verb, url, ...) {
      captured_url <<- url
      mock_definition()
    }
  )

  x <- fetch_description("SV_abcdef123456789")
  expect_equal(
    as.character(captured_url),
    "https://t.qualtrics.com/API/v3/survey-definitions/SV_abcdef123456789/"
  )
  expect_named(
    x,
    c("metadata", "surveyoptions", "flow", "blocks", "questions",
      "responsesets", "scoring")
  )
})

test_that("fetch_description() with a version ID fetches that version", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  captured_url <- NULL
  local_mocked_bindings(
    qualtrics_api_request = function(verb, url, ...) {
      captured_url <<- url
      mock_definition("Version one")
    }
  )

  x <- fetch_description("SV_abcdef123456789", version = "9000000000000001")
  expect_equal(
    as.character(captured_url),
    "https://t.qualtrics.com/API/v3/survey-definitions/SV_abcdef123456789/versions/9000000000000001"
  )
  expect_named(
    x,
    c("metadata", "surveyoptions", "flow", "blocks", "questions",
      "responsesets", "scoring")
  )
  expect_s3_class(x$metadata, c("tbl_df", "tbl", "data.frame"))
  expect_equal(x$metadata$value[x$metadata$name == "SurveyName"], "Version one")
})

test_that("fetch_description() with version = 'published' fetches the published version", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  captured_url <- NULL
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) mock_versions(),
    qualtrics_api_request = function(verb, url, ...) {
      captured_url <<- url
      mock_definition()
    }
  )

  x <- fetch_description(
    "SV_abcdef123456789",
    elements = c("metadata", "questions"),
    version = "published"
  )
  expect_equal(
    as.character(captured_url),
    "https://t.qualtrics.com/API/v3/survey-definitions/SV_abcdef123456789/versions/9000000000000002"
  )
  expect_named(x, c("metadata", "questions"))
})

test_that("fetch_description() with version = 'published' aborts when nothing is published", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  versions <- mock_versions()
  versions[[2]]$metadata$published <- FALSE
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) versions,
    qualtrics_api_request = function(...) {
      stop("the survey definition should not be requested")
    }
  )

  expect_error(
    fetch_description("SV_abcdef123456789", version = "published"),
    "has no published version"
  )
})

test_that("fetch_description() with version = 'published' aborts on more than one published version", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com"
  ))
  versions <- mock_versions()
  versions[[1]]$metadata$published <- TRUE
  local_mocked_bindings(
    paginate_api_request = function(fetch_url) versions
  )

  expect_error(
    fetch_description("SV_abcdef123456789", version = "published"),
    "more than one published version"
  )
})

test_that("fetch_description() rejects version combined with legacy = TRUE", {
  local_mocked_bindings(
    metadata = function(...) stop("metadata() should not be called")
  )
  expect_error(
    fetch_description("SV_abcdef123456789", legacy = TRUE, version = "published"),
    "not supported with `legacy = TRUE`"
  )
  expect_error(
    fetch_description("SV_abcdef123456789", legacy = TRUE, version = "9000000000000001"),
    "not supported with `legacy = TRUE`"
  )
})

test_that("fetch_description() checks the version argument", {
  expect_error(
    fetch_description("SV_abcdef123456789", version = c("1", "2")),
    "single string"
  )
  expect_error(
    fetch_description("SV_abcdef123456789", version = 1),
    "single string"
  )
  expect_error(
    fetch_description("SV_abcdef123456789", version = ""),
    "must be `NULL`, a version ID"
  )
})
