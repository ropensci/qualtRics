skip_on_cran()

test_that("fetch_directory_id discovers the directory ID from the API when unset", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = ""
  ))
  local_mocked_bindings(
    qualtrics_api_request = function(...) {
      list(result = list(elements = list(
        list(directoryId = "POOL_discovered123")
      )))
    }
  )

  expect_identical(fetch_directory_id(), "POOL_discovered123")
  # Discovered ID is cached in the environment variable for the session:
  expect_identical(Sys.getenv("QUALTRICS_DIRECTORY_ID"), "POOL_discovered123")
})

test_that("fetch_directory_id uses the cached env var without calling the API", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = "POOL_cached123"
  ))
  local_mocked_bindings(
    qualtrics_api_request = function(...) {
      stop("qualtrics_api_request should not be called when the ID is cached")
    }
  )

  expect_identical(fetch_directory_id(), "POOL_cached123")
})

test_that("fetch_directory_id aborts when no directories are found", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = ""
  ))
  local_mocked_bindings(
    qualtrics_api_request = function(...) {
      list(result = list(elements = list()))
    }
  )

  expect_error(fetch_directory_id(), "No XM Directories found")
})

test_that("fetch_directory_id warns and uses the first when multiple directories exist", {
  withr::local_envvar(list(
    "QUALTRICS_API_KEY" = "abcdef",
    "QUALTRICS_BASE_URL" = "t.qualtrics.com",
    "QUALTRICS_DIRECTORY_ID" = ""
  ))
  local_mocked_bindings(
    qualtrics_api_request = function(...) {
      list(result = list(elements = list(
        list(directoryId = "POOL_first123"),
        list(directoryId = "POOL_second456")
      )))
    }
  )

  expect_warning(
    dir_id <- fetch_directory_id(),
    "QUALTRICS_DIRECTORY_ID"
  )
  expect_identical(dir_id, "POOL_first123")
})
