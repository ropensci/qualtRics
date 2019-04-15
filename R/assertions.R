# Check if QUALTRICS_API_KEY is stored
assert_api_key <- function() { # nolint start

  # Check if API key is set in environment
  assertthat::assert_that(Sys.getenv("QUALTRICS_API_KEY") != "",
    msg = "You need to register your Qualtrics API key and base URL using the\n'qualtrics_api_credentials()' function."
  )
}

# Check if QUALTRICS_BASE_URL is stored
assert_base_url <- function() {

  # Check if base URL is set in environment
  assertthat::assert_that(Sys.getenv("QUALTRICS_BASE_URL") != "",
    msg = "You need to register your Qualtrics API key and base URL using the\n'qualtrics_api_credentials()' function."
  )

  # Test if root URL ends with '.qualtrics.com'
  assertthat::assert_that(endsWith(Sys.getenv("QUALTRICS_BASE_URL"), ".qualtrics.com"),
    msg = paste0(
      "The Qualtrics base URL must end with '.qualtrics.com'. Your base URL looks like this: '",
      Sys.getenv("QUALTRICS_BASE_URL"),
      "'.\nPlease visit https://api.qualtrics.com/docs/root-url for instructions about the Qualtrics base URL."
    )
  )
}

# Check if save directory exists
assert_saveDir_exists <- function(save_dir) {
  assertthat::assert_that(ifelse((!file.info(save_dir)$isdir |
    is.na(file.info(save_dir)$isdir) == TRUE),
  FALSE, TRUE
  ),
  msg = paste0("The directory ", save_dir, " does not exist.")
  )
}

# Check if unanswer_recode is a string
assert_seenUnansweredRecode_string <- function(unanswer_recode) {
  assertthat::assert_that(assertthat::is.string(unanswer_recode))
}

# Check if last_response is a string
assert_lastResponseId_string <- function(last_response) {
  assertthat::assert_that(assertthat::is.string(last_response))
}

# Check if start_date is string
assert_startDate_string <- function(start_date) {
  assertthat::assert_that(assertthat::is.string(start_date))
}

# Check if end_date is string
assert_endDate_string <- function(end_date) {
  assertthat::assert_that(assertthat::is.string(end_date))
}

# Check if include_questions are string(s)
assert_includedQuestionIds_string <- function(include_questions) {
  assertthat::assert_that(mode(include_questions) == "character",
    msg = "'include_questions' must be a character vector."
  )
}

# Check if limit > 0
assert_limit_abovezero <- function(limit) {
  assertthat::assert_that(limit > 0,
    msg = "Limit parameter should be at least 1."
  )
}

# Check if survey file exists
assert_surveyFile_exists <- function(file_name) {
  assertthat::assert_that(file.exists(file_name),
    msg = paste0(
      "File ",
      file_name,
      " does not exist. Please check if you passed the right file path."
    )
  )
}

# Check if these arguments are logical
assert_options_logical <- function(verbose, convert,
                                   local_time, label) {
  assertthat::assert_that(assertthat::is.flag(verbose),
    msg = "'verbose' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(convert),
    msg = "'convert' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(local_time),
    msg = "'local_time' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(label),
    msg = "'label' must be TRUE or FALSE."
  )
} # nolint end
