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
                            "'.\nPlease visit https://api.qualtrics.com/docs/ for instructions about the Qualtrics base URL."
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

# Check if unanswer_recode is a integer-like scalar:
# (Uses the unexported code for is.integerish() from the dev version of assertthat)
# TODO: appropriate to replace w/assertthat:::is.integerish(unanswer_recode)?
assert_seenUnansweredRecode_integer <- function(unanswer_recode) {
  assertthat::assert_that({
    length(unanswer_recode) == 1 &&
      (is.integer(unanswer_recode) ||
         (is.numeric(unanswer_recode) &&
            all(unanswer_recode == trunc(unanswer_recode)) && !is.na(unanswer_recode)
         )
      )
    },
    msg = "unanswer_recode must be an integer-like scalar (numeric w/nothing after decimal)")
}

# Check if unanswer_recode_multi is a integer-like scalar:
# (Uses the unexported code for is.integerish() from the dev version of assertthat)
assert_multiselectSeenUnansweredRecode_integer <- function(unanswer_recode_multi) {
  assertthat::assert_that({
    length(unanswer_recode_multi) == 1 &&
      (is.integer(unanswer_recode_multi) ||
         (is.numeric(unanswer_recode_multi) &&
            all(unanswer_recode_multi == trunc(unanswer_recode_multi)) && !is.na(unanswer_recode_multi)
         )
      )
    },
    msg = "unanswer_recode_multi must be an integer-like scalar (numeric w/nothing after decimal)")
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

# Check if newline_string is string
assert_newlineReplacement_string <- function(newline_string) {
  assertthat::assert_that(assertthat::is.string(newline_string))
}

# Check if time_zone is string
assert_timeZone_string <- function(time_zone) {
  assertthat::assert_that(assertthat::is.string(time_zone))
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
assert_options_logical <- function(verbose,
                                   convert,
                                   import_id,
                                   label,
                                   include_displayorder) {
  assertthat::assert_that(assertthat::is.flag(verbose),
                          msg = "'verbose' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(convert),
                          msg = "'convert' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(import_id),
                          msg = "'import_id' must be TRUE or FALSE."
  )

  assertthat::assert_that(assertthat::is.flag(label),
                          msg = "'label' must be TRUE or FALSE."
  )
  assertthat::assert_that(assertthat::is.flag(include_displayorder),
                          msg = "'include_displayorder' must be TRUE or FALSE."
  )
} # nolint end
