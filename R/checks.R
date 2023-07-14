

# General-purpose checking tools ------------------------------------------

## Credentials -------------------------------------------------------------

#' General check that credentials are present
#' @importFrom rlang abort
#' @keywords internal
check_credentials <- function(){
  creds <- c(
    base_url = Sys.getenv("QUALTRICS_BASE_URL"),
    api_key = Sys.getenv("QUALTRICS_API_KEY")
  )
  # Check that they exist:
  if(any(creds == "")){
    rlang::abort(c(
      "Qualtrics API key and/or base URL need registering:",
      i = "Use `qualtrics_api_credentials()`"
    ))
  }

  # Check URL again just to be sure:
  checkarg_base_url(Sys.getenv("QUALTRICS_BASE_URL"))

  return()

}

#' Checking and re-formatting base_url:
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#' @keywords internal
checkarg_base_url <- function(base_url){

  # Check string:
  checkarg_isstring(base_url)

  # Remove protocol with warning:
  if(stringr::str_detect(base_url, "^[a-zA-Z]*://")){
    protocol <- stringr::str_extract(base_url, "^[a-zA-Z]*://")
    base_url <- stringr::str_remove(base_url, "^[a-zA-Z]*://")

    rlang::inform(
      glue::glue("Protocol (e.g. '{protocol})' not needed in `base_url`, removing.")
    )

  }
  # Remove trailing '/' if present (silently), and check for qualtrics.com ending:
  if(endsWith(base_url, "qualtrics.com/")){
    base_url <- stringr::str_remove(base_url, "/$", "")
  } else if (!endsWith(base_url, ".qualtrics.com")){
    rlang::abort(
      c("Error in argument `base_url`",
        "`base_url` must be of the form '{datacenter ID}.qualtrics.com'",
        "See https://api.qualtrics.com/ZG9jOjg3NjYzMw-base-url-and-datacenter-i-ds"
      )
    )
  }
  # Return amended base_url:
  return(base_url)
}



## Argument types --------------------------------------------------------


# importing rlang's null default operator
`%||%` <- rlang::`%||%`

#' Is boolean (length-1 logical)
#' @param arg argument to be checked
#' @param null_okay Boolean. whether argument can be NULL
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isboolean <-
  function(arg, null_okay = FALSE){
    if(null_okay && is.null(arg)){return()}
    test <-
      is.logical(arg) && !is.na(arg) && length(arg) == 1

    if(!test){
      rlang::abort(
        c(glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a single `TRUE` or `FALSE`.")
      )
    }
  }

#' Is string (length-1 character)
#' @param arg argument to be checked
#' @param null_okay Boolean. whether argument can be NULL
#' @param na_okay Boolean. whether argument can be a length-1 NA
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isstring <-
  function(arg, null_okay = TRUE, na_okay = FALSE){
    if(null_okay && is.null(arg)){return()}

    # Check that length-1 string, non-NA unless na_okay == TRUE
    if(na_okay) {
      test <-
        is.character(arg) && length(arg) == 1
      msg <-
        "Argument must be a single string or NA"
    } else {
      test <-
        is.character(arg) && length(arg) == 1 && !is.na(arg)
      msg <-
        "Argument must be a single string."
    }

    if(!test){
      rlang::abort(
        c(glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          msg)
      )
    }

  }

#' Is character vector with no missing values:
#' @param arg argument to be checked
#' @param null_okay Boolean. whether argument can be NULL
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_ischaracter <-
  function(arg, null_okay = TRUE){
    if(null_okay && is.null(arg)){return()}

    test_char <-
      is.character(arg)

    if(!test_char){
      rlang::abort(
        c(glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a character vector.")
      )
    }

    test_missing <-
      all(!is.na(arg))

    if(!test_missing){
      rlang::abort(
        c(glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must not have missing values.")
      )
    }
  }


#' Is integerish (length-1 numeric w/o any fractional component)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isintegerish <-
  function(arg, null_okay = TRUE){
    if(null_okay && is.null(arg)){return()}

    test <-
      length(arg) == 1 &&
      (is.integer(arg) ||
         (is.numeric(arg) && arg == trunc(arg) && !is.na(arg))
      )

    if(!test){
      rlang::abort(
        c(glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a single integer or integer-like numeric")
      )
    }
  }


## Dates & times  --------------------------------------------------------------

#' Checks time zone, setting default if needed:
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_time_zone <-
  function(time_zone){

    # Make local (system) time zone if NULL
    time_zone <-
      time_zone %||% Sys.timezone()

    # Check that timezone is a string:
    checkarg_isstring(time_zone)

    # Check that it's a valid time-zone name:
    if(!time_zone %in% OlsonNames()){
      rlang::abort(
        c(glue::glue("Error in argument `time_zone`:"),
          "`time_zone` must be a valid R time zone designation",
          "See ?OlsonNames for list of valid names"
        )
      )
    }

    return(time_zone)
  }

#' Title Check date-time-like inputs and convert them to ISO8601 (with time
#' zone)
#'
#' @param date_arg Date, POSIX(c/l)t date/time, or parseable string in
#'   YYYY(/-)MM(/-)DD format, optionally with a space and HH:MM:SS in 24 hour
#'   time.  Intended to be converted to ISO8601 string for use in Qualtrics API
#'   call
#' @param time_zone Optional arg for time zone specification, defaults to system
#'   local timezone.
#' @param endofday Boolean. If TRUE, if the entry has no time information (only
#'   date), then result will have 23:59:59 (end of day) versus the implicit
#'   00:00:00 (start).  Intended for, e.g., 'end_date' argument in
#'   fetch_survey(). This allows the intuitive expectation where entering
#'   end_date = 2022/05/05 includes rather than excludes cases on 05/05 itself.
#' @importFrom lubridate is.POSIXt
#' @importFrom lubridate is.Date
#' @importFrom lubridate format_ISO8601
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom rlang abort
#' @keywords internal
#' @return single string date/time formatted in ISO8601
checkarg_datetime <-
  function(date_arg,
           time_zone = NULL,
           endofday = FALSE
  ){

    if(is.null(date_arg)){return()}

    # Check time_zone arg and fix defaults:
    time_zone <-
      checkarg_time_zone(time_zone)

    # Check that date_arg is correct type:
    test_date_arg_type <-
      length(date_arg) == 1 &&
      (lubridate::is.POSIXt(date_arg) |
         lubridate::is.Date(date_arg) |
         is.character(date_arg))
    if(!test_date_arg_type){
      rlang::abort(
        c(
          glue::glue("Error in {deparse(substitute(date_arg))}:"),
          "Argument must be a length-1 Date, POSIXlt, or POSIXct object, or string representation thereof."
        )
      )
    }
    # Check that date_arg is correct format:
    if(is.character(date_arg)){
      date_format <-
        glue::glue(
          "[0-9]{{4}}", # Years
          "(0[1-9]|1[0-2])", # Months
          "([0-2][0-9]|3[0-1])", # Days
          .sep = "[-/]" # Either / or - separating them
        )
      time_format <-
        glue::glue(
          "(0[0-9]|1[0-9]|2[0-3])", # Hours
          "[0-5][0-9]", # Minutes
          "[0-5][0-9]", # Seconds
          .sep = ":" # Separated by :
        )
      # Any string format with dates or dates & times:
      datetime_format <-
        glue::glue("^{date_format}( {time_format})?$")
      # Format with just dates:
      dateonly_format <-
        glue::glue("^{date_format}$")

      test_datetime_format <-
        stringr::str_detect(date_arg, datetime_format)
      if(!test_datetime_format){
        rlang::abort(
          c(
            glue::glue("Error in {deparse(substitute(date_arg))}:"),
            "String input must follow one of the following formats:",
            "'YYYY/MM/DD' or 'YYYY-MM-DD'",
            "'YYYY/MM/DD HH:MM:SS' or 'YYYY-MM-DD HH:MM:SS'",
            "Times use 24-hour notation",
            "If string ends w/time zone (e.g. EST), remove and use argument `time_zone`"
          )
        )
      }
    }

    # Check if we need to add end-of-day adjustment, and do so:
    if(endofday){
      # Check if date or a string w/just a date:
      test_dateonly <-
        lubridate::is.Date(date_arg) || (
          is.character(date_arg) &&
          stringr::str_detect(date_arg, dateonly_format)
        )
      # If so, append 23:59:59:
      if(test_dateonly){
        date_arg <-
          paste0(date_arg, " 23:59:59")
      }
    }

    # Attempt to parse the date/time object:
    date_parsed <-
      suppressWarnings(
        lubridate::as_datetime(
          x = date_arg,
          tz = time_zone
        )
      )

    # If it didn't parse for some other reason, throw an error:
    if(is.na(date_parsed)){
      rlang::abort(
        c(
          glue::glue("Error in {deparse(substitute(arg))}:"),
          "Failed to parse date/time, please check input"
        )
      )
    }

    # Format in ISO 8601 (displayed in UTC using Z notation) for API call:
    date_formatted <-
      lubridate::format_ISO8601(
        x = date_parsed, usetz = "Z",
        precision = "ymdhms"
      )

    # Return formatted date:
    return(date_formatted)
  }



# fetch_survey() & read_survey()------------------------------------------------


## include_* args  ----------------------


#' Check that include_metadata has the right elements & format for API call
#' @importFrom rlang abort
#' @importFrom dplyr setdiff
#' @keywords internal
checkarg_include_metadata <-
  function(include_metadata){
    # If NULL, ignore:
    if(is.null(include_metadata)){
      return()
    }
    # If NA, return character(), which will exclude via api
    if(length(include_metadata) == 1 && is.na(include_metadata)){
      return(character())
    }

    # Check type:
    checkarg_ischaracter(include_metadata)

    # references for valid metadata names::
    metadata_ref <-
      c(startdate = "startDate",
        enddate = "endDate",
        status = "status",
        ipaddress = "ipAddress",
        progress = "progress",
        `duration (in seconds)` = "duration",
        duration = "duration",
        finished = "finished",
        recordeddate = "recordedDate",
        responseid = "_recordId",
        `_recordid` = "_recordId",
        recipientlastname = "recipientLastName",
        recipientfirstname = "recipientFirstName",
        recipientemail = "recipientEmail",
        externalreference = "externalDataReference",
        externaldatareference = "externalDataReference",
        locationlatitude = "locationLatitude",
        locationlongitude = "locationLongitude",
        distributionchannel = "distributionChannel",
        userlanguage = "userLanguage")

    # Check that all names of the metadata are valid:
    test <-
      dplyr::setdiff(tolower(include_metadata), names(metadata_ref))

    if(length(test) > 0){

      rlang::abort(
        c("Error in argument 'include_metadata': invalid names used",
          "See Details in ?fetch_survey for more information.",
          cli::cli_text("Problem items: {test}")
        )
      )
    }

    # Format for the API call:
    include_metadata_formatted <-
      unique(metadata_ref[tolower(include_metadata)])

    return(include_metadata_formatted)

  }


#' Check that include_questions uses the QID format, and format for API call:
#' @importFrom rlang abort
#' @keywords internal
checkarg_include_questions <-
  function(include_questions){
    # If NULL, ignore:
    if(is.null(include_questions)){
      return()
    }
    # If NA, return character(), which will exclude via api
    if(length(include_questions) == 1 &&
       is.na(include_questions)){
      return(character())
    }
    # Check type:
    checkarg_ischaracter(include_questions)

    # Check format:
    test <-
      all(
        grepl(pattern = "^QID[0-9]+$",
              x = include_questions,
              ignore.case = TRUE)
      )

    if(!test){
      rlang::abort(
        c("Error in `include_questions`",
          "Argument requires using Qualtrics internal IDs, e.g. c('QID5', 'QID25')",
          "See Details in ?fetch_survey."
        )
      )
    }

    #Make uppercase and return:
    return(toupper(include_questions))
  }

#' Check include_embedded and format for API call:
#' @importFrom rlang abort
#' @keywords internal
checkarg_include_embedded <-
  function(include_embedded){
    # If NULL, ignore:
    if(is.null(include_embedded)){
      return()
    }
    # If NA, return character(), which will exclude via api
    if(length(include_embedded) == 1 && is.na(include_embedded)){
      return(character())
    }
    # Check type:
    checkarg_ischaracter(include_embedded)

    # Return unchanged:
    return(include_embedded)
  }

## col_types - "col_spec" object ---------------------------------------------------------

#' col_types must be a col_spec object from readr
#' @importFrom rlang abort
#' @keywords internal
checkarg_col_types <-
  function(col_types){
    if(is.null(col_types)){return()}
    if(class(col_types) != "col_spec"){
      rlang::abort(
        c("Error in argument `col_types`",
          "Must be a `col_spec` object from `readr::cols()`")
      )
    }
  }

## limit - Response limits -----------------------------------------------------

#' Check limit for being integer >= 1:
#' @importFrom rlang abort
#' @keywords internal
checkarg_limit <-
  function(limit){
    if(is.null(limit)){return()}

    checkarg_isintegerish(limit)

    if(limit < 1){
      rlang::abort(
        c("Error in argument `limit`:",
          "The value of `limit` must be 1 or greater.")
      )
    }
  }


# ## convert,  label,  and breakouts --------------------

#' Check conditions around combinations of convert, label, and breakout_sets
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @keywords internal
checkarg_convert_label_breakouts <-
  function(convert, label, breakout_sets){
    # Check type:
    checkarg_isboolean(convert)
    checkarg_isboolean(label)
    checkarg_isboolean(breakout_sets)

    if(convert && !label){
      rlang::abort(
        c("Error in arguments `convert` & `label`:",
          "`convert = TRUE` requires `label = TRUE` to facilitate factor conversion",
          "Set `label = TRUE`, or set `convert = FALSE`"
        )
      )
    }

    if(!label && !breakout_sets){
      rlang::warn(
        c("Use caution with `breakout_sets = FALSE` plus `label = FALSE`",
          "Results will likely be incorrectly guessed and read in as numeric",
          "Use a `col_types` specification to override")
      )
    }
  }


## directories and files ---------------------------------------------------

#' Check if downloaded file already exists
#' @param file_location (potential) path to previous download
#' @param surveyID Qualtrics survey ID
#' @param verbose whether to report if match is found
#' @importFrom rlang inform
#' @importFrom glue glue
#' @keywords internal
check_existing_download <-
  function(file_location,
           surveyID,
           verbose = TRUE) {
    if (file.exists(file_location)) {
      if (verbose) {
        rlang::inform(
          c(glue::glue("Loading saved prior download for surveyID = {surveyID}."),
            "Set `force_request = TRUE` to override this.")
        )
      }
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
    return(file_exists)
  }

#' Check if save directory exists
#' @importFrom rlang abort
#' @keywords internal
checkarg_save_dir <-
  function(save_dir) {
    if(is.null(save_dir)){return()}

    if(!dir.exists(save_dir)){
      rlang::abort(
        c("Error in `save_dir`:",
          "The directory given does not exist:",
          save_dir)
      )

    }
  }


#' Check if survey file specified in file_name exists
#' @importFrom rlang abort
#' @keywords internal
checkarg_file_name <-
  function(file_name) {
    if(!file.exists(file_name)){
      rlang::abort(
        c("Error in `file_name`:",
          glue::glue("The file given does not exist: {file_name}")
        )
      )

    }
  }



# fetch_description() & metadata()----------------------------------------------

#' Check if elements given in fetch_description are properly specified
#' @importFrom rlang abort
#' @importFrom dplyr setdiff
#' @keywords internal
checkarg_elements <-
  function(elements){
    # Allowed elements:
    allowed <-
      c("metadata",
        "surveyoptions",
        "flow",
        "blocks",
        "questions",
        "responsesets",
        "scoring")

    # If NULL or empty, return all the allowed elements:
    if(is.null(elements) || length(elements) == 0){
      return(allowed)
    }

    # Check that is character vector w/no missings:
    checkarg_ischaracter(elements)

    # Check that all names of the metadata are valid:
    test <-
      dplyr::setdiff(tolower(elements), allowed)

    if(length(test) > 0){

      rlang::abort(
        c("Error in argument `elements`:",
          "Invalid elements specified, see ?fetch_description for more information.",
          cli::cli_text("Problem items: {test}")
        )
      )
    }

    return(elements)

  }

#' Check if elements given in metadata's 'get' are properly specified
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @keywords internal
checkarg_get <- function(get){
  # Allowed elements in get :
  allowed <-
    c("metadata",
      "questions",
      "responsecounts",
      "blocks", "flow",
      "embedded_data",
      "comments")

  # Default elements
  default <-
    c("metadata",
      "questions",
      "responsecounts")

  if(is.list(get)){
    rlang::warn(
      c("Warning for argument `get`",
        "Use of logical lists has been deprecated",
        "In the future, use a character vector of desired elements")
    )

    # Pull out the TRUE elements of the list:
    get_true <-
      names(get)[unlist(get)]
    # Pull out the FALSE elements of the list:
    get_false <-
      names(get)[!unlist(get)]

    # Restore old behavior when using lists (metadata, questions, responsecounts
    # included unless specifically specified as FALSE):
    get <-
      dplyr::setdiff(
        dplyr::union(c("metadata", "questions", "responsecounts"),
                     get_true),
        get_false)
  }
  # If NULL or empty, return the default elements:
  if(is.null(get) || length(get) == 0){
    return(default)
  }

  # Check that is character vector w/no missings:
  checkarg_ischaracter(get)

  # Check that all names of the metadata are valid:
  test <-
    dplyr::setdiff(tolower(get), allowed)

  if(length(test) > 0){

    rlang::abort(
      rlang::abort(
        c("Error in argument `get`:",
          "Invalid elements specified, see ?metadata for more information.",
          cli::cli_text("Problem items: {test}")
        )
      )
    )
  }

  return(get)

}


# fetch_id() --------------------------------------------------------------

#' Check if data for fetch_id() is correct
#' @importFrom rlang abort
#' @keywords internal
checkarg_fetch_id_data <-
  function(.data){

    test <-
      is.data.frame(.data) &&
      all(c("id", "name") %in% names(.data))

    if(!test){
      rlang::abort(
        c("Error in `.data`:",
          "`fetch_id()` needs a dataframe from `all_surveys()` with columns `id` & `name`",
          'Example usage: `all_surveys() %>% fetch_id("That Survey I Need")`')
      )
    }
  }

# update_survey_metadata -------------------------------------------------------

#' Check if active from update_survey_metadata() is boolean, then convert to active/inactive
#' @keywords internal
checkarg_survey_active <-
  function(active){
    if(is.null(active)){return()}

    checkarg_isboolean(active)

    # Format
    active_formatted <-
      ifelse(active, "Active", "Inactive")

    return(active_formatted)
  }

#' Check and format activation_date
#' @importfrom glue glue
#' @importfrom rlang warn
#' @keywords internal
checkarg_activation_date <-
  function(activation_date,
           active,
           time_zone,
           surveyID
  ){

    # Return NULL if NULL:
    if(is.null(activation_date)){return(NULL)}
    # Return NA if length-1 NA:
    if(length(activation_date) == 1 && is.na(activation_date)){return(NA)}
    # Return NULL w/message if active == TRUE & activation_date given
    # (you're starting the survey now, so a future start time makes no sense)
    if(isTRUE(active)){
      rlang::warn(
        c(
          glue::glue("Warning for survey {surveyID}"),
          "`active = TRUE` implies immediate activation; ignoring `activation_date`"
        )
      )
      return(NULL)
    }

    activation_date_formatted <-
      checkarg_datetime(
        activation_date,
        time_zone = time_zone
      )

    return(activation_date_formatted)

  }

#' Check and format expiration_date
#' @importfrom glue glue
#' @importfrom rlang warn
#' @keywords internal
checkarg_expiration_date <-
  function(expiration_date,
           active,
           time_zone,
           surveyID
  ){

    # Return NULL if NULL:
    if(is.null(expiration_date)){return(NULL)}
    # Return NA if length-1 NA:
    if(length(expiration_date) == 1 && is.na(expiration_date)){return(NA)}
    # Return NULL w/message if active == TRUE & expiration_date given
    # (you're end the survey now, so a future end time makes no sense)
    if(isFALSE(active)){
      rlang::warn(
        c(
          glue::glue("Warning for survey {surveyID}"),
          "`active = FALSE` implies immediate deactivation; ignoring `expiration_date`"
        )
      )
      return(NULL)
    }

    expiration_date_formatted <-
      checkarg_datetime(
        expiration_date,
        time_zone = time_zone,
        endofday = TRUE
      )

    return(expiration_date_formatted)

  }
