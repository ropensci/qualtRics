# utils.R contains helper functions for the qualtRics package. These functions should not be called directly by the user and should not be exported.


#' Checks responses against Qualtrics response codes and returns error message.
#'
#' @param res Response from httr::GET
#' @param raw If TRUE, add 'raw' flag to httr::content() function.

qualtrics_response_codes <- function(res, raw = FALSE) {
  # Check status code and raise error/warning
  if (res$status_code == 200) {
    if (raw) {
      result <- httr::content(res, "raw")
    } else {
      result <- httr::content(res)
    }
    return(list(
      "content" = result,
      "OK" = TRUE
    ))
  } else if (res$status_code == 401) {
    rlang::abort("Qualtrics API raised an authentication (401) error - you may not have the\nrequired authorization. Please check your API key and root url.") # nolint
  } else if (res$status_code == 403) {
    rlang::abort("Qualtrics API raised an forbidden (403) error - you may have a valid API\nkey that lacks permissions to query the API. Please check your settings and/or talk to your administrators.") # nolint
  } else if (res$status_code == 400) {
    rlang::abort("Qualtrics API raised a bad request (400) error - Please report this on\nhttps://github.com/ropensci/qualtRics/issues") # nolint
  } else if (res$status_code == 404) {
    rlang::abort("Qualtrics API complains that the requested resource cannot be found (404 error).\nPlease check if you are using the correct survey ID.") # nolint
  } else if (res$status_code == 500) {
    rlang::abort(paste0(
      "Qualtrics API reports an internal server (500) error. Please contact\nQualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n", # nolint
      "\n",
      "instanceId:", " ",
      httr::content(res)$meta$error$instanceId,
      "\n",
      "errorCode: ",
      httr::content(res)$meta$error$errorCode
    ))
    return(list(
      "content" = httr::content(res),
      "OK" = FALSE
    ))
  } else if (res$status_code == 503) {
    rlang::abort(paste0(
      "Qualtrics API reports a temporary internal server (500) error. Please\ncontact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and\nerrorCode below or retry your query.", "\n", # nolint
      "\n",
      "instanceId:", " ", httr::content(res)$meta$error$instanceId,
      "\n",
      "errorCode: ", httr::content(res)$meta$error$errorCode
    ))
    return(list(
      "content" = httr::content(res),
      "OK" = FALSE
    ))
  } else if (res$status_code == 413) {
    rlang::abort("The request body was too large. This can also happen in cases where a\nmultipart/form-data request is malformed.") # nolint
  } else if (res$status_code == 429) {
    rlang::abort("You have reached the concurrent request limit.")
  } else {
    rlang::abort(paste0("Qualtrics API reports a ", res$status_code, " status code."))
  }
}

#' Construct a header to send to Qualtrics API
#'
#' @param API_TOKEN API token. Available in your Qualtrics account (see: \url{https://api.qualtrics.com/})

construct_header <- function(API_TOKEN) {
  # Construct and return
  headers <- c(
    "X-API-TOKEN" = API_TOKEN,
    "Content-Type" = "application/json",
    "Accept" = "*/*",
    "accept-encoding" = "gzip, deflate"
  )
  return(headers)
}

#' Check if httr GET result contains a warning
#'
#' @param resp object returned by \code{\link{qualtrics_response_codes}}


check_for_warnings <- function(resp) {
  # Raise warning if resp contains notice
  if (!is.null(resp$content$meta)) {
    if (!is.null(resp$content$meta$notice)) {
      warning(resp$content$meta$notice)
    }
  }
  NULL
}

#' Check if parameters passed to functions are correct
#'
#' @param ... options passed to function

check_params <- function(...) {
  args <- list(...)

  ## options
  if (all(c(
    "verbose",
    "convert",
    "import_id",
    "label",
    "include_display_order",
    "breakout_sets",
    "add_column_map",
    "add_var_labels"
  ) %in% names(args))) {
    assert_options_logical(
      args$verbose,
      args$convert,
      args$import_id,
      args$label,
      args$include_display_order,
      args$breakout_sets,
      args$add_column_map,
      args$add_var_labels
    )
  }

  if (args$convert) {
    assertthat::assert_that(
      args$label,
      msg = "To convert to factors, we need the Qualtrics labels.\nUse `label = TRUE` or `convert = FALSE`."
    )
  }

  if (!args$label & !args$breakout_sets) {
    rlang::warn(
      c("Use caution with `breakout_sets = FALSE` plus `label = FALSE`",
        "Results will likely be incorrectly guessed and read in as numeric",
        "Use a `col_types` specification to override")
    )
  }

  # Check if params are of the right type
  if ("start_date" %in% names(args)) {
    if (!is.null(args$start_date)) {
      assert_startDate_string(args$start_date)
    }
  }
  if ("end_date" %in% names(args)) {
    if (!is.null(args$end_date)) assert_endDate_string(args$end_date)
  }
  # Check if save_dir exists
  if ("save_dir" %in% names(args)) {
    if (!is.null(args$save_dir)) {
      assert_saveDir_exists(args$save_dir)
    }
  }
  # Check if seenUnansweredRecode is NULL or else integerlike
  if ("unanswer_recode" %in% names(args)) {
    if (!is.null(args$unanswer_recode)) {
      assert_seenUnansweredRecode_integer(args$unanswer_recode)
    }
  }
  # Check if multiselectSeenUnansweredRecode is NULL or else integerlike
  if ("unanswer_recode_multi" %in% names(args)) {
    if (!is.null(args$unanswer_recode_multi)) {
      assert_multiselectSeenUnansweredRecode_integer(args$unanswer_recode_multi)
    }
  }
  # Check if limit > 0
  if ("limit" %in% names(args)) {
    if (!is.null(args$limit)) {
      assert_limit_abovezero(args$limit)
    }
  }
  # Check if includedQuestionIds is a string
  if ("include_questions" %in% names(args)) {
    if (!is.null(args$include_questions)) {
      assert_includedQuestionIds_string(args$include_questions)
    }
  }
}

#' Generate URL for specific API query by type and (if appropriate) ID
#'
#' @param query string.  The specific API query desired.  Generally named the
#'   same as associated functions but without underscores, so the request for
#'   `fetch_survey()` would be be "fetchsurvey".
#' @param ... Named elements of URL for specific query desired, such as
#'   `surveyID` or `mailinglistID`
#'
#' @importFrom glue glue
#'
#' @return Endpoint URL to be passed to querying tools
#' @keywords internal
#' @export
generate_url <- function(query, ...){

  args <- list(...)
  list2env(args, envir = environment())

  # Get the user's specific base URL from environment:
  base_url <- Sys.getenv("QUALTRICS_BASE_URL")

  # make sure that the base_url ends w/o forward slash:
  base_url <-
    ifelse(
      substring(base_url, nchar(base_url)) == "/",
      substr(base_url, 1, nchar(base_url)-1),
      base_url
    )

  # Construct URL root for the v3 api endpoint:
  root_url <- glue::glue("https://{base_url}/API/v3")

  # List of templates for how to build URLs
  # (add to this when new functions made):
  endpoint_template <-
    switch(
      query,
      allsurveys = "{rooturl}/surveys/",
      allmailinglists = "{rooturl}/mailinglists/",
      metadata = "{rooturl}/surveys/{surveyID}/",
      fetchsurvey = "{rooturl}/surveys/{surveyID}/export-responses/",
      fetchdescription = "{rooturl}/survey-definitions/{surveyID}/",
      fetchmailinglist = "{rooturl}/mailinglists/{mailinglistID}/contacts/",
      fetchdistributions = "{rooturl}/distributions?surveyId={surveyID}",
      rlang::abort("Internal error: invalid URL generation query")
    )

  # Construct the actual URL:
  glue::glue(endpoint_template, rooturl = root_url, ...)

}

#' Create raw JSON payload to post response exports request
#'
#' @param label Flag
#' @param start_date Flag
#' @param end_date Flag
#' @param limit Flag
#' @param time_zone Flag
#' @param unanswer_recode Flag
#' @param unanswer_recode_multi Flag
#' @param include_display_order Flag
#' @param include_questions Flag
#' @param breakout_sets Flag
#'
#' @seealso See \code{\link{all_surveys}} for more details on these parameters
#'
#' @importFrom jsonlite toJSON
#' @importFrom purrr discard
#'
#' @return JSON file with options to send to API

create_raw_payload <- function(label = TRUE,
                               start_date = NULL,
                               end_date = NULL,
                               limit = NULL,
                               time_zone = NULL,
                               unanswer_recode = NULL,
                               unanswer_recode_multi = NULL,
                               include_display_order = TRUE,
                               include_questions = NULL,
                               breakout_sets = NULL) {

  params <- as.list(environment())

  names_crosswalk <-
    c(label = "useLabels",
      start_date = "startDate",
      end_date = "endDate",
      limit = "limit",
      time_zone = "timeZone",
      unanswer_recode = "seenUnansweredRecode",
      unanswer_recode_multi = "multiselectSeenUnansweredRecode",
      include_display_order = "includeDisplayOrder",
      include_questions = "questionIds",
      breakout_sets = "breakoutSets")



  if(!is.null(params$start_date)){
    params$start_date <- paste0(start_date, "T00:00:00Z")
  }
  if(!is.null(params$end_date)){
    params$end_date <- paste0(end_date, "T00:00:00Z")
  }

  # Adjust names to fit API names:

  names(params) <- names_crosswalk[names(params)]

  # Add in format param:
  params$format <- "csv"

  # Unbox
  params_ub <- map(params, function(x){
    if(length(x) == 1) jsonlite::unbox(x) else x
  }
  )

  # But "questionIds" needs to be boxed
  params_ub["questionIds"] <- params["questionIds"]

  # Drop any NULL elements:
  params_ub <- purrr::discard(params_ub, ~ is.null(.x))

  # convert to JSON:
  jsonlite::toJSON(params_ub, auto_unbox = FALSE)

}


#' Send httr requests to Qualtrics API
#'
#' @param verb Type of request to be sent (@seealso [httr::VERB()])
#' @param url Qualtrics endpoint URL created by [generate_url()] functions
#' @param body Options created by [create_raw_payload()] function
#' @keywords internal

qualtrics_api_request <- function(verb = c("GET", "POST"),
                                  url = url,
                                  body = NULL) {
  # Match arg
  verb <- match.arg(verb)
  # Construct header
  headers <- construct_header(Sys.getenv("QUALTRICS_API_KEY"))
  # Send request to Qualtrics API
  res <- httr::VERB(verb,
                    url = url,
                    httr::add_headers(headers),
                    body = body
  )
  # Check if response type is OK
  cnt <- qualtrics_response_codes(res)
  # Check if OK
  if (cnt$OK) {
    # If notice occurs, raise warning
    w <- check_for_warnings(cnt)
    # return content
    return(cnt$content)
  }
}

#' Download response export
#'
#' @param fetch_url URL provided by Qualtrics API that shows the download percentage completeness
#' @param requestID ID
#' @param verbose See \code{\link{fetch_survey}}


download_qualtrics_export <- function(fetch_url, requestID, verbose = FALSE) {
  # Construct header
  headers <- construct_header(Sys.getenv("QUALTRICS_API_KEY"))

  # This is the url to use when checking the ID
  check_url <- paste0(fetch_url, requestID)

  # Create a progress bar and monitor when export is ready
  if (verbose) {
    pbar <- utils::txtProgressBar(
      min = 0,
      max = 100,
      style = 3
    )
  }
  # While download is in progress
  progress <- 0
  while (progress < 100) {
    # Get percentage complete
    CU <- qualtrics_api_request("GET", url = check_url)
    progress <- CU$result$percentComplete
    # Set progress
    if (verbose) {
      utils::setTxtProgressBar(pbar, progress)
    }
  }
  # Kill progress bar
  if (verbose) {
    close(pbar)
  }

  # Construct a url for obtaining the file:
  file_url <- paste0(fetch_url, CU$result$fileId, "/file")


  # Download file
  f <- tryCatch({
    httr::GET(file_url, httr::add_headers(headers))
  }, error = function(e) {
    # Retry if first attempt fails
    httr::GET(file_url, httr::add_headers(headers))
  })

  # Load raw zip file
  ty <- qualtrics_response_codes(f, raw = TRUE)
  # To zip file
  tf <- paste0(
    tempdir(),
    ifelse(substr(
      tempdir(),
      nchar(tempdir()),
      nchar(tempdir())
    ) == "/",
    "temp.zip",
    "/temp.zip"
    )
  )
  # Write to temporary file
  writeBin(ty$content, tf)
  # Try to unzip
  u <- tryCatch({
    utils::unzip(tf, exdir = tempdir())
  }, error = function(e) {
    rlang::abort(paste0(
      "Error extracting CSV",
      " from zip file. Please re-run your query."
    ))
  })
  # Remove zipfile
  p <- file.remove(tf)
  # Return file location
  return(u)
}

#' Set proper data types on survey data.
#'
#' @param data Imported Qualtrics survey
#' @param surveyID ID of survey
#' @param verbose Flag
#'
#' @importFrom purrr map
#' @importFrom purrr map_chr

infer_data_types <- function(data,
                             surveyID,
                             verbose = FALSE) {

  # Download survey metadata
  md <- md <- tibble::enframe(metadata(surveyID, get = "questions")[[1]])

  # Check which questions are of allowed types
  md_parsed <- dplyr::mutate(md,
                             question_type = map(value, "questionType"),
                             question_name = map_chr(value, "questionName"),
                             type_supp = map_chr(question_type, "type"),
                             selector_supp = map_chr(question_type, "selector"),
                             type_supp = type_supp %in% c("MC"),
                             selector_supp = selector_supp %in% c("SAVR"),
                             name_in_survey = question_name %in% names(data),
                             supported = type_supp & selector_supp & name_in_survey)

  mc <- dplyr::pull(dplyr::filter(md_parsed, supported), name)

  # Conversion process (next) removes labels, so get them first to keep
  lab <- sjlabelled::get_label(data)

  # For each question we have appropriate metadata for, convert type
  for (m in mc) {
    question_meta <- dplyr::pull(dplyr::filter(md, name == m), value)[[1]]
    data <- wrapper_mc(data, question_meta)
  }

  # Put labels back on
  data <- sjlabelled::set_label(data, lab)

  # Check if warning given
  if (Sys.getenv("QUALTRICS_WARNING_DATE_GIVEN") == "") {
    rlang::inform(
      c("'StartDate', 'EndDate', and 'RecordedDate' were converted without a specific timezone",
        "To set a timezone, visit https://www.qualtrics.com/support/survey-platform/managing-your-account/",
        "Timezone information is under 'User Settings'",
        "See https://api.qualtrics.com/instructions/docs/Instructions/dates-and-times.md for more")
    )
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
  }
  # Return data
  return(data)
}

#' Convert multiple choice questions to ordered factors
#'
#' @param data Imported Qualtrics survey
#' @param question_meta Question metadata
#'
#' @importFrom rlang ':='

wrapper_mc <- function(data, question_meta) {
  # TODO: add ORDER = TRUE/FALSE if user wants factors to be ordered
  # TODO: add REVERSE = TRUE/FALSE if user wants the factor levels to be reversed

  # Get question details from metadata
  col_name <- rlang::sym(question_meta$questionName)
  meta <- tibble::enframe(question_meta$choices)

  # Level names
  ln <- dplyr::pull(dplyr::mutate(meta,
                                  meta_levels = purrr::map_chr(value,
                                                               "choiceText")),
                    meta_levels)
  ln <- remove_html(ln)

  # Convert
  dplyr::mutate(
    data,
    !!col_name := as.character(!!col_name),
    !!col_name := readr::parse_factor(!!col_name,
                                      levels = ln,
                                      ordered = TRUE
    )
  )
}

## simple HTML stripping
remove_html <- function(string) {
  stringr::str_remove_all(string, '<[^>]+>')
}
