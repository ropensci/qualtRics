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
    stop("Qualtrics API raised an authentication (401) error - you may not have the\nrequired authorization. Please check your API key and root url.") # nolint
  } else if (res$status_code == 403) {
    stop("Qualtrics API raised an forbidden (403) error - you may have a valid API\nkey that lacks permissions to query the API. Please check your settings and/or talk to your administrators.") # nolint
  } else if (res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on\nhttps://github.com/ropensci/qualtRics/issues") # nolint
  } else if (res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error).\nPlease check if you are using the correct survey ID.") # nolint
  } else if (res$status_code == 500) {
    stop(paste0(
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
    stop(paste0(
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
    stop("The request body was too large. This can also happen in cases where a\nmultipart/form-data request is malformed.") # nolint
  } else if (res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  } else {
    stop(paste0("Qualtrics API reports a ", res$status_code, " status code."))
  }
}

#' Construct a header to send to Qualtrics API
#'
#' @param API_TOKEN API token. Available in your Qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#'
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API.

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
    "local_time",
    "label"
  ) %in% names(args))) {
    assert_options_logical(
      args$verbose,
      args$convert,
      args$import_id,
      args$local_time,
      args$label
    )
  }

  if (args$convert) {
    assertthat::assert_that(args$label,
                            msg = "To convert to factors, we need the Qualtrics labels.\nUse `label = TRUE` or `convert = FALSE`.")
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
  if ("last_response" %in% names(args)) {
    if (!is.null(args$last_response)) {
      assert_lastResponseId_string(args$last_response)
    }
  }
  # Check if save_dir exists
  if ("save_dir" %in% names(args)) {
    if (!is.null(args$save_dir)) {
      assert_saveDir_exists(args$save_dir)
    }
  }
  # Check if seenUnansweredRecode is NULL or else a string
  if ("unanswer_recode" %in% names(args)) {
    if (!is.null(args$unanswer_recode)) {
      assert_seenUnansweredRecode_string(args$unanswer_recode)
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

#' Append proper end points to create root URL
#'
#' @param base_url Base URL for your institution (see
#' \url{https://api.qualtrics.com/docs/root-url}
#' @param type Either `"responseexports"` or `"surveys"`
#'
#' @return Root URL, plus appended end point

append_root_url <- function(base_url, type = c("responseexports", "surveys")) {
  # match
  type <- match.arg(type)
  # create root url
  root_url <- paste0(
    base_url,
    ifelse(substr(
      base_url, nchar(base_url),
      nchar(base_url)
    ) == "/",
    paste0("API/v3/", type, "/"),
    paste0("/API/v3/", type, "/")
    )
  )
  return(root_url)
}

#' Create raw JSON payload to post response exports request
#'
#' @param surveyID Survey ID
#' @param label Flag
#' @param last_response Flag
#' @param start_date Flag
#' @param end_date Flag
#' @param limit Flag
#' @param local_time Flag
#' @param unanswer_recode Flag
#' @param include_questions Flag
#'
#' @seealso Functions \code{\link{all_surveys}} and \code{\link{registerOptions}}
#' have more details on these parameters
#'
#' @return JSON file with options to send to API

create_raw_payload <- function(surveyID,
                               label = TRUE,
                               last_response = NULL,
                               start_date = NULL,
                               end_date = NULL,
                               limit = NULL,
                               local_time = FALSE,
                               unanswer_recode = NULL,
                               include_questions = NULL) {
  paste0(
    '{"format": ', '"', "csv", '"',
    ', "surveyId": ', '"', surveyID, '"',
    ifelse(
      is.null(last_response),
      "",
      paste0(
        ', "lastResponseId": ',
        '"',
        last_response,
        '"'
      )
    ),
    ifelse(
      is.null(start_date),
      "",
      paste0(
        ', "startDate": ',
        '"',
        paste0(start_date, "T00:00:00Z"),
        '"'
      )
    ),
    ifelse(
      is.null(end_date),
      "",
      paste0(
        ', "endDate": ',
        '"',
        paste0(end_date, "T00:00:00Z"),
        '"'
      )
    ),
    ifelse(
      is.null(unanswer_recode),
      "",
      paste0(
        ', "seenUnansweredRecode": ',
        '"',
        unanswer_recode,
        '"'
      )
    ),
    ifelse(
      !local_time,
      "",
      paste0(
        ', "useLocalTime": ',
        tolower(local_time)
      )
    ),
    ifelse(
      is.null(include_questions),
      "",
      paste0(
        ', "includedQuestionIds": ',
        "[", paste0('"', include_questions, '"', collapse = ", "), "]"
      )
    ),
    ifelse(
      is.null(limit),
      "",
      paste0(
        ', "limit": ',
        limit
      )
    ),
    ", ",
    '"useLabels": ', tolower(label),
    "}"
  )
}

#' Send httr requests to Qualtrics API
#'
#' @param verb Type of request to be sent (@seealso \code{\link[httr]{VERB}})
#' @param url Qualtrics endpoint URL created by \code{\link{append_root_url}} function
#' @param body Options created by \code{\link{create_raw_payload}} function

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
#' @param check_url URL provided by Qualtrics API that shows the download percentage completeness
#' @param verbose See \code{\link{fetch_survey}}


download_qualtrics_export <- function(check_url, verbose = FALSE) {
  # Construct header
  headers <- construct_header(Sys.getenv("QUALTRICS_API_KEY"))
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
  # Download file
  f <- tryCatch({
    httr::GET(paste0(check_url, "/file"), httr::add_headers(headers))
  }, error = function(e) {
    # Retry if first attempt fails
    httr::GET(paste0(check_url, "/file"), httr::add_headers(headers))
  })
  # If content is test request, then load temp file (this is purely for testing)
  # httptest library didn't work the way it needed and somehow still called the API
  # leading to errors
  if (f$request$url == "t.qualtrics.com/API/v3/responseexports/T_123/file") {
    if (f$request$headers["X-API-TOKEN"] == "1234") {
      ct <- readRDS("files/file_fetch_survey.rds")
      f$content <- ct
    }
  }
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
    stop(paste0(
      "Error extracting ",
      "csv",
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
  md <- tibble::enframe(metadata(surveyID,
                                 get = list(
                                   "questions" = TRUE,
                                   "metadata" = FALSE,
                                   "responsecounts" = FALSE
                                 ))[[1]])

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
    warning("The 'StartDate', 'EndDate' and 'RecordedDate' variables were converted without passing\na specific timezone. If you like to set these timestamps to your own timezone, please\nvisit https://www.qualtrics.com/support/survey-platform/getting-started/managing-your-account/\n(under 'User Settings'). See https://api.qualtrics.com/docs/dates-and-times for more\ninformation about how the Qualtrics API handles dates and times.")
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
