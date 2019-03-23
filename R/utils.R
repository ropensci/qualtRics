# utils.R contains helper functions for the qualtRics package. These functions should not be called directly by the user and should not be exported.


#' Checks responses against Qualtrics response codes and returns error message.
#'
#' @param res Response from httr::GET
#' @param raw If TRUE, add 'raw' flag to httr::content() function.

qualtrics_response_codes <- function(res, raw = FALSE) {
  # Check status code and raise error/warning
  if(res$status_code == 200) {
    if(raw) {
      result <- httr::content(res, "raw")
    } else {
      result <- httr::content(res)
    }
    return(list(
      "content" = result,
      "OK" = TRUE
    )
    )
  } else if(res$status_code == 401) {
    stop("Qualtrics API raised an authentication (401) error - you may not have the\nrequired authorization. Please check your API key and root url.") # nolint
  } else if(res$status_code == 403) {
    stop("Qualtrics API raised an forbidden (403) error - you may have a valid API\nkey that lacks permissions to query the API. Please check your settings and/or talk to your administrators.") # nolint
  } else if(res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on\nhttps://github.com/ropensci/qualtRics/issues") # nolint
  } else if(res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error).\nPlease check if you are using the correct survey ID.") # nolint
  } else if(res$status_code == 500) {
    stop(paste0("Qualtrics API reports an internal server (500) error. Please contact\nQualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n", # nolint
                "\n",
                "instanceId:", " ",
                httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ",
                httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    ))
  } else if(res$status_code == 503) {
    stop(paste0("Qualtrics API reports a temporary internal server (500) error. Please\ncontact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and\nerrorCode below or retry your query.", "\n", # nolint
                "\n",
                "instanceId:", " ", httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ", httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a\nmultipart/form-data request is malformed.") # nolint
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
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
    'X-API-TOKEN' = API_TOKEN,
    'Content-Type' = "application/json",
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )
  return(headers)
}

#' Check if httr GET result contains a warning
#'
#' @param resp object returned by \code{\link{qualtrics_response_codes}}


check_for_warnings <- function(resp) {
  # Raise warning if resp contains notice
  if(!is.null(resp$content$meta)) {
    if(!is.null(resp$content$meta$notice)) {
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
  if(all(c("verbose", "convertVariables",
           "useLocalTime", "useLabels") %in% names(args))) {
    assert_options_logical(
      args$verbose,
      args$convertVariables,
      args$useLocalTime,
      args$useLabels
    )
  }

  # Check if params are of the right type
  if("startDate" %in% names(args)) {
    if(!is.null(args$startDate)) {
      assert_startDate_string(args$startDate)
    }
  }
  if("endDate" %in% names(args)) {
    if(!is.null(args$endDate)) assert_endDate_string(args$endDate)
  }
  if("lastResponseId" %in% names(args)) {
    if(!is.null(args$lastResponseId)) {
      assert_lastResponseId_string(args$lastResponseId)
    }
  }
  # Check if save_dir exists
  if("save_dir" %in% names(args)) {
    if(!is.null(args$save_dir)) {
      assert_saveDir_exists(args$save_dir)
    }
  }
  # Check if seenUnansweredRecode is NULL or else a string
  if("seenUnansweredRecode" %in% names(args)) {
    if(!is.null(args$seenUnansweredRecode)) {
      assert_seenUnansweredRecode_string(args$seenUnansweredRecode)
    }
  }
  # Check if limit > 0
  if("limit" %in% names(args)) {
    if(!is.null(args$limit)) {
      assert_limit_abovezero(args$limit)
    }
  }
  # Check if includedQuestionIds is a string
  if("includedQuestionIds" %in% names(args)) {
    if(!is.null(args$includedQuestionIds)) {
      assert_includedQuestionIds_string(args$includedQuestionIds)
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
  root_url <- paste0(base_url,
                     ifelse(substr(base_url, nchar(base_url),
                                   nchar(base_url)) == "/",
                            paste0("API/v3/", type,"/"),
                            paste0("/API/v3/", type,"/")))
  return(root_url)
}

#' Create raw JSON payload to post response exports request
#'
#' @param surveyID Survey ID
#' @param useLabels Flag
#' @param lastResponseId Flag
#' @param startDate Flag
#' @param endDate Flag
#' @param limit Flag
#' @param useLocalTime Flag
#' @param seenUnansweredRecode Flag
#' @param includedQuestionIds Flag
#'
#' @seealso Functions \code{\link{getSurveys}} and \code{\link{registerOptions}}
#' have more details on these parameters
#'
#' @return JSON file with options to send to API

create_raw_payload <- function(surveyID,
                               useLabels = TRUE,
                               lastResponseId = NULL,
                               startDate = NULL,
                               endDate = NULL,
                               limit = NULL,
                               useLocalTime = FALSE,
                               seenUnansweredRecode = NULL,
                               includedQuestionIds = NULL) {

  paste0(
    '{"format": ', '"', 'csv', '"' ,
    ', "surveyId": ', '"', surveyID, '"',
    ifelse(
      is.null(lastResponseId),
      "",
      paste0(
        ', "lastResponseId": ',
        '"',
        lastResponseId,
        '"')
    ) ,
    ifelse(
      is.null(startDate),
      "",
      paste0(
        ', "startDate": ',
        '"',
        paste0(startDate,"T00:00:00Z"),
        '"')
    ) ,
    ifelse(
      is.null(endDate),
      "",
      paste0(
        ', "endDate": ',
        '"',
        paste0(endDate,"T00:00:00Z"),
        '"')
    ) ,
    ifelse(
      is.null(seenUnansweredRecode),
      "",
      paste0(
        ', "seenUnansweredRecode": ',
        '"',
        seenUnansweredRecode,
        '"')
    ),
    ifelse(
      !useLocalTime,
      "",
      paste0(
        ', "useLocalTime": ',
        tolower(useLocalTime)
      )
    ),
    ifelse(
      is.null(includedQuestionIds),
      "",
      paste0(
        ', "includedQuestionIds": ',
        '[', paste0('"',includedQuestionIds, '"', collapse=", "), ']'
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
    ', ',
    '"useLabels": ', tolower(useLabels),
    '}'
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
                    body = body)
  # Check if response type is OK
  cnt <- qualtrics_response_codes(res)
  # Check if OK
  if(cnt$OK) {
    # If notice occurs, raise warning
    w <- check_for_warnings(cnt)
    # return content
    return(cnt$content)
  }
}

#' Download response export
#'
#' @param check_url URL provided by Qualtrics API that shows the download percentage completneness
#' @param verbose See \code{\link{getSurvey}}


download_qualtrics_export <- function(check_url, verbose = FALSE) {
  # Construct header
  headers <- construct_header(Sys.getenv("QUALTRICS_API_KEY"))
  # Create a progress bar and monitor when export is ready
  if(verbose) {
    pbar <- utils::txtProgressBar(min=0,
                                  max=100,
                                  style = 3)
  }
  # While download is in progress
  progress <- 0
  while(progress < 100) {
    # Get percentage complete
    CU <- qualtrics_api_request("GET", url = check_url)
    progress <- CU$result$percentComplete
    # Set progress
    if(verbose) {
      utils::setTxtProgressBar(pbar, progress)
    }
  }
  # Kill progress bar
  if(verbose) {
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
  if(f$request$url == "t.qualtrics.com/API/v3/responseexports/T_123/file"){
    if(f$request$headers["X-API-TOKEN"] == "1234") {
      ct <- readRDS("files/file_getSurvey.rds")
      f$content <- ct
    }
  }
  #browser()
  # Load raw zip file
  ty <- qualtrics_response_codes(f, raw=TRUE)
  # To zip file
  tf <- paste0(tempdir(),
               ifelse(substr(tempdir(),
                             nchar(tempdir()),
                             nchar(tempdir())) == "/",
                      "temp.zip",
                      "/temp.zip"))
  # Write to temporary file
  writeBin(ty$content, tf)
  # Try to unzip
  u <- tryCatch({
    utils::unzip(tf, exdir = tempdir())
  }, error = function(e) {
    stop(paste0("Error extracting ",
                "csv",
                " from zip file. Please re-run your query."))
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

infer_data_types <- function(data,
                             surveyID,
                             verbose = FALSE) {

  # Download survey metadata
  md <- metadata(surveyID, get=list("questions"=TRUE,
                                    "metadata"=FALSE,
                                    "responsecounts"=FALSE))[[1]]
  # Check which questions are of type allowed
  interest <- lapply(md, function(x) {
    # Check if question type supported
    type_supp <- ifelse(!is.null(x$questionType$type),
                        x$questionType$type %in%
                          getOption("QUALTRICS_INTERNAL_SETTINGS")$question_types_supported$type,
                        FALSE
    ) # This one is the most important so ifelse
    selector_supp <- ifelse(!is.null(x$questionType$selector),
                            x$questionType$selector %in%
                              getOption("QUALTRICS_INTERNAL_SETTINGS")$question_types_supported$selector,
                            FALSE)
    if(!is.null(x$questionType$subSelector)) {
      subselector_supp <- x$questionType$subSelector %in%
        getOption("QUALTRICS_INTERNAL_SETTINGS")$question_types_supported$subSelector
    } else {
      subselector_supp <- NA
    }
    # Evaluate
    supported <- ifelse(all(selector_supp, type_supp), TRUE, FALSE)
    # Check if question in survey
    question_is_in_survey <- x$questionName %in% names(data)
    # If both true, return element
    if(supported & question_is_in_survey) {
      return(x)
    }
  })
  # Remove NULL values
  interest <- interest[!vapply(interest, is.null, FALSE)]
  # Get value names
  mc <- vapply(interest, function(x) x$questionName, "character", USE.NAMES = FALSE)
  # Unfortunately, something goes wrong with the labels so we need to do this
  lab <- sjlabelled::get_label(data)
  # For each, convert
  #browser()
  for(m in mc) {
    data <- wrapper_mc(data, m, interest)
  }
  # Return labels
  data <- sjlabelled::set_label(data, lab)

  # Check if warning given
  if(Sys.getenv("QUALTRICS_WARNING_DATE_GIVEN") == "") {
    warning("The 'StartDate', 'EndDate' and 'RecordedDate' variables were converted without passing\na specific timezone. If you like to set these timestamps to your own timezone, please\nvisit https://www.qualtrics.com/support/survey-platform/getting-started/managing-your-account/\n(under 'User Settings'). See https://api.qualtrics.com/docs/dates-and-times for more\ninformation about how the Qualtrics API handles dates and times.")
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE)
  }
  # Return data
  return(data)
}

#' Convert multiple choice questions to ordered factors
#'
#' @param data Imported Qualtrics survey
#' @param col_name Column name
#' @param survey_meta Survey metadata
#'
#' @importFrom rlang ':='

wrapper_mc <- function(data, col_name, survey_meta) {
  # Idea: add ORDER = TRUE/FALSE (if user wants factors to be ordered). Add # REVERSE = TRUE/FALSE if user wants the factor levels to be reversed
  # Get question data from metadata
  meta <- survey_meta[vapply(survey_meta,
                             function(x) x$questionName == col_name, TRUE)]
  # Level names
  ln <- vapply(meta[[1]]$choices,
               function(x) x$choiceText, "character", USE.NAMES = FALSE)
  # Convert
  dplyr::mutate(data,
                !!col_name := readr::parse_factor(
                  dplyr::pull(dplyr::select(data,
                                            !!col_name)),
                  levels=ln,
                  ordered = TRUE
                ))

}
