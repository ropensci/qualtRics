#' Download a survey and import it into R
#'
#' This function is soft deprecated; use \code{\link[qualtRics]{fetch_survey}}
#' instead.
#' @param ... All arguments for \code{fetch_survey}
#'
#' @export
getSurvey <- function(...) {
  warning("Soon, `getSurvey` will be deprecated. Try using `fetch_survey()` instead.")
  fetch_survey(...)
}



#' Download a survey and import it into R
#'
#' Download a Qualtrics survey you own via API and import the survey directly into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#' Returned as \code{id} by the \link[qualtRics]{all_surveys} function.
#' @param last_response String. Export all responses received after the
#' specified response ID. Defaults to \code{NULL}.
#' @param start_date String. Filter to only exports responses recorded after the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to \code{NULL}.
#' @param end_date String. Filter to only exports responses recorded before the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to \code{NULL}.
#' @param unanswer_recode String. Recode seen but unanswered questions with a
#' string value. Defaults to \code{NULL}.
#' @param limit Integer. Maximum number of responses exported. Defaults to
#' \code{NULL} (all responses).
#' @param include_questions Vector of strings (e.g. c('QID1', 'QID2', 'QID3').
#' Export only specified questions. Defaults to \code{NULL}.
#' @param save_dir String. Directory where survey results will be stored.
#' Defaults to a temporary directory which is cleaned when your R session is
#' terminated. This argument is useful if you'd like to store survey results.
#' The downloaded survey will be stored as an RDS file (see
#' \code{\link[base]{readRDS}}).
#' @param force_request Logical. fetch_survey() saves each survey in a temporary
#' directory so that it can quickly be retrieved later. If force_request is
#' \code{TRUE}, fetch_survey() always downloads the survey from the API instead
#' of loading it from the temporary directory. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, verbose messages will be printed to
#' the R console. Defaults to \code{TRUE}.
#' @param label Logical. \code{TRUE} to export survey responses as Choice Text
#' or \code{FALSE} to export survey responses as values.
#' @param convert Logical. If \code{TRUE}, then the
#' \code{\link[qualtRics]{fetch_survey}} function will convert certain question
#' types (e.g. multiple choice) to proper data type in R. Defaults to \code{TRUE}.
#' @param local_time Logical. Use local timezone to determine response date
#' values? Defaults to \code{FALSE}. See
#' \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param ... optional arguments. You can pass all arguments listed in
#' \code{\link{registerOptions}} (except a different base url / api key).
#' You can also pass a argument 'fileEncoding' (see 'fileEncoding' argument in
#' \code{\link{readSurvey}}) to import your survey using a specific encoding.
#'
#' @seealso See \url{https://api.qualtrics.com/docs/csv} for documentation on the Qualtrics API.
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#'
#' # Retrieve a list of surveys
#' surveys <- all_surveys()
#'
#' # Retrieve a single survey
#' mysurvey <- fetch_survey(surveyID = surveys$id[6])
#'
#' mysurvey <- fetch_survey(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   start_date = "2018-01-01",
#'   end_date = "2018-01-31",
#'   limit = 100,
#'   label = TRUE,
#'   unanswer_recode = "UNANS",
#'   verbose = TRUE
#' )
#' }
#'
fetch_survey <- function(surveyID,
                         last_response = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         unanswer_recode = NULL,
                         limit = NULL,
                         include_questions = NULL,
                         save_dir = NULL,
                         force_request = FALSE,
                         verbose = TRUE,
                         label = TRUE,
                         convert = TRUE,
                         local_time = FALSE,
                         ...) {

  # OPTIONS AND CHECK PARAMETERS ----

  opts <- list(...)
  # Get all arguments passed
  calls <- names(vapply(match.call(), deparse, "character"))[-1]

  # Options

  parse_opts <- function(flag, option_flag) {
    ifelse(!is.null(getOption(option_flag)),
      getOption(option_flag),
      flag
    )
  }

  verbose <- parse_opts(verbose, "QUALTRICS_VERBOSE")
  convert <- parse_opts(
    convert,
    "QUALTRICS_CONVERTVARIABLES"
  )
  local_time <- parse_opts(
    local_time,
    "QUALTRICS_USELOCALTIME"
  )
  label <- parse_opts(
    label,
    "QUALTRICS_USELABELS"
  )

  # Check params

  ## Are the API credentials stored?
  assert_base_url()
  assert_api_key()

  check_params(
    verbose = verbose,
    convert = convert,
    local_time = local_time,
    label = label,
    last_response = last_response,
    start_date = start_date,
    end_date = end_date,
    include_questions = include_questions,
    save_dir = save_dir,
    unanswer_recode = unanswer_recode,
    limit = limit
  )

  # See if survey already in tempdir
  if (!force_request) {
    if (paste0(surveyID, ".rds") %in% list.files(tempdir())) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, ".rds"))
      if (verbose) {
        message(paste0(
          "Found an earlier download for survey with id ", surveyID, # nolint
          ". Loading this file.\nSet 'force_request' to TRUE if you want to override this."
        ))
      } # nolint
      return(data)
    }
  }

  # CONSTRUCT API CALL ----

  # add endpoint to root url
  root_url <- append_root_url(Sys.getenv("QUALTRICS_BASE_URL"), "responseexports")
  # Create raw JSON payload
  raw_payload <- create_raw_payload(
    surveyID = surveyID,
    label = label,
    last_response = last_response,
    start_date = start_date,
    end_date = end_date,
    unanswer_recode = unanswer_recode,
    limit = limit,
    local_time = local_time,
    include_questions = include_questions
  )

  # SEND POST REQUEST TO API ----

  # POST request for download
  res <- qualtrics_api_request("POST", url = root_url, body = raw_payload)
  # Get id
  if (is.null(res$result$id)) {
    if (is.null(res$content[[1]]$id)) {
      stop("Something went wrong. Please re-run your query.")
    } else {
      ID <- res$content[[1]]$id
    }
  } else {
    ID <- res$result$id
  } # NOTE This is not fail safe because ID can still be NULL
  # This is the url to use when checking the ID
  check_url <- paste0(root_url, ID)
  # Download, unzip and return file path
  survey.fpath <- download_qualtrics_export(check_url, verbose = verbose)

  # READ DATA AND SET VARIABLES ----

  # Read data
  data <- readSurvey(survey.fpath)
  # Add types
  if (convert) {
    data <- infer_data_types(data, surveyID)
  }
  # Save survey as RDS file in temp folder so that it can be easily
  # retrieved this session.
  saveRDS(data, paste0(tempdir(), "/", surveyID, ".rds"))

  # RETURN ----

  # Remove tmpfiles
  if (!is.null(save_dir)) {
    # Save file to directory
    saveRDS(data, file = paste0(save_dir, "/", surveyID, ".rds"))
    # Return
    return(data)
  } else {
    p <- file.remove(survey.fpath)
    # Return
    return(data)
  }
}
