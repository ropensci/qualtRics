#' Download a survey and import it into R
#'
#' Download a Qualtrics survey you own via API and import the survey directly into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#' Returned as \code{id} by the \link[qualtRics]{all_surveys} function.
#' @param last_response Deprecated.
#' @param start_date String. Filter to only exports responses recorded after the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to \code{NULL}.
#' @param end_date String. Filter to only exports responses recorded before the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to \code{NULL}.
#' @param unanswer_recode Integer. Recode seen but unanswered questions with an
#' integer-like value, such as 999. Defaults to \code{NULL}.
#' @param unanswer_recode_multi Integer. Recode seen but unanswered multi-select
#' questions with an integer-like value, such as 999. Defaults to value for
#' \code{unaswer_recode}.
#' @param include_display_order Display order information (such as for
#' surveys with randomization).
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
#' @param import_id Logical. If \code{TRUE}, use Qualtrics import IDs instead of
#' question IDs as column names. Will also alter names in the column map, if
#' used. Defaults to \code{FALSE}.
#' @param time_zone String. A local timezone to determine response date
#' values. Defaults to \code{NULL} which corresponds to UTC time. See
#' ["Dates and Times"](https://api.qualtrics.com/instructions/) from Qualtrics
#' for more information on format.
#' @param breakout_sets Logical. If \code{TRUE}, then the
#' \code{\link[qualtRics]{fetch_survey}} function will split multiple
#' choice question answers into columns. If \code{FALSE}, each multiple choice
#' question is one column. Defaults to \code{TRUE}.
#' @param add_column_map Logical. If \code{TRUE}, then a column map data frame
#' will be added as an attribute to the main response data frame.
#' This column map captures Qualtrics-provided metadata associated with the
#' response download, such as an item description and internal ID's. Defaults to
#' \code{TRUE}.
#' @param add_var_labels Logical. If \code{TRUE}, then the item description from
#' each variable (equivalent to the one in the column map) will be added as a
#' "label" attribute using \code{\link[sjlabelled]{set_label}}. Useful for
#' reference as well as cross-compatibility with other stats packages (e.g.,
#' Stata, see documentation in \code{sjlabelled}). Defaults to \code{TRUE}.
#' @param col_types Optional. This argument provides a way to manually overwrite
#' column types that may be incorrectly guessed. Takes a \code{\link[readr]{cols}}
#' specification. See example below and \code{\link[readr]{cols}} for formatting
#' details. Defaults to \code{NULL}. Overwritten by \code{convert = TRUE}.
#' @param ... Optional arguments, such as a `fileEncoding` (see `fileEncoding`
#' argument in \code{\link[qualtRics]{read_survey}}) to import your survey using
#' a specific encoding.
#'
#' @seealso See \url{https://api.qualtrics.com/} for documentation on
#' the Qualtrics API.
#'
#' @importFrom lifecycle deprecated
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
#'   unanswer_recode = 999,
#'   verbose = TRUE,
#'   # Manually override EndDate to be a character vector
#'   col_types = readr::cols(EndDate = readr::col_character())
#' )
#'
#' }
#'
fetch_survey <- function(surveyID,
                         last_response = deprecated(),
                         start_date = NULL,
                         end_date = NULL,
                         unanswer_recode = NULL,
                         unanswer_recode_multi = unanswer_recode,
                         include_display_order = TRUE,
                         limit = NULL,
                         include_questions = NULL,
                         save_dir = NULL,
                         force_request = FALSE,
                         verbose = TRUE,
                         label = TRUE,
                         convert = TRUE,
                         import_id = FALSE,
                         time_zone = NULL,
                         breakout_sets = TRUE,
                         add_column_map = TRUE,
                         add_var_labels = TRUE,
                         col_types = NULL,
                         ...) {

  if (lifecycle::is_present(last_response)) {
    lifecycle::deprecate_warn("3.1.2", "fetch_survey(last_response = )")
  }

  ## Are the API credentials stored?
  assert_base_url()
  assert_api_key()

  check_params(
    verbose = verbose,
    convert = convert,
    import_id = import_id,
    time_zone = time_zone,
    label = label,
    start_date = start_date,
    end_date = end_date,
    include_questions = include_questions,
    save_dir = save_dir,
    unanswer_recode = unanswer_recode,
    unanswer_recode_multi = unanswer_recode_multi,
    include_display_order = include_display_order,
    limit = limit,
    breakout_sets = breakout_sets,
    add_column_map = add_column_map,
    add_var_labels = add_var_labels
  )

  # See if survey already in tempdir
  if (!force_request) {
    if (paste0(surveyID, ".rds") %in% list.files(tempdir())) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, ".rds"))
      if (verbose) {
        rlang::inform(paste0(
          "Found an earlier download for survey with id ", surveyID, # nolint
          ". Loading this file.\nSet 'force_request' to TRUE if you want to override this."
        ))
      } # nolint
      return(data)
    }
  }

  # CONSTRUCT API CALL ----

  # fetch URL:
  fetch_url <- generate_url(query = "fetchsurvey",
                            surveyID = surveyID)

  # Create raw JSON payload
  raw_payload <- create_raw_payload(
    label = label,
    start_date = start_date,
    end_date = end_date,
    unanswer_recode = unanswer_recode,
    unanswer_recode_multi = unanswer_recode_multi,
    include_display_order = include_display_order,
    limit = limit,
    time_zone = time_zone,
    include_questions = include_questions,
    breakout_sets = breakout_sets
  )

  # SEND POST REQUEST TO API ----

  # POST request for download
  res <- qualtrics_api_request("POST", url = fetch_url, body = raw_payload)
  # Get id
  if (is.null(res$result$progressId)) {
      stop("Something went wrong. Please re-run your query.")
    } else {
    requestID <- res$result$progressId
  } # NOTE This is not fail safe because ID can still be NULL

  # Download, unzip and return file path
  survey.fpath <- download_qualtrics_export(fetch_url, requestID, verbose = verbose)

  # READ DATA AND SET VARIABLES ----

  # Read data

  data <- read_survey(survey.fpath, import_id = import_id,
                      time_zone = time_zone,
                      col_types = col_types,
                      add_column_map = add_column_map)

  # Add types
  if (convert & label) {
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
