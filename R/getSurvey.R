#' Export a survey and download it into R
#'
#' Export a Qualtrics survey you own and import the survey directly into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param lastResponseId String. Export all responses received after the specified response. Defaults to NULL.
#' @param startDate String. Filter to only exports responses recorded after the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.
#' @param endDate String. Filter to only exports responses recorded before the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.
#' @param seenUnansweredRecode String. Recode seen but unanswered questions with a string value. Defaults to NULL.
#' @param limit Integer. Maximum number of responses exported. Defaults to NULL (all responses).
#' @param includedQuestionIds Vector of strings (e.g. c('QID1', 'QID2', 'QID3'). Export only specified questions. Defaults to NULL.
#' @param save_dir String. Directory where survey results will be stored. Defaults to a temporary directory which is cleaned when your R session is terminated. This argument is useful if you'd like to store survey results. The downloaded survey will be stored as an RDS file (see \link[base]{readRDS}).
#' @param force_request Logical. getSurvey() saves each survey in a temporary directory so that it can quickly be retrieved later. If force_request is TRUE, getSurvey() always downloads the survey from the API instead of loading it from the temporary directory. Defaults to FALSE.
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertVariables Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param ... optional arguments. You can pass all arguments listed in \code{\link{registerOptions}} (except a different base url / api key). You can also pass a argument 'fileEncoding' (see 'fileEncoding' argument in \code{\link{readSurvey}}) to import your survey using a specific encoding.
#'
#' @seealso See \url{https://api.qualtrics.com/docs/csv} for documentation on the Qualtrics API.
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(api_key = "<YOUR-API-KEY>",
#'                           base_url = "<YOUR-BASE-URL>")
#'
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#'
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6])
#'
#' # You can use the same parameters as those found in the Qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       startDate = "2018-01-01",
#'                       endDate = "2018-01-31",
#'                       limit = 100,
#'                       useLabels = TRUE,
#'                       seenUnansweredRecode = "UNANS",
#'                       verbose = TRUE)
#' }

getSurvey <- function(surveyID,
                      lastResponseId = NULL,
                      startDate = NULL,
                      endDate = NULL,
                      seenUnansweredRecode = NULL,
                      limit = NULL,
                      includedQuestionIds = NULL,
                      save_dir = NULL,
                      force_request = FALSE,
                      verbose = TRUE,
                      useLabels = TRUE,
                      convertVariables = TRUE,
                      useLocalTime = FALSE,
                      ...) {

  # OPTIONS AND CHECK PARAMETERS ----

  opts <- list(...)
  # Get all arguments passed
  calls <- names(vapply(match.call(), deparse, "character"))[-1]

  # Options

  parse_opts <- function(flag, option_flag) {
    ifelse(!is.null(getOption(option_flag)),
           getOption(option_flag),
           flag)
  }

  verbose <- parse_opts(verbose, "QUALTRICS_VERBOSE")
  convertVariables <- parse_opts(convertVariables,
                                 "QUALTRICS_CONVERTVARIABLES")
  useLocalTime <- parse_opts(useLocalTime,
                             "QUALTRICS_USELOCALTIME")
  useLabels <- parse_opts(useLabels,
                          "QUALTRICS_USELABELS")

  # Check params

  ## Are the API credentials stored?
  assert_base_url()
  assert_api_key()

  check_params(verbose = verbose,
               convertVariables = convertVariables,
               useLocalTime = useLocalTime,
               useLabels = useLabels,
               lastResponseId = lastResponseId,
               startDate = startDate,
               endDate = endDate,
               includedQuestionIds = includedQuestionIds,
               save_dir = save_dir,
               seenUnansweredRecode = seenUnansweredRecode,
               limit = limit)

  # See if survey already in tempdir
  if(!force_request) {
    if(paste0(surveyID, ".rds") %in% list.files(tempdir())) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, ".rds"))
      if(verbose) message(paste0("Found an earlier download for survey with id ", surveyID, # nolint
                                 ". Loading this file.\nSet 'force_request' to TRUE if you want to override this.")) # nolint
      return(data)
    }
  }

  # CONSTRUCT API CALL ----

  # add endpoint to root url
  root_url <- append_root_url(Sys.getenv("QUALTRICS_BASE_URL"), "responseexports")
  # Create raw JSON payload
  raw_payload <- createRawPayload(surveyID = surveyID,
                                  useLabels = useLabels,
                                  lastResponseId = lastResponseId,
                                  startDate = startDate,
                                  endDate = endDate,
                                  seenUnansweredRecode = seenUnansweredRecode,
                                  limit = limit,
                                  useLocalTime = useLocalTime,
                                  includedQuestionIds = includedQuestionIds)

  # SEND POST REQUEST TO API ----

  # POST request for download
  res <- qualtricsApiRequest("POST", url = root_url, body = raw_payload)
  # Get id
  if(is.null(res$result$id)) {
    if(is.null(res$content[[1]]$id)) {
      stop("Something went wrong. Please re-run your query.")
    } else{
      ID <- res$content[[1]]$id
    }
  } else{
    ID <- res$result$id
  } # NOTE This is not fail safe because ID can still be NULL
  # This is the url to use when checking the ID
  check_url <- paste0(root_url, ID)
  # Download, unzip and return file path
  survey.fpath <- downloadQualtricsExport(check_url, verbose = verbose)

  # READ DATA AND SET VARIABLES ----

  # Read data
  data <- readSurvey(survey.fpath)
  # Add types
  if(convertVariables) {
    data <- inferDataTypes(data, surveyID)
  }
  # Save survey as RDS file in temp folder so that it can be easily
  # retrieved this session.
  saveRDS(data, paste0(tempdir(), "/", surveyID, ".rds"))

  # RETURN ----

  # Remove tmpfiles
  if(!is.null(save_dir)) {
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
