#   Download qualtrics data into R
#    Copyright (C) 2017 Jasper Ginn

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Export a survey and download into R
#'
#' Export a qualtrics survey you own and import the survey directly into R.
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
#' @param ... optional arguments. You can pass all arguments listed in \code{\link{registerOptions}}. You can also pass a argument 'fileEncoding' (see 'fileEncoding' argument in \code{\link{readSurvey}}) to import your survey using a specific encoding.
#'
#' @seealso See \url{https://api.qualtrics.com/docs/csv} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom stringr str_sub
#' @importFrom utils read.csv
#' @importFrom utils unzip
#' @importFrom utils write.csv
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       verbose=TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       useLabels = TRUE,
#'                       seenUnansweredRecode = "UNANS",
#'                       verbose=TRUE)
#' # You can also choose to only download an export with specific questions using
#' # the \code{\link[getSurveyQuestions]{getSurveyQuestions}} function.
#'
#' # Retrieve questions for a survey
#' questions <- getSurveyQuestions(surveyID = surveys$id[6])
#' # Retrieve a single survey, filtering for questions I want.
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       includedQuestionIds = c("QID1", "QID2", "QID3"),
#'                       verbose=TRUE)
#' }

getSurvey <- function(surveyID,
                      lastResponseId=NULL,
                      startDate=NULL,
                      endDate=NULL,
                      seenUnansweredRecode=NULL,
                      limit = NULL,
                      includedQuestionIds = NULL,
                      save_dir=NULL,
                      force_request=FALSE,
                      ...) {
  # Options
  opts <- list(...)
  verbose <- ifelse("verbose" %in% names(opts), opts$verbose,
                    getOption("QUALTRICS_VERBOSE"))
  convertStandardColumns <- ifelse("convertStandardColumns" %in% names(opts),
                                   opts$convertStandardColumns,
                                   getOption("QUALTRICS_CONVERTSTANDARDCOLUMNS"))
  useLocalTime <- ifelse("useLocalTime" %in% names(opts), opts$useLocalTime,
                         getOption("QUALTRICS_USELOCALTIME"))
  useLabels <- ifelse("useLabels" %in% names(opts), opts$useLabels,
                      getOption("QUALTRICS_USELABELS"))
  fileEncoding <- ifelse("fileEncoding" %in% names(opts), opts$fileEncoding,
                         "")
  # Check params
  checkParams(verbose=verbose,
              convertStandardColumns=convertStandardColumns,
              useLocalTime=useLocalTime,
              useLabels=useLabels,
              lastResponseId=lastResponseId,
              startDate=startDate,
              endDate=endDate,
              includedQuestionIds=includedQuestionIds,
              save_dir=save_dir,
              seenUnansweredRecode=seenUnansweredRecode,
              limit=limit)
  # See if survey already in tempdir
  if(!force_request) {
    if(paste0(surveyID, ".rds") %in% list.files(tempdir())) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, ".rds"))
      if(verbose) message(paste0("Found an earlier download for survey with id ", surveyID,
                                 ". Loading this file. Set 'force_request' to TRUE if you want to override this."))
      return(data)
    }
  }
  # add endpoint to root url
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "responseexports")
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
  # POST request for download
  res <- qualtricsApiRequest("POST", url=root_url, body = raw_payload)
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
  # Read data
  data <- readSurvey(survey.fpath, convertStandardColumns = convertStandardColumns, fileEncoding = fileEncoding)
  # Save survey as RDS file in temp folder so that it can be easily retrieved this session.
  saveRDS(data, paste0(tempdir(), "/", surveyID, ".rds"))
  # Remove tmpfiles
  if(!is.null(save_dir)) {
    # Save file to directory
    saveRDS(data, file=paste0(save_dir, "/", surveyID, ".rds"))
    # Return
    return(data)
  } else {
    p<- file.remove(survey.fpath)
    # Return
    return(data)
  }
}
