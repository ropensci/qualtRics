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
#' Export a qualtrics survey you own and import the survey directly into R. NOTE: If you keep getting errors try to use your institution's base URL. See \url{https://api.qualtrics.com/docs/root-url}.
#'
#' @param surveyID String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param root_url String. Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. You need to supply this url. Your query will NOT work without it.).
#' @param useLabels Boolean. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertStandardColumns Boolean. If TRUE, then the function will convert general data columns (first name, last name, lat, lon, ip address, startdate, enddate etc.) to their proper format. Defaults to TRUE
#' @param lastResponseId String. Export all responses received after the specified response. Defaults to NULL.
#' @param startDate Date. Filter to only exports responses recorded after the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.
#' @param endDate Date. Filter to only exports responses recorded before the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.
#' @param seenUnansweredRecode String. Recode seen but unanswered questions with a string value. Defaults to NULL.
#' @param limit Integer. Maximum number of responses exported. Defaults to NULL (all responses).
#' @param useLocalTime Boolean. Use local timezone to determine response date values. Defaults to FALSE.
#' @param save_dir String. Directory where survey results will be stored. Defaults to a temporary directory which is cleaned when your R session is terminated. This parameter is useful if you'd like to store survey results. The downloaded survey will be stored as an RDS file (see \link[base]{readRDS}).
#' @param force_request Boolean. getSurvey() saves each survey in a temporary directory so that it can quickly be retrieved later. If force_request is TRUE, getSurvey() always downloads the survey from the API instead of loading it from the temporary directory. Defaults to FALSE.
#' @param verbose Print verbose messages to the R console? Defaults to FALSE.
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
#' registerApiKey("<YOUR-QUALTRICS-API-KEY>")
#' surveys <- getSurveys("https://leidenuniv.eu.qualtrics.com")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' mysurvey <- getSurvey(surveys$id[6],
#'                       save_dir = tempdir(),
#'                       "https://leidenuniv.eu.qualtrics.com",
#'                       verbose=TRUE)
#' }

getSurvey <- function(surveyID,
                      root_url,
                      useLabels=TRUE,
                      convertStandardColumns = TRUE,
                      lastResponseId=NULL,
                      startDate=NULL,
                      endDate=NULL,
                      seenUnansweredRecode=NULL,
                      limit = NULL,
                      useLocalTime = FALSE,
                      save_dir=NULL,
                      force_request=FALSE,
                      verbose=FALSE) {
  # Check params
  checkParams(save_dir,
              root_url=root_url,
              seenUnansweredRecode = seenUnansweredRecode,
              limit=limit,
              check_qualtrics_api_key = TRUE)
  # See if survey already in tempdir
  if(!force_request) {
    if(paste0(surveyID, ".rds") %in% tempdir()) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, ".rds"))
      return(data)
    }
  }
  # add endpoint to root url
  root_url <- appendRootUrl(root_url, "responseexports")
  # Create raw JSON payload
  raw_payload <- createRawPayload(surveyID = surveyID,
                                  useLabels = useLabels,
                                  lastResponseId = lastResponseId,
                                  startDate = startDate,
                                  endDate = endDate,
                                  seenUnansweredRecode = seenUnansweredRecode,
                                  limit = limit,
                                  useLocalTime = useLocalTime)
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
  data <- readSurvey(survey.fpath, convertStandardColumns = convertStandardColumns)
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
