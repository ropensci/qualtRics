#   Download qualtrics data into R
#    Copyright (C) 2016 Jasper Ginn

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
#' @param surveyID Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param root_url Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. You need to supply this url. Your query will NOT work without it.).
#' @param infer_types If TRUE, the function will download additional information about the survey from Qualtrics that allow it to assign proper data types. For example, multiple choice questions will be turned into factors with the correct labels (and order), slider questions will be turned into percentages and so on. See the qualtRics vignette for more information.
#' @param useLabels TRUE to export survey responses as Choice Text or FALSE to export survey responses as values
#' @param lastResponseId Export all responses received after the specified response
#' @param startDate Date range filter to only exports responses recorded after the specified date. Accepts dates as character strings in format "YYYY-MM-DD"
#' @param endDate Date range filter to only exports responses recorded before the specified date. Accepts dates as character strings in format "YYYY-MM-DD"
#' @param save_dir Directory where survey results will be stored. Defaults to a temporary directory which is cleaned when your R session is terminated. This parameter is useful if you'd like to store survey results.
#' @param force_request getSurvey() saves each survey in a temporary directory so that it can quickly be retrieved later. If force_request is TRUE, getSurvey() always downloads the survey from the API instead of loading it from the temporary directory.
#' @param verbose Print verbose messages to the R console? Defaults to FALSE
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
                      infer_types=FALSE,
                      useLabels=TRUE,
                      lastResponseId=NULL,
                      startDate=NULL,
                      endDate=NULL,
                      save_dir=NULL,
                      force_request=FALSE,
                      verbose=FALSE) {
  # Check params
  checkParams(save_dir, check_qualtrics_api_key = TRUE)
  # If infer_types == TRUE, then useLabels must likewise be TRUE
  if(infer_types) {
    if(useLabels == FALSE) useLabels <- TRUE
  }
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
  raw_payload <- createRawPayload(surveyID,
                                  useLabels,
                                  lastResponseId,
                                  startDate,
                                  endDate)
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
  data <- readSurvey(survey.fpath)
  # Save survey as RDS file in temp folder so that it can be easily retrieved this session.
  saveRDS(data, paste0(tempdir(), "/", surveyID, ".rds"))
  # Remove tmpfiles
  if(!is.null(save_dir)) {
    # Save file to directory
    write.csv(data, file=paste0(save_dir, "/", surveyID, ".csv"))
    # Return
    return(data)
  } else {
    p<- file.remove(survey.fpath)
    # Return
    return(data)
  }
}
