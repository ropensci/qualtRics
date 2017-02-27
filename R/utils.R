# Helper function. Checks responses against qualtrics response codes.

qualtRicsResponseCodes <- function(res, raw=FALSE) {
  # Check status code and raise error/warning
  if(res$status_code == 200) {
    if(raw) {
      result <- content(res, "raw")
    } else {
      result <- content(res)
    }
    return(list(
      "content" = result,
      "OK" = TRUE
    )
    )
  } else if(res$status_code == 401) {
    stop("Qualtrics API raised an authentication (401) error - you may not have the required authorization. Please check your API key and root url.")
  } else if(res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on https://github.com/JasperHG90/qualtRics/issues")
  } else if(res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error). Please check if you are using the correct survey ID.")
  } else if(res$status_code == 500) {
    warning(paste0("Qualtrics API reports an internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 503) {
    warning(paste0("Qualtrics API reports a temporary internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and errorCode below or retry your query.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a multipart/form-data request is malformed.")
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  }
}

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

#' Construct a header to send to qualtrics API
#'
#' This function is not exported as it is a helper function. It should not be called directly by the user.
#'
#' @param API.TOKEN API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#'
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @examples
#' \dontrun{
#' registerApiKey("<YOUR-QUALTRICS-API-KEY>")
#' surveys <- getSurveys("https://leidenuniv.eu.qualtrics.com")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' mysurvey <- getSurvey(surveys$id[6],
#'                       format = "csv",
#'                       save_dir = tempdir(),
#'                       "https://leidenuniv.eu.qualtrics.com",
#'                       verbose=TRUE)
#' }

constructHeader <- function(API.TOKEN) {
  # Construct and return
  headers = c(
    'X-API-TOKEN' = API.TOKEN,
    'Content-Type' = "application/json",
    'Accept' = '*/*',
    'accept-encoding' = 'gzip, deflate'
  )
  return(headers)
}

#    Download qualtrics data into R
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

#' Retrieve a JSON file containing quiz metadata
#'
#' @param surveyID Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param root_url Base url for your institution (see \url{https://api.qualtrics.com/docs/root-url}. If you do not fill in anything, the function will use the default url. Using your institution-specific url can significantly speed up queries.)
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @examples
#' \dontrun{
#' registerApiKey("<YOUR-QUALTRICS-API-KEY>")
#' surveys <- getSurveys("https://leidenuniv.eu.qualtrics.com")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' mysurvey <- getSurvey(surveys$id[6],
#'                       format = "csv",
#'                       save_dir = tempdir(),
#'                       "https://leidenuniv.eu.qualtrics.com",
#'                       verbose=TRUE)
#' }

getSurveyMetadata <- function(surveyID,
                              root_url = "https://yourdatacenterid.qualtrics.com") {
  # Ensure that root url is character
  assertthat::assert_that(assertthat::is.string(root_url))
  # Look in temporary directory. If file 'qualtRics_header.rds' does not exist,
  # then abort and tell user to register API key first
  if(assert_apikey_stored(dir = tempdir())){
    # Read headers information
    headers <- readRDS(paste0(tempdir(), "/qualtRics_header.rds"))
  }
  # Function-specific API stuff
  root_url <- paste0(root_url,
                     ifelse(substr(root_url, nchar(root_url), nchar(root_url)) == "/",
                            "API/v3/surveys",
                            "/API/v3/surveys"))
  # Paste survey id
  root_url <- paste0(root_url,
                     "/",
                     surveyID)
  # Send GET request to list all surveys
  res <- GET(root_url, add_headers(headers))
  # Check response codes
  resp <- qualtRicsResponseCodes(res)
  # Check if status is OK
  if(resp$OK) {
    cnt <- resp$content
    # Get question information and map
    qi <- cnt$result$questions
    cm <- cnt$result$exportColumnMap
    # Return list of mapping data and quiz information
    md <- data.frame(
      "question" = names(cm),
      "QID" = sapply(cm, function(x) x$question),
      stringsAsFactors = FALSE
    )
    # Return
    return(
      list(
        "mapping" = md,
        "questions" = qi
      )
    )
  } else {
    return(resp$content)
  }
}
