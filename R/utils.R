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

#' Helper function. Checks responses against qualtrics response codes.
#' @param res response from httr::GET
#' @param raw if TRUE, add 'raw' flag to httr::content() function.

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
    stop(paste0("Qualtrics API reports an internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = content(res),
      "OK"= FALSE
    ))
  } else if(res$status_code == 503) {
    stop(paste0("Qualtrics API reports a temporary internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and errorCode below or retry your query.", "\n",
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
  # Check params
  checkParams(root_url=root_url, check_qualtrics_api_key=TRUE)
  # Function-specific API stuff
  root_url <- appendRootUrl(root_url, "surveys")
  # Add survey id
  root_url <- paste0(root_url,
                     "/",
                     surveyID)
  # GET request to download metadata
  resp <- qualtricsApiRequest("GET", root_url)
  # Get question information and map
  qi <- resp$result$questions
  cm <- resp$result$exportColumnMap
  # Return list of mapping data and quiz information
  md <- data.frame(
    # Replace all special characters
    "question" = gsub("[^[:alnum:]_]", "\\.", names(cm)),
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
}

#' Check if httr GET result contains a warning
#' @param resp object returned by 'qualtRicsResponseCodes()'

checkForWarnings <- function(resp) {
  # Raise warning if resp contains notice
  if(!is.null(resp$content$meta$notice)) {
    warning(resp$content$meta$notice)
  }
  NULL
}

#' Check if parameters passed to functions are correct
#' @param save_dir Directory where survey results will be stored. Defaults to a temporary directory which is cleaned when your R session is terminated. This parameter is useful if you'd like to store survey results.
#' @param check_qualtrics_api_key TRUE/FALSE statement. Does function need to check if qualtrics key exists?

checkParams <- function(save_dir = NULL,
                        root_url = NULL,
                        check_qualtrics_api_key = FALSE
                        ) {
  ### root_url
  if(!is.null(root_url)) {
    # Ensure that root url is character
    assertthat::assert_that(assertthat::is.string(root_url))
  }
  ### save_dir
  # Check if save_dir exists
  if(!is.null(save_dir)) {
    if(!file.info(save_dir)$isdir | is.na(file.info(save_dir)$isdir)) stop(paste0("The directory ", save_dir, " does not exist."))
  }
  ### check_qualtrics_api_key
  if(check_qualtrics_api_key) {
    # Look in temporary directory. If file 'qualtRics_header.rds' does not exist, then abort and tell user to register API key first
    assert_apikey_stored(tempdir())
  }
  ### ..
}

#' Append proper end points to create root url
#' @param root_url Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. You need to supply this url. Your query will NOT work without it.).
#' @return root url + appended end point

appendRootUrl <- function(root_url, type = c("responseexports", 'surveys')) {
  # match
  type <- match.arg(type)
  # Create root url
  root_url <- paste0(root_url,
                     ifelse(substr(root_url, nchar(root_url), nchar(root_url)) == "/",
                            paste0("API/v3/", type,"/"),
                            paste0("/API/v3/", type,"/")))
  # Return
  return(root_url)
}

#' Create raw JSON payload to post response exports request
#' @param

createRawPayload <- function(surveyID,
                             useLabels=TRUE,
                             lastResponseId=NULL,
                             startDate=NULL,
                             endDate=NULL) {
  paste0(
    '{"format": ', '"', 'csv', '"' ,
    ', "surveyId": ', '"', surveyID,
    ifelse(
      is.null(lastResponseId),
      "",
      paste0('"' ,
             ', "lastResponseId": ',
             '"',
             lastResponseId)
    ) ,
    ifelse(
      is.null(startDate),
      "",
      paste0('"' ,
             ', "startDate": ',
             '"',
             paste0(startDate,"T00:00:00Z"))
    ) ,
    ifelse(
      is.null(endDate),
      "",
      paste0('"' ,
             ', "endDate": ',
             '"',
             paste0(endDate,"T00:00:00Z"))
    ) , '", ',
    '"useLabels": ', tolower(useLabels),
    '}'
  )
}

#' Send httr requests to qualtrics API
#' @param

qualtricsApiRequest <- function(verb = c("GET", "POST"), url = url,
                                body = NULL, raw = FALSE) {
  # Match arg
  verb <- match.arg(verb)
  # Get headers information
  headers <- readRDS(paste0(tempdir(), "/qualtRics_header.rds"))
  # Send request to qualtrics API
  res <- httr::VERB(verb,
                    url = url,
                    config = add_headers(
                              headers
                            ),
                    body = body)
  # Check if response type is OK
  cnt <- qualtRicsResponseCodes(res, raw = raw)
  # Check if OK
  if(cnt$OK) {
    # If notice occurs, raise warning
    checkForWarnings(cnt)
    # return content
    return(cnt$content)
  }
}

#' Download response export
#' @param

downloadQualtricsExport <- function(check_url, verbose = FALSE) {
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
    CU <- qualtricsApiRequest(check_url)
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
    GET(paste0(check_url, "/file"), add_headers(headers))
  }, error = function(e) {
    # Retry if first attempt fails
    GET(paste0(check_url, "/file"), add_headers(headers))
  })
  # Load raw zip file
  ty <- qualtRicsResponseCodes(f, raw=TRUE)
  # Check for notice
  checkForWarnings(f)
  # To zip file
  tf <- paste0(tempdir(),
               ifelse(substr(tempdir(), nchar(tempdir()), nchar(tempdir())) == "/",
                      "temp.zip",
                      "/temp.zip"))
  # Write to temporary file
  writeBin(ty$content, tf)
  # Try to unzip
  u <- tryCatch({
    unzip(tf, exdir = tempdir())
  }, error = function(e) {
    stop(paste0("Error extracting ", "csv", " from zip file. Please re-run your query."))
  })
  # Return file location
  return(u)
}
