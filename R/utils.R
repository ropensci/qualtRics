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

# Helper function. Checks responses against qualtrics response codes.
#
# @param res response from httr::GET
# @param raw if TRUE, add 'raw' flag to httr::content() function.
#
# @author Jasper Ginn

qualtRicsResponseCodes <- function(res, raw=FALSE) {
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
      "content" = httr::content(res),
      "OK"= FALSE
    ))
  } else if(res$status_code == 503) {
    stop(paste0("Qualtrics API reports a temporary internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and errorCode below or retry your query.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a multipart/form-data request is malformed.")
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  }
}

# Construct a header to send to qualtrics API
#
# This function is not exported as it is a helper function. It should not be called directly by the user.
#
# @param API.TOKEN API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#
# @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API.
# @author Jasper Ginn

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

# Check if httr GET result contains a warning
# @param resp object returned by 'qualtRicsResponseCodes()'

checkForWarnings <- function(resp) {
  # Raise warning if resp contains notice
  if(!is.null(resp$content$meta)) {
    if(!is.null(resp$content$meta$notice)) {
      warning(resp$content$meta$notice)
    }
  }
  NULL
}

# Check if parameters passed to functions are correct
# @param ... options passed to checkParams

checkParams <- function(...) {
  args <- list(...)
  ### root_url
  assert_rootUrl_stored()
  ### API Token
  assert_apikey_stored()
  ### Options
  if(all(c("verbose", "convertStandardColumns",
           "useLocalTime", "useLabels") %in% names(args))) {
    assert_options_logical(
      args$verbose,
      args$convertStandardColumns,
      args$useLocalTime,
      args$useLabels
    )
  }

  # Check if params are of the right type
  if("startDate" %in% names(args)) {
    if(!is.null(args$startDate)) assert_startDate_string(args$startDate)
  }
  if("endDate" %in% names(args)) {
    if(!is.null(args$endDate)) assert_endDate_string(args$endDate)
  }
  if("lastResponseId" %in% names(args)) {
    if(!is.null(args$lastResponseId)) assert_lastResponseId_string(args$lastResponseId)
  }
  # Check if save_dir exists
  if("save_dir" %in% names(args)) {
    if(!is.null(args$save_dir)) assert_saveDir_exists(args$save_dir)
  }
  # Check if seenUnansweredRecode is NULL or else a string
  if("seenUnansweredRecode" %in% names(args)) {
    if(!is.null(args$seenUnansweredRecode)) assert_seenUnansweredRecode_string(args$seenUnansweredRecode)
  }
  # Check if limit > 0
  if("limit" %in% names(args)) {
    if(!is.null(args$limit)) assert_limit_abovezero(args$limit)
  }
  # Check if includedQuestionIds is a string
  if("includedQuestionIds" %in% names(args)) {
    if(!is.null(args$includedQuestionIds)) assert_includedQuestionIds_string(args$includedQuestionIds)
  }
}

# Append proper end points to create root url
# @param root_url Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. You need to supply this url. Your query will NOT work without it.).
# @return root url + appended end point

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

# Create raw JSON payload to post response exports request
# @param

createRawPayload <- function(surveyID,
                             useLabels=TRUE,
                             lastResponseId=NULL,
                             startDate=NULL,
                             endDate=NULL,
                             limit=NULL,
                             useLocalTime=FALSE,
                             seenUnansweredRecode=NULL,
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

# Send httr requests to qualtrics API
# @param

qualtricsApiRequest <- function(verb = c("GET", "POST"), url = url,
                                body = NULL, raw = FALSE) {
  # Match arg
  verb <- match.arg(verb)
  # Construct header
  headers <- constructHeader(Sys.getenv("QUALTRICS_API_KEY"))
  # Send request to qualtrics API
  res <- httr::VERB(verb,
                    url = url,
                    httr::add_headers(
                              headers
                            ),
                    body = body)
  # Check if response type is OK
  cnt <- qualtRicsResponseCodes(res, raw = raw)
  # Check if OK
  if(cnt$OK) {
    # If notice occurs, raise warning
    w <- checkForWarnings(cnt)
    # return content
    return(cnt$content)
  }
}

# Download response export
# @param

downloadQualtricsExport <- function(check_url, verbose = FALSE) {
  # Construct header
  headers <- constructHeader(Sys.getenv("QUALTRICS_API_KEY"))
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
    CU <- qualtricsApiRequest("GET", url = check_url)
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
  # Load raw zip file
  ty <- qualtRicsResponseCodes(f, raw=TRUE)
  # To zip file
  tf <- paste0(tempdir(),
               ifelse(substr(tempdir(), nchar(tempdir()), nchar(tempdir())) == "/",
                      "temp.zip",
                      "/temp.zip"))
  # Write to temporary file
  writeBin(ty$content, tf)
  # Try to unzip
  u <- tryCatch({
    utils::unzip(tf, exdir = tempdir())
  }, error = function(e) {
    stop(paste0("Error extracting ", "csv", " from zip file. Please re-run your query."))
  })
  # Remove zipfile
  p <- file.remove(tf)
  # Return file location
  return(u)
}

# Set proper data types on data
# @param
# @author Jasper Ginn

inferDataTypes <- function(data,
                           #surveyID,
                           #root_url,
                           verbose = FALSE) {
  # Download survey metadata
  #sm <- getSurveyMetadata(surveyID, root_url = root_url)
  # For each column, cycle and assign
  for(col.name in names(data)) {
    #print(col.name)
    # These are added to qualtrics surveys
    qNum <- c("LocationLatitude", "LocationLongitude", "Progress",
              "Duration..in.seconds")
    qChar <- c('IPAddress','ResponseID','RecipientLastName',
               'RecipientFirstName','RecipientEmail','ExternalDataReference',
               'ExternalReference', 'DistributionChannel') # Last two are unclear
    qFact <- c('ResponseSet')
    qBin <- c("Finished", "Status")
    qDate <- c('StartDate','EndDate', 'RecordedDate')
    # Check for generic data
    if(col.name %in% qNum) {
      data[,col.name] <- as.numeric(data[,col.name])
    } else if(col.name %in% qChar) {
      data[,col.name] <- as.character(data[,col.name])
    } else if(col.name %in% qFact) {
      data[,col.name] <- as.factor(data[,col.name])
    } else if(col.name %in% qBin) {
      data[,col.name] <- factor(data[,col.name], levels=c("0", "1"))
    } else if(col.name %in% qDate) {
      data[,col.name] <- lubridate::as_datetime(data[,col.name], tz=NULL)
    } else {
      NULL
    }

    # Check if warning given
    if(Sys.getenv("QUALTRICS_WARNING_DATE_GIVEN") == "") {
      warning("The 'StartDate', 'EndDate' and 'RecordedDate' variables were converted without passing a specific timezone. If you like to set these timestamps to your own timezone, please visit https://www.qualtrics.com/support/survey-platform/getting-started/managing-your-account/ (under 'User Settings'). See https://api.qualtrics.com/docs/dates-and-times for more information about how the Qualtrics API handles dates and times.")
      Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE)
    }


  }

  # Return data
  return(data)
}
