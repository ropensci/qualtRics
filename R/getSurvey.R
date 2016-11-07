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
#' @param headers 'headers' object - returned by the 'constructHeader' function. See \link[qualtRics]{constructHeader}.
#' @param base_url Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. You need to supply this url. Your query will NOT work without it.)
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
#' @export
#' @examples
#' \dontrun{
#' head <- constructHeader("<YOUR-API-KEY-HERE>")
#' surveys <- getSurveys(head,
#'                       "https://leidenuniv.eu.qualtrics.com")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' mysurvey <- getSurvey(surveys$id[6],
#'                       head,
#'                       "https://leidenuniv.eu.qualtrics.com",
#'                       verbose=TRUE)
#' }

getSurvey <- function(surveyID, headers,
                      base_url,
                      verbose = FALSE) {

  # Function-specific API stuff
  survey_baseurl <- paste0(survey_baseurl,
                           ifelse(substr(survey_baseurl, nchar(survey_baseurl), nchar(survey_baseurl)) == "/",
                                  "API/v3/responseexports/",
                                  "/API/v3/responseexports/"))

  # Create raw JSON payload
  raw_payload <- paste0('{"format": "csv", "surveyId": ',
                        '"',
                        surveyID,
                        '",',
                        '"useLabels": true',
                        '}')
  # POST request for download
  res <- POST(base_url,
              add_headers(
                headers
              ),
              body = raw_payload
  )
  # Get content
  cnt <- content(res)
  # If http status == 200
  if(! cnt$meta$httpStatus == "200 - OK" ) {
    stop(paste0("Query returned status ",
                '"',
                cnt$meta$httpStatus,
                '"',
                " .Please check your code."))
  }
  # Get id
  ID = cnt$result$id
  # Monitor when export is ready
  progress <- 0
  check_url <- paste0(base_url, ID)
  while(progress < 100) {
    CU <- GET(check_url, add_headers(headers))
    progress <- content(CU)$result$percentComplete
    if( verbose ) {
      print(paste0("Download is ", progress, "% complete."))
    }
    Sys.sleep(3)
  }

  # Download file
  f <- tryCatch({
    GET(paste0(check_url, "/file"), add_headers(headers))
  }, error = function(e) {
    # Retry if first attempt fails
    GET(paste0(check_url, "/file"), add_headers(headers))
  })
  ty <- content(f, "raw")
  # To zip file
  tf <- paste0(tempdir(), "/temp.zip")
  writeBin(ty, tf)
  # Take snapshot
  SS <- list.files(tempdir())
  u <- tryCatch({
    unzip(tf, exdir = tempdir())
  }, error = function(e) {
    stop("Error extracting csv from zip file.")
  })
  data <- read.csv(u, header=TRUE, skip = 1, stringsAsFactors = FALSE)
  # Remove tmpfiles
  p <- file.remove(tf) ; p<- file.remove(u)
  # Return
  return(data[-1,])
}
