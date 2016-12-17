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

#' Retrieve a list of all active surveys that you own on qualtrics
#'
#' @param root_url Base url for your institution (see \url{https://api.qualtrics.com/docs/root-url}. If you do not fill in anything, the function will use the default url. Using your institution-specific url can significantly speed up queries.)
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @export
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

getSurveys <- function(root_url = "https://yourdatacenterid.qualtrics.com") {

  # Assert types
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
  # Send GET request to list all surveys
  res <- GET(root_url, add_headers(headers))
  # Check status code and raise error/warning
  if(res$status_code == 401) {
    stop("Qualtrics API raised 401 error - you may not have the required authorization. Please check your API key and root url.")
  } else if(res$status_code == 200) {
    # Return
    return(do.call(rbind.data.frame, content(res)$result$elements))
  } else {
    warning(paste0("Qualtrics API raised ", as.character(res$status_code),
                   " . Trying to return result."))
    # Try to return
    return(do.call(rbind.data.frame, content(res)$result$elements))
  }
}
