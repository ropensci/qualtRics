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
#'                       "https://leidenuniv.eu.qualtrics.com",
#'                       verbose=TRUE)
#' }

getSurveys <- function(root_url = "https://yourdatacenterid.qualtrics.com") {

  # Look in temporary directory. If file 'qualtRics_header.rds' does not exist, then abort and tell user to register API key first
  f <- list.files(tempdir())
  if(!"qualtRics_header.rds" %in% f) stop("You need to register your qualtrics API key first using the 'registerApiKey()' function.")

  # Read headers information
  headers <- readRDS(paste0(tempdir(), "/qualtRics_header.rds"))

  # Function-specific API stuff
  root_url <- paste0(root_url,
                           ifelse(substr(root_url, nchar(root_url), nchar(root_url)) == "/",
                                  "API/v3/surveys",
                                  "/API/v3/surveys"))
  # Send GET request to list all surveys
  res <- GET(root_url, add_headers(headers))
  # Return
  return(do.call(rbind.data.frame, content(res)$result$elements))
}
