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

#' Retrieve a data frame of all active surveys on Qualtrics
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
#' # Retrieve a list of surveys
#' surveys <- getSurveys(root_url = "https://leidenuniv.eu.qualtrics.com")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       root_url = "https://leidenuniv.eu.qualtrics.com",
#'                       save_dir = tempdir(),
#'                       verbose=TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       root_url = "https://leidenuniv.eu.qualtrics.com",
#'                       save_dir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       useLabels = TRUE,
#'                       seenUnansweredRecode = "UNANS",
#'                       verbose=TRUE)
#' }

getSurveys <- function(root_url = "https://yourdatacenterid.qualtrics.com") {
  # Check params
  cp <- checkParams(root_url=root_url, check_qualtrics_api_key=TRUE)
  # Function-specific API stuff
  root_url <- appendRootUrl(root_url, "surveys")
  # Send GET request to list all surveys
  resp <- qualtricsApiRequest("GET", root_url)
  # Put results in list
  master <- list()
  # Append results
  master <- append(master, resp$result$elements)
  # If nextPage != null, keep calling
  while(!is.null(resp$result$nextPage)) {
    # Send GET request to list all surveys
    resp <- qualtricsApiRequest("GET", resp$result$nextPage)
    # Append results
    master <- append(master, resp$result$elements)
  }
  # Bind to one large data frame & return
  return(do.call(rbind.data.frame, master))
}
