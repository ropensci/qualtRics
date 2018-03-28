#   Download qualtrics data into R
#    Copyright (C) 2018 Jasper Ginn

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

#' Retrieve a Data Frame of All Active Surveys on Qualtrics
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' registerOptions(api_token = "<YOUR-API-TOKEN>",
#'                 base_url = "<YOUR-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       saveDir = tempdir(),
#'                       verbose = TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       saveDir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       useLabels = TRUE,
#'                       seenUnansweredRecode = "UNANS",
#'                       verbose = TRUE)
#' }

getSurveys <- function() {

  # CHECK PARAMS AND PREP QUERY ----

  # Check params
  cp <- checkParams()
  # Function-specific API stuff
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "surveys")

  # SEND REQUEST TO QUALTRICS ----

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

  # WRAP-UP AND RETURN ----

  # Bind to one large data frame & return
  d <- bind_rows(master)
  return(d)


}
