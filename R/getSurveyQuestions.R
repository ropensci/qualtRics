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

#' Retrieve a data frame containing question IDs and labels
#'
#' @param surveyID String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#' @author Jasper Ginn, Phoebe Wong
#' @importFrom dplyr as_tibble
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve questions for a survey
#' questions <- getSurveyQuestions(surveyID = surveys$id[6])
#' # Retrieve a single survey, filtering for questions I want.
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       includedQuestionIds = c("QID1", "QID2", "QID3"),
#'                       verbose=TRUE)
#' }

getSurveyQuestions <- function(surveyID) {
  # Check params
  checkParams()
  # Function-specific API stuff
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "surveys")
  # Add survey id
  root_url <- paste0(root_url,
                     "/",
                     surveyID)
  # GET request to download metadata
  resp <- qualtricsApiRequest("GET", root_url)
  # Get question information and map
  qi <- resp$result$questions
  # Add questions, question labels, question names and force response info
  quest <- data.frame(qid = names(qi),
                      qnames = vapply(qi, function(x) x$questionName, ""),
                      question = vapply(qi,function(x) x$questionText, ""),
                      force_resp = vapply(qi, function(x) x$validation$doesForceResponse, TRUE),
                      stringsAsFactors = FALSE)

  # Row names
  row.names(quest) <- seq_along(1:nrow(quest))
  # Return
  return(dplyr::as_tibble(quest))
}
