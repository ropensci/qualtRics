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

#' Register qualtrics API key
#'
#' This function registers the user's qualtrics API key for the remainder of the R session. This function only needs to be called once (at the beginning of each R session).
#'
#' @param API.TOKEN API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#'
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API.
#' @author Jasper Ginn
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
#'

registerApiKey <- function(API.TOKEN) {

  # Get temporary directory
  td <- tempdir()

  # Construct header to send to qualtrics API
  head <- constructHeader(API.TOKEN)

  # Save to temporary directory
  saveRDS(head, paste0(td, "/qualtRics_header.rds"))

  # Return
  return(TRUE)

}
