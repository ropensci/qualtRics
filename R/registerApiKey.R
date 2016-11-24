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

#' Register qualtrics API key
#'
#' @param API.TOKEN API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#'
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @export
#' @examples
#' \dontrun{
#' head <- constructHeader("<YOUR-API-KEY-HERE>")
#' surveys <- getSurveys(head,
#'                       "https://leidenuniv.eu.qualtrics.com/API/v3/responseexports/")
#'                       # URL is for my own institution.
#'                       # Substitute with your own institution's url
#' mysurvey <- getSurvey(surveys$id[6],
#'                       head,
#'                       "https://leidenuniv.eu.qualtrics.com/API/v3/responseexports/",
#'                       verbose=TRUE)
#' }
#'

registerApiKey <- function(API.TOKEN) {

  # Get temporary directory
  td <- tempdir()

  # Construct header to send to qualtrics API
  head <- qualtRics:::constructHeader(API.TOKEN)

  # Save to temporary directory
  saveRDS(head, paste0(td, "/qualtRics_header.rds"))

  # Return
  return(TRUE)

}
