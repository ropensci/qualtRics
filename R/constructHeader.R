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
