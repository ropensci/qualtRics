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

#' Prints an Example of a QualtRics Configuration File to the Console.
#'
#' @param api_token String. API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#' @param base_url String. Base url for your institution (see: \url{https://api.qualtrics.com/docs/root-url})
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertVariables Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param dateWarning Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.
#' @param root_url String. Deprecated. Use base url instead. This will be removed in future versions.
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API. See \url{https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file.
#' @author Jasper Ginn
#' @export
#' @examples
#' \dontrun{
#' # Execute this line to get instructions on how to make a .qualtrics.yml config file.
#' qualtRicsConfigFile()
#' }
#'

qualtRicsConfigFile <- function(api_token = NULL,
                                base_url=NULL,
                                verbose=TRUE,
                                useLabels=TRUE,
                                convertVariables=TRUE,
                                useLocalTime=FALSE,
                                dateWarning=TRUE,
                                root_url = NULL) {

  # Check for deprecated arguments
  calls <- names(vapply(match.call(), deparse, "character"))[-1]
  # Check if deprecated params passed
  if(any("root_url" %in% calls)) {
    warning("'root_url' is deprecated and will be removed in qualtRics 4.0.\n Please use 'base_url' instead.")
    base_url <- root_url
  }
  # Temporary
  root_url <- base_url

  # Paste together a message to cat to console
  msg <- paste0(
    "Copy-paste the lines between the dashes into a new plain text file, replace the
values for the api_token and root_url if they are not yet filled out. and save it in
your working directory as '.qualtRics.yml'. Execute '?qualtRics::qualtRicsConfigFile'
to view an explanation of the additional arguments. Visit
https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file
for more information.", "\n\n",# nolint
    "--------------","\n",
    'api_token: ', ifelse(is.null(api_token), '<YOUR-API-TOKEN-HERE>',
                                 paste0(api_token)), "\n",
    'base_url: ', ifelse(is.null(root_url), '<YOUR-ROOT-URL-HERE>',
                                paste0(root_url)), "\n",
    'verbose: ', verbose, "\n",
    'uselabels: ', useLabels, "\n",
    'convertvariables: ', convertVariables, "\n",
    'uselocaltime: ', useLocalTime, "\n",
    'datewarning: ', dateWarning, "\n",
    "--------------"
  )

  # Cat to console
  cat(msg)

}
