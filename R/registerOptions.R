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

#' Register qualtrics API key, root url and other options
#'
#' This function registers the user's qualtrics API key, root url and other options for the remainder of the R session. This function only needs to be called once (at the beginning of each R session). You may also use a configuration file. See \code{\link{qualtRicsConfigFile}} or \url{https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file}
#' . Note that you must pass both an api token and a root url if you call this function for the first time in a session and you're not using a config file. Thereafter, you can pass these options individually.
#'
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertStandardColumns Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert general data columns (first name, last name, lat, lon, ip address, startdate, enddate etc.) to their proper format. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param dateWarning Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.
#' @param ... Either one or both of 'api_token' and 'root_url' to register the Qualtrics api key and institution-specific root url manually. (see example). See also \code{\link{qualtRicsConfigFile}} for an explanation of the root_url and api_token parameters.
#'
#' @seealso See \url{https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file. See: \url{https://api.qualtrics.com/docs/authentication} to find your Qualtrics API key and \url{https://api.qualtrics.com/docs/root-url} for more information about the institution-specific root url.
#'
#' @author Jasper Ginn
#' @importFrom yaml yaml.load_file
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' # Note that you need to pass both the 'api_token' and 'root_url'
#' # parameters if you call this function for the first time.
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
#' # Register a different root url
#' registerOptions(root_url="<YOUR-OTHER-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       verbose=TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       seenUnansweredRecode = "UNANS")
#' }
#'

registerOptions <- function(verbose=TRUE,
                            useLabels=TRUE,
                            convertStandardColumns=TRUE,
                            useLocalTime=FALSE,
                            dateWarning=TRUE,
                            ...) {
  # Take additional arguments
  args <- list(...)
  # Store root_url/api_token. Else give NA as value
  root_url <- ifelse("root_url" %in% names(args), args$root_url, NA)
  api_token <- ifelse("api_token" %in% names(args), args$api_token, NA)
  # If environment variables are already set and user just wants to change verbose argument
  if(Sys.getenv("QUALTRICS_ROOT_URL") != "" & Sys.getenv("QUALTRICS_API_KEY") != "") {
    # Check
    assertthat::assert_that(assertthat::is.flag(verbose), msg=paste0("'verbose' must be either TRUE or FALSE."))
    assertthat::assert_that(assertthat::is.flag(convertStandardColumns), msg=paste0("'convertstandardcolumns' must be either TRUE or FALSE."))
    assertthat::assert_that(assertthat::is.flag(useLabels), msg=paste0("'uselabels' must be either TRUE or FALSE."))
    assertthat::assert_that(assertthat::is.flag(useLabels), msg=paste0("'uselabels' must be either TRUE or FALSE."))
    assertthat::assert_that(assertthat::is.flag(dateWarning), msg=paste0("'dateWarning' must be either TRUE or FALSE."))
    # If user just wants to pass options, do:
    options(
      "QUALTRICS_VERBOSE" = verbose,
      "QUALTRICS_USELABELS" = useLabels,
      "QUALTRICS_CONVERTSTANDARDCOLUMNS" = convertStandardColumns,
      "QUALTRICS_USELOCALTIME" = useLocalTime
    )
    # Set warning
    invisible(ifelse(dateWarning == FALSE, Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE),
                     TRUE))
    # Set root url and api token if not NA
    if(!is.na(root_url)) Sys.setenv("QUALTRICS_ROOT_URL" = root_url)
    if(!is.na(api_token)) Sys.setenv("QUALTRICS_API_KEY" = api_token)
    # Quietly quit
    return(invisible(NULL))
  }
  # If API token and root url are NA, then look for .qualtRics.yml in the current directory
  if(is.na(api_token) & is.na(root_url)) {
    ex <- file.exists(".qualtRics.yml")
    # Throw error if file does not exist
    assertthat::assert_that(ex == TRUE, msg = "No .qualtRics.yml configuration file found in working directory. Either set your api token and root url using the 'registerOptions' function or create a configuration file. Execute 'qualtRicsConfigFile()' to view an example of a configuration file.")
    # Load file
    cred <- yaml::yaml.load_file(".qualtRics.yml")
    # Assert that names are "api_token" and "root_url"
    assertthat::assert_that(all(c("api_token", "root_url") %in% names(cred)), msg="Either the 'api_token' or 'root_url' arguments are missing in your .qualtRics.yml configuration file. Execute 'qualtRicsConfigFile()' to view an example of the configuration file.")
    # If verbose, print message
    if(verbose) message(paste0("Found a .qualtRics.yml configuration file in ", getwd(), ". Using these credentials."))
    # Set vars
    api_token <- cred$api_token
    root_url <- cred$root_url
    # Set optional vars
    if("verbose" %in% names(cred)) {
      verbose <- cred$verbose
      assertthat::assert_that(assertthat::is.flag(verbose), msg=paste0("'verbose' must be either TRUE or FALSE but is ", as.character(verbose), " in your config file."))
    }
    if("convertstandardcolumns" %in% names(cred)) {
      convertStandardColumns <- cred$convertstandardcolumns
      assertthat::assert_that(assertthat::is.flag(convertStandardColumns), msg=paste0("'convertstandardcolumns' must be either TRUE or FALSE but is ", as.character(convertStandardColumns), " in your config file."))
    }
    if("uselabels" %in% names(cred)) {
      useLabels <- cred$uselabels
      assertthat::assert_that(assertthat::is.flag(useLabels), msg=paste0("'uselabels' must be either TRUE or FALSE but is ", as.character(useLabels), " in your config file."))
    }
    if("uselocaltime" %in% names(cred)) {
      useLocalTime <- cred$uselocaltime
      assertthat::assert_that(assertthat::is.flag(useLocalTime), msg=paste0("'useLocalTime' must be either TRUE or FALSE but is ", as.character(useLocalTime), " in your config file."))
    }
    if("datewarning" %in% names(cred)) {
      dateWarning <- cred$datewarning
      assertthat::assert_that(assertthat::is.flag(dateWarning), msg=paste0("'dateWarning' must be either TRUE or FALSE but is ", as.character(dateWarning), " in your config file."))
    }
  }
  # Check arguments
  # If either is still NA AND environment is empty, throw error
  assertthat::assert_that(!is.na(root_url) | Sys.getenv("QUALTRICS_ROOT_URL") != "", msg="'root_url' parameter must either be specified in the .qualtRics.yml configuration file or passed to the 'registerOptions' function. To view an example of a configuration file, execute 'qualtRicsConfigFile()'.")
  assertthat::assert_that(!is.na(api_token) | Sys.getenv("QUALTRICS_API_KEY") != "", msg="'api_token' parameter must either be specified in the .qualtRics.yml configuration file or passed to the 'registerOptions' function. To view an example of a configuration file, execute 'qualtRicsConfigFile()'.")
  # Set environment variables
  if(!is.na(api_token)) Sys.setenv("QUALTRICS_API_KEY" = api_token)
  if(!is.na(root_url)) Sys.setenv("QUALTRICS_ROOT_URL" = root_url)
  # Set flag options
  options(
    "QUALTRICS_VERBOSE" = verbose,
    "QUALTRICS_USELABELS" = useLabels,
    "QUALTRICS_CONVERTSTANDARDCOLUMNS" = convertStandardColumns,
    "QUALTRICS_USELOCALTIME" = useLocalTime
  )
  # Set warning
  invisible(ifelse(dateWarning == FALSE, Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE),
         TRUE))
}
