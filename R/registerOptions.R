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
#' This function registers the user's qualtrics API key, root url and other options for the remainder of the R session. This function only needs to be called once (at the beginning of each R session).
#'
#' @param api_token String. API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#' @param root_url String. Root url for your institution (see: \url{https://api.qualtrics.com/docs/root-url})
#'
#' @seealso See \url{https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file.
#'
#' @author Jasper Ginn
#' @importFrom yaml yaml.load_file
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
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
#'                       useLabels = TRUE,
#'                       seenUnansweredRecode = "UNANS",
#'                       verbose=TRUE)
#' }
#'

registerOptions <- function(api_token = NULL, root_url = NULL) {

  # If API token or root url are NULL, then look for .qualtRics in the current directory
  if(is.null(api_token) & is.null(root_url)) {
    ex <- file.exists(".qualtRics.yml")
    # Throw error if file does not exist
    assertthat::assert_that(ex == TRUE, msg = "No .qualtRics.yml configuration file found in working directory. Either set your api token and root url using the 'registerOptions' function or create a configuration file. Execute 'qualtRicsConfigFile()' to view an example of a configuration file.")
    # Load file
    cred <- yaml::yaml.load_file(".qualtRics.yml")
    # Assert that names are "api_token" and "root_url"
    assertthat::assert_that(length(names(cred)) == 2, msg="Either the 'api_token' or 'root_url' arguments are missing in your .qualtRics.yml configuration file. Execute 'qualtRicsConfigFile()' for an example of how this file should look")
    assertthat::assert_that(names(cred)[1] == "api_token", msg = "The first line of the .qualtRics.yml file must be named 'api_token'. Execute 'qualtRicsConfigFile()' to view an example of the configuration file.")
    assertthat::assert_that(names(cred)[2] == "root_url", msg = "The second line of the .qualtRics.yml file must be named 'root_url'. Execute 'qualtRicsConfigFile()' to view an example of the configuration file.")
    # Set vars
    api_token <- cred[[1]]
    root_url <- cred[[2]]
  }
  # If either is still NULL, throw error
  assertthat::assert_that(!is.null(root_url), msg="'root_url' parameter must either be specified in the .qualtRics.yml configuration file or passed to the 'registerOptions' function. To view an example of a configuration file, execute 'qualtRicsConfigFile()'.")
  assertthat::assert_that(!is.null(api_token), msg="'api_token' parameter must either be specified in the .qualtRics.yml configuration file or passed to the 'registerOptions' function. To view an example of a configuration file, execute 'qualtRicsConfigFile()'.")
  # Set environment variables
  Sys.setenv("QUALTRICS_API_KEY" = api_token, "QUALTRICS_ROOT_URL" = root_url)

}
