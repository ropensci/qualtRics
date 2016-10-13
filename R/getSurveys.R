#   Copyright 2016 Jasper Ginn
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at

#       http://www.apache.org/licenses/LICENSE-2.0

#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' Retrieve a list of all active surveys that you own on qualtrics
#'
#' @param headers 'headers' object - returned by the 'constructHeader' function. See \link[qualtRics]{constructHeader}.
#' @param survey_baseurl Base url for your institution (see \url{https://api.qualtrics.com/docs/root-url}. If you do not fill in anything, the function will use the default url. Using your institution-specific url can significantly speed up queries.)
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @export

getSurveys <- function(headers, survey_baseurl = "https://yourdatacenterid.qualtrics.com/API/v3/surveys") {
  # Send GET request to list all surveys
  res <- GET(survey_baseurl, add_headers(headers))
  # Return
  return(do.call(rbind.data.frame, content(res)$result$elements))
}
