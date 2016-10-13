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
  return(content(res)$result$elements)
}
