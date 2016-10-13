#' Retrieve a list of all active surveys that you own on qualtrics
#'
#' @param
#'
#' @seealso See \url{https://api.qualtrics.com/docs/list-surveys} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET

getSurveyIDs <- function(survey_baseurl = "https://yourdatacenterid.qualtrics.com/API/v3/surveys", headers) {
  # Send GET request to list all surveys
  res <- GET(survey_baseurl, add_headers(headers))
  # Return
  return(content(res)$result$elements)
}
