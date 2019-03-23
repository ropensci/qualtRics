#' Retrieve a data frame of all active surveys on Qualtrics
#'
#' This function is soft deprecated; use \code{\link[qualtRics]{all_surveys}}
#' instead.
#' @export
getSurveys <- function() {
  warning("Soon, `getSurveys` will be deprecated. Try using `all_surveys()` instead.")
  all_surveys()
}

#' Retrieve a data frame of all active surveys on Qualtrics
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the
#' Qualtrics API.
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#' 
#' # Retrieve a list of all surveys
#' surveys <- all_surveys()
#' 
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6])
#' 
#' # You can use the same parameters as those found in the Qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   startDate = "2018-01-01",
#'   endDate = "2018-01-31",
#'   limit = 100,
#'   useLabels = TRUE,
#'   seenUnansweredRecode = "UNANS",
#'   verbose = TRUE
#' )
#' }
#' 
all_surveys <- function() {

  # CHECK PARAMS AND PREP QUERY ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Function-specific API stuff
  root_url <- append_root_url(Sys.getenv("QUALTRICS_BASE_URL"), "surveys")

  # SEND REQUEST TO QUALTRICS ----

  # Send GET request to list all surveys
  resp <- qualtrics_api_request("GET", root_url)
  # Put results in list
  master <- list()
  # Append results
  master <- append(master, resp$result$elements)
  # If nextPage != null, keep calling
  while (!is.null(resp$result$nextPage)) {
    # Send GET request to list all surveys
    resp <- qualtrics_api_request("GET", resp$result$nextPage)
    # Append results
    master <- append(master, resp$result$elements)
  }

  # WRAP-UP AND RETURN ----

  # Bind to one large data frame & return
  d <- bind_rows(master)
  return(d)
}
