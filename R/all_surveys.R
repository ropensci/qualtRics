#' Retrieve a data frame of all active surveys on Qualtrics
#'
#' @template retry-advice
#' @seealso See <https://api.qualtrics.com/> for documentation on the
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
#' mysurvey <- fetch_survey(surveyID = surveys$id[6])
#'
#' mysurvey <- fetch_survey(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   start_date = "2018-01-01",
#'   end_date = "2018-01-31",
#'   limit = 100,
#'   label = TRUE,
#'   unanswer_recode = "UNANS",
#'   verbose = TRUE
#' )
#' }
#'
all_surveys <- function() {

  # CHECK PARAMS AND PREP QUERY ----

  # Check params
  check_credentials()

  # Function-specific API stuff
  surveys_url <-
    generate_url(query = "allsurveys")

  # SEND REQUEST TO QUALTRICS ----

  # Send GET request to list all surveys
  resp <-
    qualtrics_api_request("GET", surveys_url)
  # Put results in list
  master <- list()
  # Append results
  master <-
    append(master, resp$result$elements)
  # If nextPage != null and not "string" placeholder, keep calling
  while (!is.null(resp$result$nextPage) && resp$result$nextPage != "string") {
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


