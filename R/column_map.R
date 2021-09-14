#' Retrieve a data frame containing survey column mapping
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as `id` by the \link[qualtRics]{all_surveys} function.
#'
#' @seealso See \url{https://api.qualtrics.com/} for documentation on the
#' Qualtrics API.
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#'
#' # Retrieve a list of surveys
#' surveys <- all_surveys()
#'
#' # Retrieve column mapping for a survey
#' mapping <- column_map(surveyID = surveys$id[6])
#'
#' # Retrieve a single survey, filtering for specific questions
#' mysurvey <- fetch_survey(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   include_questions = c("QID1", "QID2", "QID3"),
#'   verbose = TRUE
#' )
#' }
#'
column_map <- function(surveyID) {

  # OPTIONS AND BUILD QUERY ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Function-specific API stuff
  survey_url <- generate_url(query = "metadata",
                             surveyID = surveyID)

  # SEND REQUEST TO API ----
  # Send GET request to specific survey
  resp <- qualtrics_api_request("GET", survey_url)

  # Get question information and map
  c_map <- resp$result$exportColumnMap

  # Column mapping
  mapping <- tibble::tibble(
    qname = names(c_map),
    qid = purrr::map_chr(c_map, "question"),
    choice = purrr::map_chr(c_map, "choice", .null = NA_character_),
    textEntry = purrr::map_chr(c_map, "textEntry", .null = NA_character_)
  )

  return(mapping)

}
