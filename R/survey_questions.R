#' Retrieve a data frame containing question IDs and labels
#'
#' This function is soft deprecated; use \code{\link[qualtRics]{survey_questions}}
#' instead.
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as `id` by the \link[qualtRics]{getSurveys} function.
#'
#' @export
getSurveyQuestions <- function(surveyID) {
  warning("Soon, `getSurveyQuestions` will be deprecated. Try using `survey_questions()` instead.")
  survey_questions(surveyID)
}


#' Retrieve a data frame containing question IDs and labels
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as `id` by the \link[qualtRics]{all_surveys} function.
#' @param column_map Logical. \code{TRUE} to export the current (active) Q to
#' QID mapping. Defaults to \code{FALSE} to export survey questions.
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the
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
#' # Retrieve questions for a survey
#' questions <- survey_questions(surveyID = surveys$id[6])
#'
#' # Retrieve column mapping for a survey
#' questions <- survey_questions(surveyID = surveys$id[6], column_map = TRUE)
#'
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
survey_questions <- function(surveyID, column_map = FALSE) {

  # OPTIONS AND BUILD QUERY ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Function-specific API stuff
  root_url <- append_root_url(Sys.getenv("QUALTRICS_BASE_URL"), "surveys")
  # Add survey id
  root_url <- paste0(
    root_url,
    "/",
    surveyID
  )

  # SEND REQUEST TO API ----

  # GET request to download metadata
  resp <- qualtrics_api_request("GET", root_url)
  # Get question information and map
  qi <- resp$result$questions
  c_map <- resp$result$exportColumnMap
  # Add questions, question labels, question names and force response info
  quest <- tibble::tibble(
    qid = names(qi),
    qnames = purrr::map_chr(qi, "questionName"),
    question = purrr::map_chr(qi, "questionText"),
    force_resp = purrr::map_lgl(qi, ~ .$validation$doesForceResponse))

  mapping <- tibble::tibble(
    qid = names(c_map),
    question = purrr::map_chr(c_map, "question"),
    choice = purrr::map_chr(c_map, "choice", .null = NA_character_),
    textEntry = purrr::map_chr(c_map, "textEntry", .null = NA_character_)
  )

  # RETURN DATA ----

  # Return

  if (column_map) {
    return(mapping)
  } else {
    return(quest)
  }

}
