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
#' Returned as `id` by the \link[qualtRics]{getSurveys} function.
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the
#' Qualtrics API.
#' @importFrom dplyr as_tibble
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
#' # Retrieve a single survey, filtering for specific questions
#' mysurvey <- getSurvey(
#'   surveyID = surveys$id[6],
#'   saveDir = tempdir(),
#'   includedQuestionIds = c("QID1", "QID2", "QID3"),
#'   verbose = TRUE
#' )
#' }
#' 
survey_questions <- function(surveyID) {

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
  # Add questions, question labels, question names and force response info
  quest <- data.frame(
    qid = names(qi),
    qnames = vapply(qi, function(x) x$questionName, ""),
    question = vapply(qi, function(x) x$questionText, ""),
    force_resp = vapply(
      qi,
      function(x) x$validation$doesForceResponse, # nolint
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  # Row names
  row.names(quest) <- seq_len(nrow(quest))

  # RETURN DATA ----

  # Return
  return(dplyr::as_tibble(quest))
}
