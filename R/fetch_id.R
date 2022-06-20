#' Fetch a unique Qualtrics survey ID based on survey name in the Qualtrics UI
#'
#' @param .data Data frame of active surveys created by the function
#'   [all_surveys()].
#' @param survey_name String. Name of the survey as it appears in the Qualtrics
#'   UI. Must be unique to be passed to `fetch_id()`.
#' @param partial_match Boolean. Will match all surveys containing the exact
#'   string provided.  Defaults to FALSE, which matches against the entire name.
#'
#' @details Survey names in the Qualtrics platform are not required to be
#'   unique, but the `survey_name` argument for this function _must_ be unique.
#'   If input results in multiple surveys being matched, will error with a list
#'   of up to 5 matches & their IDs
#'
#' @export
#'
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
#' # Retrieve surveyID for a unique survey
#' my_id <- fetch_id(surveys, "Unique Survey Name")
#'
#' }

fetch_id <- function(.data, survey_name, partial_match = FALSE) {

  # check that a dataframe from all_surveys() is supplied to x
  checkarg_fetch_id_data(.data)
  checkarg_isstring(survey_name)
  checkarg_isboolean(partial_match)

  # pull survey id
  if(partial_match){
    surveys_matched <-
      dplyr::filter(.data,
                    stringr::str_detect(name, stringr::fixed(survey_name)))
  } else {
    surveys_matched <-
      dplyr::filter(.data,
                    name == survey_name)
  }

  # Throw error if no matches:
  if(nrow(surveys_matched) == 0){
    rlang::abort(
      c("No survey names matched",
        "Please verify that `survey_name` is correct and try again.")
    )
  }

  # Informative error if multiple matches:
  if(nrow(surveys_matched) > 1){
    matched_surveys <-
      paste0(surveys_matched$id, " - ", surveys_matched$name)

    if(length(matched_surveys > 5)){
      matched_surveys <- matched_surveys[1:5]
    }

    rlang::abort(
      c("Multiple survey IDs matched to 'survey_name'.",
        "Matches returned (up to 5):",
        matched_surveys)
    )
  }

  survey_id <-
    dplyr::pull(surveys_matched, id, name)

  # Give the name matched as a message if partial match is used:
  if(partial_match){
    rlang::inform(
      paste0("Survey matched: ", names(survey_id))
    )
  }

  return(survey_id)
}
