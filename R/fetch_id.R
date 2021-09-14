#' Fetch a unique Qualtrics survey ID based on survey name in the Qualtrics UI
#'
#' @param .data Data frame of active surveys created by the function
#' [all_surveys()].
#' @param survey_name Name of the survey as it appears in the Qualtrics UI. Must
#' be unique to be passed to `fetch_id()`.
#'
#' @details Survey names in the Qualtrics platform are not required to be
#' unique, but the `survey_name` argument for this function _must_ be unique.
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

fetch_id <- function(.data, survey_name) {

  # check that a dataframe from all_surveys() is supplied to x
  assertthat::assert_that(
    "id" %in% colnames(.data) & "name" %in% colnames(.data),
    msg = paste0("Error: Please pass a dataframe created by all_surveys() with columns 'id' and 'name'",
                 "\nExample: all_surveys() %>% fetch_id(survey_name)")
  )

  # pull survey id
  survey_id <- dplyr::filter(.data, name == survey_name)
  survey_id <- dplyr::pull(survey_id, id)

  # check that at least one survey_name is found
  assertthat::assert_that(
    length(survey_id) > 0,
    msg = "Error: No survey IDs returned.\nPlease verify that `survey_name` is correct and try again."
  )

  # check that survey_name is unique and returns one surveyID
  assertthat::assert_that(
    length(survey_id) < 2,
    msg = "Error: Multiple survey IDs returned. Please supply a unique `survey_name`."
  )

  survey_id
}
