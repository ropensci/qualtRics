#' Fetch a Qualtrics surveyID based on the survey name in the Qualtrics UI
#'
#' @param x Dataframe created by the function all_surveys().
#' @param survey_name Name of the survey as it appears in the Qualtrics UI. Must
#' be unique to be passed to fetch_id().
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

fetch_id <- function(x, survey_name) {

  # check that a dataframe from all_surveys() is supplied to x
  assertthat::assert_that(
    "id" %in% colnames(x) & "name" %in% colnames(x),
    msg = "Error: x incompatible. Please pass a dataframe created by all_surveys()\nExample: all_surveys() %>% fetch_id(survey_name)"
  )

  # pull survey id
  survey_id <-
    x %>%
    dplyr::filter(name == survey_name) %>%
    dplyr::pull(id)

  # check that at least one survey_name is found
  assertthat::assert_that(
    length(survey_id) > 0,
    msg = "Error: No surveyIDs returned. Please verify that the survey_name \nis correct and try again."
  )

  # check that survey_name is unique and returns one surveyID
  assertthat::assert_that(
    length(survey_id) < 2,
    msg = "Error: Multiple surveyIDs returned. Please supply a unique survey_name."
  )



}
