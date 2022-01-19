#' Extract column map from survey data download
#'
#' Helper function to extract the column map attached to a response data
#' download obtained from [qualtRics::fetch_survey()] (using the
#' default `add_column_map = TRUE`)
#'
#' @template retry-advice
#' @param respdata Response data including a column map dataframe as an attribute
#'
#' @importFrom purrr imap_dfr
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve a list of surveys
#' surveys <- all_surveys()
#'
#' # Retrieve a single survey
#' mysurvey <- fetch_survey(surveyID = surveys$id[6])
#'
#' # Extract column mapping for survey
#' extract_colmap(mysurvey)
#' }
#'

extract_colmap <- function(respdata) {

  return(attr(respdata, "column_map"))

}

