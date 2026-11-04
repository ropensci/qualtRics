#' Retrieve the versions of a survey from Qualtrics
#'
#' Qualtrics keeps a version history for each survey: a new version is saved
#' each time the survey is published, and can also be saved by hand. Each
#' version is a snapshot of the survey definition. `fetch_survey_versions()`
#' lists these versions, and the definition of any one of them can then be
#' downloaded with [fetch_description()] via its `version` argument.
#'
#' The versions endpoint is paged the same way as other list endpoints of the
#' Qualtrics API: if the response includes a `nextPage` URL, subsequent pages
#' are requested and combined.
#'
#' @param surveyID A string. Unique ID for the survey whose versions you want
#'   to retrieve. Returned as "id" by the [all_surveys] function.
#'
#' @template retry-advice
#' @return A tibble with one row per survey version, in the order returned by
#'   the API (most recent `versionNumber` first), with columns:
#'   * `surveyID`: the survey ID.
#'   * `versionID`: the version ID (a string of digits), as passed to the
#'     `version` argument of [fetch_description()].
#'   * `versionNumber`: the sequential version number (integer).
#'   * `description`: the description entered when the version was saved.
#'   * `userID`: the ID of the user who saved the version.
#'   * `creationDate`: when the version was saved (POSIXct, UTC).
#'   * `published`: whether this is the currently published version; at most
#'     one version has `published == TRUE`.
#'   * `wasPublished`: whether this version has ever been published.
#'   * `publishEvents`: a list column; each element is a list of the publish
#'     events (timestamp and user ID) for that version, or `NULL` if it has
#'     none.
#' @importFrom purrr map map_chr map_int map_lgl
#' @importFrom tibble tibble
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
#' # List the versions of a survey
#' versions <- fetch_survey_versions(surveyID = surveys$id[6])
#'
#' # Get the definition of the currently published version
#' descrip <- fetch_description(surveyID = surveys$id[6], version = "published")
#'
#' # Get the definition of a specific version
#' descrip_old <- fetch_description(
#'   surveyID = surveys$id[6],
#'   version = versions$versionID[2]
#' )
#' }
fetch_survey_versions <- function(surveyID) {

  check_credentials()
  checkarg_isstring(surveyID, null_okay = FALSE)

  fetch_url <- generate_url(query = "fetchsurveyversions",
                            surveyID = surveyID)

  elements <- paginate_api_request(fetch_url)

  # Each element holds its fields under "metadata":
  versions <- purrr::map(elements, "metadata")

  x <- tibble::tibble(surveyID = purrr::map_chr(versions, "surveyID", .default = NA_character_),
                      versionID = purrr::map_chr(versions, "versionID", .default = NA_character_),
                      versionNumber = purrr::map_int(versions, "versionNumber", .default = NA_integer_),
                      description = purrr::map_chr(versions, "description", .default = NA_character_),
                      userID = purrr::map_chr(versions, "userID", .default = NA_character_),
                      creationDate = lubridate::ymd_hms(purrr::map_chr(versions, "creationDate", .default = NA_character_)),
                      published = purrr::map_lgl(versions, "published", .default = NA),
                      wasPublished = purrr::map_lgl(versions, "wasPublished", .default = NA),
                      publishEvents = purrr::map(versions, "publishEvents"))

  return(x)

}
