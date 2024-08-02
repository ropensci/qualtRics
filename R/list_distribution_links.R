#' Download distribution links for a distribution from Qualtrics
#'
#' @param distributionID String. Unique distribution ID for the distribution links you want to download.
#' @param surveyID String. Unique ID for the survey you want to download.
#'
#' @template retry-advice
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
#' surveys <- all_surveys()
#' distributions <- fetch_distributions(surveys$id[1])
#' distribution_links <- list_distribution_links(distributions$id[1], surveyID = surveys$id[1])
#'}
#'

list_distribution_links <- function(distributionID, surveyID){

  # qualtrics distribution links parameters can be found at https://api.qualtrics.com/guides/reference/distributions.json/paths/~1distributions~1%7BdistributionId%7D~1links/get

  check_credentials()
  checkarg_isstring(distributionID)
  checkarg_isstring(surveyID)

  fetch_url <- generate_url(query = "listdistributionlinks",
                            distributionID = distributionID,
                            surveyID = surveyID)

  elements <- paginate_api_request(fetch_url)

  x <- tibble::tibble(contactId = purrr::map_chr(elements, "contactId", .default = NA_character_),
                      transactionId = purrr::map_chr(elements, "transactionId", .default = NA_character_),
                      link = purrr::map_chr(elements, "link", .default = NA_character_),
                      exceededContactFrequency = purrr::map_lgl(elements, "exceededContactFrequency", .default = NA),
                      linkExpiration = lubridate::ymd_hms(purrr::map_chr(elements, "linkExpiration", .default = NA_character_)),
                      status = purrr::map_chr(elements, "status", .default = NA_character_),
                      lastName = purrr::map_chr(elements, "lastName", .default = NA_character_),
                      firstName = purrr::map_chr(elements, "firstName", .default = NA_character_),
                      externalDataReference = purrr::map_chr(elements, "externalDataReference", .default = NA_character_),
                      email = purrr::map_chr(elements, "email", .default = NA_character_),
                      unsubscribed = purrr::map_lgl(elements, "unsubscribed", .default = NA))

  return(x)

}
