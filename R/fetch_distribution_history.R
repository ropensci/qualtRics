#' Download distribution history data for a distribution from Qualtrics
#'
#' @param distributionID String. Unique distribution ID for the distribution history you want to download.
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
#' distribution_history <- fetch_distribution_history(distributions$id[1])
#'}
#'

fetch_distribution_history <- function(distributionID){

  # qualtrics distribution parameters can be found at
  # https://api.qualtrics.com/docs/getting-information-about-distributions-1#get-distribution

  check_credentials()
  checkarg_isstring(distributionID)

  fetch_url <- generate_url(query = "fetchdistributionhistory",
                            distributionID = distributionID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(contactId = purrr::map_chr(elements, "contactId", .default = NA_character_),
                      contactLookupId = purrr::map_chr(elements, "contactLookupId", .default = NA_character_),
                      distributionId = purrr::map_chr(elements, "distributionID", .default = NA_character_),
                      status = purrr::map_chr(elements, "status", .default = NA_character_),
                      surveyLink = purrr::map_chr(elements, "surveyLink", .default = NA_character_),
                      contactFrequencyRuleId = purrr::map_chr(elements, "contactFrequencyRuleId", .default = NA_character_),
                      responseId = purrr::map_chr(elements, "responseId", .default = NA_character_),
                      responseCompletedAt = lubridate::ymd_hms(purrr::map_chr(elements, "responseCompletedAt", .default = NA_character_)),
                      sentAt = lubridate::ymd_hms(purrr::map_chr(elements, "sentAt", .default = NA_character_)),
                      openedAt = lubridate::ymd_hms(purrr::map_chr(elements, "openedAt", .default = NA_character_)),
                      responseStartedAt = lubridate::ymd_hms(purrr::map_chr(elements, "responseStartedAt", .default = NA_character_)),
                      surveySessionId = purrr::map_chr(elements, "surveySessionId", .default = NA_character_))

  return(x)

}
