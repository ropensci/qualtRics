
#' Download distribution history data for a distribution from Qualtrics
#'
#' @param distributionID String. Unique distribution ID for the distribution history you want to download.
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
#' surveys <- all_surveys()
#' distributions <- fetch_distributions(surveys$id[1])
#' distribution_history <- fetch_distribution_history(distributions$id[1])
#'}
#'

fetch_distribution_history <- function(distributionID){

  # qualtrics distribution history parameters can be found at https://api.qualtrics.com/guides/reference/distributions.json/paths/~1distributions~1%7BdistributionId%7D~1history/get

  assert_base_url()
  assert_api_key()

  fetch_url <- create_distribution_history_url(base_url = Sys.getenv("QUALTRICS_BASE_URL"),
                                               distributionID = distributionID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(contactId = purrr::map_chr(elements, "contactId", .default = NA_character_),
                      contactLookupId = purrr::map_chr(elements, "contactLookupId", .default = NA_character_),
                      distributionID = purrr::map_chr(elements, "distributionID", .default = NA_character_),
                      status = purrr::map_chr(elements, "status", .default = NA_character_),
                      surveyLink = purrr::map_chr(elements, "surveyLink", .default = NA_character_),
                      contactFrequencyRuleId = purrr::map_chr(elements, "contactFrequencyRuleId", .default = NA_character_),
                      responseId = purrr::map_chr(elements, "responseId", .default = NA_character_),
                      responseCompletedAt = purrr::map_chr(elements, "responseCompletedAt", .default = NA_character_),
                      sentAt = purrr::map_chr(elements, "sentAt", .default = NA_character_),
                      openedAt = purrr::map_chr(elements, "openedAt", .default = NA_character_),
                      responseStartedAt = purrr::map_chr(elements, "responseStartedAt", .default = NA_character_),
                      surveySessionId = purrr::map_chr(elements, "surveySessionId", .default = NA_character_))

  return(x)

}

