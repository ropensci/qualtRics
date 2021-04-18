
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

fetch_distribution_history <- function(distributionID, surveyID){

  # qualtrics distribution history parameters can be found at https://api.qualtrics.com/guides/reference/distributions.json/paths/~1distributions~1%7BdistributionId%7D~1history/get

  assert_base_url()
  assert_api_key()
  assert_parent_distribution(distributionID = distributionID,
                             surveyID = surveyID)

  fetch_url <- create_distribution_history_url(base_url = Sys.getenv("QUALTRICS_BASE_URL"),
                                               distributionId = distributionID)

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

  links <- list_distribution_links(base_url = Sys.getenv("QUALTRICS_BASE_URL"),
                                   distributionId = distributionID,
                                   surveyId = surveyID)

  links <- dplyr::select(links, contactId, email, linkExpiration, lastName,
                         firstName, externalDataReference, unsubscribed)

  x <- dplyr::left_join(x, links, by = "contactId")

  return(x)

}

