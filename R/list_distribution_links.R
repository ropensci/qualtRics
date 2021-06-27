
#' Download distribution links for a distribution from Qualtrics
#'
#' @param distributionID String. Unique distribution ID for the distribution links you want to download.
#' @inheritParams fetch_survey
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
#' distribution_links <- list_distribution_links(
#'     distributions$id[1],
#'     surveyID = surveys$id[1]
#' )
#'}
#'

list_distribution_links <- function(distributionID, surveyID){

  # qualtrics distribution links parameters can be found at https://api.qualtrics.com/guides/reference/distributions.json/paths/~1distributions~1%7BdistributionId%7D~1links/get

  assert_base_url()
  assert_api_key()
  assert_parent_distribution(distributionID = distributionID,
                             surveyID = surveyID)

  fetch_url <- create_distribution_links_url(base_url = Sys.getenv("QUALTRICS_BASE_URL"),
                                             distributionId = distributionID,
                                             surveyId = surveyID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(contactId = purrr::map_chr(elements, "contactId", .default = NA_character_),
                      link = purrr::map_chr(elements, "link", .default = NA_character_),
                      exceededContactFrequency = purrr::map_chr(elements, "exceededContactFrequency", .default = NA_character_),
                      linkExpiration = purrr::map_chr(elements, "linkExpiration", .default = NA_character_),
                      lastName = purrr::map_chr(elements, "lastName", .default = NA_character_),
                      firstName = purrr::map_chr(elements, "firstName", .default = NA_character_),
                      externalDataReference = purrr::map_chr(elements, "externalDataReference", .default = NA_character_),
                      email = purrr::map_chr(elements, "email", .default = NA_character_),
                      unsubscribed = purrr::map_chr(elements, "unsubscribed", .default = NA_character_))

  return(x)

}
