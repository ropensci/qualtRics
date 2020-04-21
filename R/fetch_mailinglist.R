
#' Download a mailing list from Qualtrics
#'
#' @param mailinglistID String. Unique ID for the mailing list you want to download. Returned as \code{id} by the \link[qualtRics]{all_mailinglists} function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mailinglists <- all_mailinglists()
#' mailinglist <- fetch_mailinglist(mailinglists$id[[1]])
#' }

fetch_mailinglist <- function(mailinglistID){

  assert_base_url()
  assert_api_key()

  fetch_url <- create_mailinglist_url(Sys.getenv("QUALTRICS_BASE_URL"), mailinglistID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(id = purrr::map_chr(elements, "id", .default = NA_character_),
                      firstName = purrr::map_chr(elements, "firstName", .default = NA_character_),
                      lastName = purrr::map_chr(elements, "lastName", .default = NA_character_),
                      email = purrr::map_chr(elements, "email", .default = NA_character_),
                      externalDataReference = purrr::map_chr(elements, "externalDataReference", .default = NA_character_),
                      language = purrr::map_chr(elements, "language", .default = NA_character_),
                      unsubscribed = purrr::map_lgl(elements, "unsubscribed", .default = NA))

  x <- dplyr::bind_cols(x, purrr::map_df(elements, "embeddedData", .default = NA_character_))

  return(x)

}
