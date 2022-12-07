
#' Retrieve a data frame of all contacts for a mailing list from Qualtrics
#'
#' @param directoryID String. Unique directory ID.
#' @param mailinglistID String. Unique ID of mailing list you want contacts from.
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
#' directories <- list_directories()
#' mailing_lists <- list_mailing_lists(directories$directoryId[1])
#' contacts <- mailing_list_contacts(directories$directoryId[1], mailing_lists$mailingListId[1])
#'}
#'

mailing_list_contacts <- function(directoryID, mailinglistID){

  # qualtrics mailing lists contacts parameters can be found at
  # https://api.qualtrics.com/af95194dd116e-list-contacts-in-mailing-list

  check_credentials()
  checkarg_isstring(directoryID)
  checkarg_isstring(mailinglistID)

  fetch_url <- generate_url(query = "mailinglistcontacts",
                            directoryID = directoryID,
                            mailinglistID = mailinglistID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url, query = list(includeEmbedded = "true"))
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(contactId = purrr::map_chr(elements, "contactId", .default = NA_character_),
                      firstName = purrr::map_chr(elements, "firstName", .default = NA_character_),
                      lastName = purrr::map_chr(elements, "lastName", .default = NA_character_),
                      email = purrr::map_chr(elements, "email", .default = NA_character_),
                      phone = purrr::map_chr(elements, "phone", .default = NA_character_),
                      extRef = purrr::map_chr(elements, "extRef", .default = NA_character_),
                      language = purrr::map_chr(elements, "language", .default = NA_character_),
                      unsubscribed = purrr::map_lgl(elements, "unsubscribed", .default = NA_character_))

  y = purrr::map_df(elements, "embeddedData", .default = NA_character_)

  x <- cbind(x, y)

  return(x)

}
