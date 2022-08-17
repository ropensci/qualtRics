
#' Retrieve a data frame of all mailing lists for a directory from Qualtrics
#'
#' @param directoryID String. Unique directory ID.
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
#'}
#'

list_mailing_lists <- function(directoryID){

  # qualtrics list mailing lists parameters can be found at
  # https://api.qualtrics.com/dd83f1535056c-list-mailing-lists

  check_credentials()
  checkarg_isstring(directoryID)

  fetch_url <- generate_url(query = "listmailinglists",
                            directoryID = directoryID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- tibble::tibble(mailingListId = purrr::map_chr(elements, "mailingListId", .default = NA_character_),
                      name = purrr::map_chr(elements, "name", .default = NA_character_),
                      ownerId = purrr::map_chr(elements, "ownerId", .default = NA_character_),
                      lastModifiedDate = purrr::map_chr(elements, "lastModifiedDate", .default = NA_character_),
                      creationDate = purrr::map_chr(elements, "creationDate", .default = NA_character_),
                      contactCount = purrr::map_chr(elements, "contactCount", .default = NA_character_))
  return(x)

}
