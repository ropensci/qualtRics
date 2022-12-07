#' Retrieve a data frame of all directories from Qualtrics
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
#' # Retrieve a list of all directories
#' directories <- list_directories()
#' }
#'

list_directories <- function(){

  check_credentials()

  # Function-specific API stuff
  fetch_url <- generate_url(query = "listdirectories")

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- purrr::map_df(elements, purrr::flatten)

  return(x)

}
