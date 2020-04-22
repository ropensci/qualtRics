
#' Retrieve a data frame of all mailing lists from Qualtrics
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
#' # Retrieve a list of all mailing lists
#' mailinglists <- all_mailinglists()
#' }
#'

all_mailinglists <- function(){

  assert_base_url()
  assert_api_key()

  fetch_url <- create_mailinglists_url(Sys.getenv("QUALTRICS_BASE_URL"))

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  x <- purrr::map_df(elements, purrr::flatten)

  return(x)

}
