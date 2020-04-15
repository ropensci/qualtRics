
#' Retrieve a data frame of all mailing lists from Qualtrics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mailinglists <- all_mailinglists()
#' }

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
