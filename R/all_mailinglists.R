#' Retrieve a data frame of all mailing lists from Qualtrics
#'
#' @template retry-advice
#' @importFrom purrr map_df
#' @importFrom purrr flatten
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

  check_credentials()

  # Function-specific API stuff
  fetch_url <- generate_url(query = "allmailinglists")
  elements <- paginate_api_request(fetch_url)
  x <- purrr::map_df(elements, purrr::flatten)
  return(x)

}
