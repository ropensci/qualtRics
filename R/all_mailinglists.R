#' Retrieve a data frame of all mailing lists from Qualtrics
#'
#' @details
#' This function uses the Qualtrics XM Directory API. The directory it queries
#' is discovered automatically from your account. If your account has more than
#' one XM Directory, set the `QUALTRICS_DIRECTORY_ID` environment variable to
#' the directory ID you want to use; otherwise the first directory returned by
#' the API is used (with a warning).
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

  directory_id <- fetch_directory_id()
  fetch_url <- generate_url(query = "allmailinglists", directoryID = directory_id)
  elements <- paginate_api_request(fetch_url)
  x <- purrr::map_df(elements, purrr::flatten)
  return(x)

}
