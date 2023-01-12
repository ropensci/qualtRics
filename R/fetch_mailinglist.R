
#' Download a mailing list from Qualtrics
#'
#' @param mailinglistID String. Unique ID for the mailing list you want to
#'   download. Returned as `id` by the [all_mailinglists][qualtRics::all_mailinglists]
#'   function.
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
#' # Retrieve a list of all mailing lists
#' mailinglists <- all_mailinglists()
#'
#' # Retrieve a single mailing list
#' mailinglist <- fetch_mailinglist(mailinglists$id[1])
#' }
#'

fetch_mailinglist <- function(mailinglistID){

  check_credentials()
  checkarg_isstring(mailinglistID)

  fetch_url <- generate_url(query = "fetchmailinglist",
                            mailinglistID = mailinglistID)

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }
  # Drop list-columns responseHistory & emailHistory
  elements <-
    purrr::map(elements, purrr::list_modify, responseHistory = rlang::zap(),
              emailHistory = rlang::zap())

  # Wrap embeddedData in single-element list for proper row-binding:
  elements <-
    purrr::map(elements, purrr::modify_in, "embeddedData", ~list(.x))

  # Row bind to create main df:
  out <-
    dplyr::bind_rows(elements)
  # Ensure unsubscribed is logical:
  out <-
    mutate(out, unsubscribed = as.logical(unsubscribed))
  # put embeddedData at end in preparation for expansion:
  out <-
    dplyr::relocate(out,
                    tidyselect::any_of("embeddedData"),
                    .after = tidyr::last_col())
  # wide-expand embeddedData:
  out <-
    tidyr::unnest_wider(out, embeddedData)

  return(out)

}
