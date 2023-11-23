
#' Download a mailing list from Qualtrics
#'
#' @param mailinglistID String. Unique ID for the mailing list you want to
#'   download. Returned as `id` by the [all_mailinglists][qualtRics::all_mailinglists]
#'   function.
#'
#' @template retry-advice
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr last_col
#' @importFrom dplyr any_of
#' @importFrom dplyr across
#' @importFrom purrr list_modify
#' @importFrom purrr map
#' @importFrom purrr modify_in
#' @importFrom purrr zap
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

  fetch_url <-
    generate_url(
      query = "fetchmailinglist",
      mailinglistID = mailinglistID
    )

  elements <- list()

  while(!is.null(fetch_url)){

    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    fetch_url <- res$result$nextPage

  }

  # Drop list-columns responseHistory & emailHistory (diff function for these)
  elements <-
    purrr::map(
      elements,
      purrr::list_assign,
      responseHistory = purrr::zap(),
      emailHistory = purrr::zap()
    )

  # Wrap embeddedData in single-element list for proper row-binding:
  elements <-
    purrr::map(
      elements,
      purrr::modify_in,
      "embeddedData",
      ~list(.x)
    )

  # Convert any remaining NULLS to (logical) NA:
  elements <-
    purrr::map_depth(
      elements,
      2,
      ~.x %||% NA
    )

  # Row bind to create main df:
  out <-
    dplyr::bind_rows(elements)

  # Ensure unsubscribed is logical and others are character:
  out <-
    dplyr::mutate(
      out,
      dplyr::across(c(-unsubscribed, -embeddedData), as.character),
      unsubscribed = as.logical(unsubscribed)
    )

  # put embeddedData at end in preparation for expansion:
  out <-
    dplyr::relocate(
      out,
      dplyr::any_of("embeddedData"),
      .after = dplyr::last_col()
    )
  # wide-expand embeddedData:
  out <-
    tidyr::unnest_wider(
      out,
      embeddedData
    )

  return(out)
}
