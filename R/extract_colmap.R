#' Helper function to extract the column map attached to a response data
#' download obtained from fetch_survey (using the default
#' \code{colmap_attrs = TRUE})
#'
#' @param respdata Response data including a column map dataframe as an attribute
#'
#' @importFrom purrr imap_dfr
#'
#' @export

extract_colmap <- function(respdata) {

  return(attr(respdata, "column_map"))

}

