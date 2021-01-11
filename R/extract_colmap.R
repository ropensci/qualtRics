#' Extract column map from survey data download
#'
#' Helper function to extract the column map attached to a response data
#' download obtained from \code{\link[qualtRics]{fetch_survey}} (using the
#' default \code{add_column_map = TRUE})
#'
#' @param respdata Response data including a column map dataframe as an attribute
#'
#' @importFrom purrr imap_dfr
#'
#' @export

extract_colmap <- function(respdata) {

  return(attr(respdata, "column_map"))

}

