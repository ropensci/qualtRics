#' Test function: a way of extracting column mappings from existing dataframes
#'
#' could be used as both helper function and as a replacement for column_map, once
#' updated to also generate column mappings from existing response download data frames,
#' or from saved response downloads, etc.
#'
#' @param r_dat response data obtained from fetch_survey w/\code{colmap_attrs = TRUE}
#'
#' @importFrom purrr imap_dfr

extract_colmap <- function(r_dat) {

  return(attr(r_dat, "column_map"))

}

