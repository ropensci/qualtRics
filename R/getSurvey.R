#' Export a survey and download into R
#'
#' @param surveyID
#' @param headers 'headers' object - returned by the 'constructHeader' function. See \link[qualtRics]{constructHeader}.
#' @param base_url Base url for your institution (see \url{https://api.qualtrics.com/docs/csv}. If you do not fill in anything, the function will use the default url. Using your institution-specific url can significantly speed up queries.)
#' @param verbose Print verbose messages to the R console? Defaults to FALSE
#'
#' @seealso See \url{https://api.qualtrics.com/docs/csv} for documentation on the Qualtrics API.
#' @author Jasper Ginn
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom stringr str_sub

getSurvey <- function(surveyID, headers, base_url = "https://yourdatacenterid.qualtrics.com/API/v3/responseexports/", verbose = FALSE) {

  if(str_sub(base_url, nchar(base_url), nchar(base_url)) != "/") {
    base_url <- paste0(base_url, "/")
  }

  # Create raw JSON payload
  raw_payload <- paste0('{"format": "csv", "surveyId": ',
                        '"',
                        surveyID,
                        '",',
                        '"useLabels": true',
                        '}')
  # POST request for download
  res <- POST(base_url,
              add_headers(
                headers
              ),
              body = raw_payload
  )
  # Get content
  cnt <- content(res)
  # If http status == 200
  if(! cnt$meta$httpStatus == "200 - OK" ) {
    stop(paste0("Query returned status ",
                '"',
                cnt$meta$httpStatus,
                '"',
                " .Please check your code."))
  }
  # Get id
  ID = cnt$result$id
  # Monitor when export is ready
  progress <- 0
  check_url <- paste0(base_url, ID)
  while(progress < 100) {
    CU <- GET(check_url, add_headers(headers))
    progress <- content(CU)$result$percentComplete
    if( verbose ) {
      print(paste0("Download is ", progress, "% complete."))
    }
    Sys.sleep(5)
  }

  # Download file
  f <- GET(paste0(check_url, "/file"), add_headers(headers))
  ty <- content(f, "raw")
  # To zip file
  tf <- paste0(tempdir(), "/temp.zip")
  writeBin(ty, tf)
  # Take snapshot
  SS <- list.files(tempdir())
  u <- tryCatch({
    unzip(tf, exdir = tempdir())
  }, error = function(e) {
    stop("Error extracting csv from zip file.")
  })
  file <- setdiff(list.files(tempdir()), SS)
  data = read.csv(paste0(tempdir(), "/", file), header=TRUE, skip = 1, stringsAsFactors = FALSE)
  # Return
  return(data)
}
