# Assert that the following works

assert_apikey_stored <- function(dir) {
  # Key should be stored by "registerApiKey"
  if(!assertthat::assert_that(assertthat::is.logical("qualtRics_header.rds" %in% list.files(dir)))) {
    stop("You need to register your qualtrics API key first using the 'registerApiKey()' function.")
  } else {
    return(TRUE)
  }
}
