# Assert that the following works

# Test whether API key is stored
assert_apikey_stored <- function(dir) {
  # Key should be stored by "registerApiKey"
  if(!assertthat::assert_that("qualtRics_header.rds" %in% list.files(dir))) {
    stop("You need to register your qualtrics API key first using the 'registerApiKey()' function.")
  } else {
    return(TRUE)
  }
}

# Test whether
