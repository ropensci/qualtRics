# On unload
.onUnload <- function(libname = find.package("qualtRics"), pkgname = "qualtRics") {

  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_BASE_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")
}
