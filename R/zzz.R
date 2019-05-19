# Loads qualtRics credentials automatically when package is loaded
# and ".qualtRics.yml" file is present in working directory. User
# needs to have #qualtRics API key and root url stored in a configuration
# file in working directory. For an example of a configuration file,
# execute "qualtRicsConfigFile()". See:
# https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file # nolint


.onLoad <- function(libname = find.package("qualtRics"), pkgname = "qualtRics") {
  if (file.exists(".qualtRics.yml")) {
    # load 'registeroptions()'
    suppressWarnings(registerOptions())
  }
}

# On unload
.onUnload <- function(libname = find.package("qualtRics"), pkgname = "qualtRics") {

  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_ROOT_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")
}
