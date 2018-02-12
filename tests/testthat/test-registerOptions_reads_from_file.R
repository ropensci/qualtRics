test_that("registerOptions() can read from file", {
  # Store .qualtRics.yml in temporary directory
  io <- yaml::as.yaml(list(
    "api_token"="1234",
    "root_url"="ABCD",
    "verbose"=TRUE,
    "uselabels"=TRUE,
    "convertvariables"=TRUE,
    "uselocaltime"= FALSE,
    "datewarning"= TRUE
  ))
  # Set environment variables to ""
  Sys.setenv("QUALTRICS_API_KEY"="",
             "QUALTRICS_ROOT_URL"="")
  # Save WD and set wd
  curr.wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(curr.wd))
  # Write yml file
  write(io, ".qualtRics.yml")
  expect_message(qualtRics::registerOptions(),
                 "Found a .qualtRics.yml configuration file")
})
