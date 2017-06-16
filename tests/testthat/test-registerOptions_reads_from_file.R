test_that("registerOptions() can read from file", {
  # Store .qualtRics.yml in temporary directory
  io <- yaml::as.yaml(list(
    "api_token"="1234",
    "root_url"="ABCD",
    "verbose"=TRUE,
    "uselabels"=TRUE,
    "convertstandardcolumns"=TRUE,
    "uselocaltime"= FALSE,
    "datewarning"= TRUE
  ))
  setwd(tempdir())
  write(io, ".qualtRics.yml")
  expect_message(qualtRics::registerOptions(),
                 "Found a .qualtRics.yml configuration file in")
})
