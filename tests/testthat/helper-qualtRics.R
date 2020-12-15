
`readRenviron("~/.Renviron")`
library(vcr)

# Load in credentials, if saved (for new tests):
readRenviron("~/.Renviron")

# Use default URL rather than branded one:
Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")

# Set up a fake API key if none is saved
if (Sys.getenv("QUALTRICS_API_KEY") == ""){
 Sys.setenv("QUALTRICS_API_KEY" = "1234")
}

# Store these for resetting later when needed
holder_API <- Sys.getenv("QUALTRICS_API_KEY")
holder_URL <- Sys.getenv("QUALTRICS_BASE_URL")

# NOTE: If writing new test that changes the credentials for some reason, add
# this line (uncommented) to end of the testing file:
# qualtrics_api_credentials(api_key = holder_API, base_url = holder_URL)



  # Set directory and mask API token
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  preserve_exact_body_bytes = TRUE,
  filter_sensitive_data =
    list(
      "<<<my_api_key>>>" = Sys.getenv('QUALTRICS_API_KEY')
    )))
