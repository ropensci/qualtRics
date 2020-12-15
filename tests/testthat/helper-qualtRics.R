
`readRenviron("~/.Renviron")`
library(vcr)

# Load in credentials, if saved (for new tests):
readRenviron("~/.Renviron")

# Use default URL rather than branded one:
Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")

# Set directory and mask API token
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  preserve_exact_body_bytes = TRUE,
  filter_sensitive_data =
    list(
      "<<<my_api_key>>>" = Sys.getenv('QUALTRICS_API_KEY')
    )))

