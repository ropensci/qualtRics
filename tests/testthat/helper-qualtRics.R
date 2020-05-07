
library(vcr)
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  preserve_exact_body_bytes = TRUE,
  filter_sensitive_data = list("<<<my_api_key>>>" = Sys.getenv('QUALTRICS_API_KEY'),
                               "<<<my_base_url>>>" = Sys.getenv('QUALTRICS_BASE_URL'))
))
