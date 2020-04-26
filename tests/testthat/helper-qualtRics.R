
library("vcr")
invisible(vcr::vcr_configure(
  dir = "tests/fixtures",
  preserve_exact_body_bytes = TRUE
))
