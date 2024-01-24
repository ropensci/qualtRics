# correct error for deprecated args

    Code
      fetch_survey("1234", force_request = TRUE)
    Condition
      Warning:
      The `force_request` argument of `fetch_survey()` is deprecated as of qualtRics 3.2.0.
      Error in `qualtrics_response_codes()`:
      ! Qualtrics API reported a not found error (404):
      * Please check if you are using the correct survey ID.

---

    Code
      fetch_survey("1234", save_dir = "~/Desktop")
    Condition
      Warning:
      The `save_dir` argument of `fetch_survey()` is deprecated as of qualtRics 3.2.0.
      Error in `qualtrics_response_codes()`:
      ! Qualtrics API reported a not found error (404):
      * Please check if you are using the correct survey ID.

