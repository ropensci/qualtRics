# correct error for deprecated args

    Code
      fetch_survey("1234", force_request = TRUE)
    Warning <lifecycle_warning_deprecated>
      The `force_request` argument of `fetch_survey()` is deprecated as of qualtRics 3.2.0.
    Error <rlang_error>
      Qualtrics API reported a not found error (404):
      * Please check if you are using the correct survey ID.

---

    Code
      fetch_survey("1234", save_dir = "~/Desktop")
    Warning <lifecycle_warning_deprecated>
      The `save_dir` argument of `fetch_survey()` is deprecated as of qualtRics 3.2.0.
    Error <rlang_error>
      Qualtrics API reported a not found error (404):
      * Please check if you are using the correct survey ID.

