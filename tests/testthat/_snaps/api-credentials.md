# absence of API key or base URL raises an error

    Qualtrics API key and/or base URL need registering:
    i Use `qualtrics_api_credentials()`

# can store and access credentials

    Code
      qualtrics_api_credentials(api_key = "1234", base_url = "abcd")
    Condition
      Error in `checkarg_base_url()`:
      ! Error in argument `base_url`
      * `base_url` must be of the form '{datacenter ID}.qualtrics.com'
      * See https://api.qualtrics.com/ZG9jOjg3NjYzMw-base-url-and-datacenter-i-ds

---

    Code
      qualtrics_api_credentials(api_key = "1234", base_url = "https://abcd.qualtrics.com")
    Message
      Protocol (e.g. 'https://)' not needed in `base_url`, removing.
      To install your credentials for use in future sessions, run this function with `install = TRUE`.

