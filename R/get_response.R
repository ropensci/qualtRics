# Get individual responses

get_response <- function() {

  # CHECK PARAMS AND PREP QUERY ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Function-specific API stuff
  surveys_url <- generate_url(query = "response")

  # SEND REQUEST TO QUALTRICS ----

  # Send GET request to list all surveys
  resp <- qualtrics_api_request("GET", surveys_url)
  # Put results in list
  master <- list()
  # Append results
  master <- append(master, resp$result$elements)

  # WRAP-UP AND RETURN ----

  # Bind to one large data frame & return
  d <- bind_rows(master)
  return(d)
}
