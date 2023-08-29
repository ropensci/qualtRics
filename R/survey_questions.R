#' Retrieve a data frame containing question IDs and labels
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as `id` by the \link[qualtRics]{all_surveys} function.
#'
#' @seealso See \url{https://api.qualtrics.com/} for documentation on the
#' Qualtrics API.
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#'
#' # Retrieve a list of surveys
#' surveys <- all_surveys()
#'
#' # Retrieve questions for a survey
#' questions <- survey_questions(surveyID = surveys$id[6])
#'
#' # Retrieve a single survey, filtering for specific questions
#' mysurvey <- fetch_survey(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   include_questions = c("QID1", "QID2", "QID3"),
#'   verbose = TRUE
#' )
#' }
#'
survey_questions <- function(surveyID) {

  # Check params
  assert_base_url()
  assert_api_key()

  surveys_url <- generate_url(query = "metadata",
                              surveyID = surveyID)

  # SEND REQUEST TO API ----

  # GET request to download metadata
  resp <- qualtRics:::qualtrics_api_request("GET", surveys_url)

  # Get question information
  survey_metadata <- resp$result

  # Get questions
  qi <- survey_metadata$questions

  # Get blocks
  blocks <- survey_metadata$blocks

  blocks_tib <- tibble(blocks) %>%
    unnest_wider(blocks) %>%
    unnest_longer(elements) %>%
    unnest(elements) %>%
    filter(names(elements) == "questionId") %>%
    unnest(elements) %>%
    rename("qid" = elements,"block_name" = description)


  # Extract and join choiceText values with line breaks

  response_options <- map(qi, 6) %>%
    map(., ~ .x %>% map_chr("choiceText") %>%
          paste0(.x$choiceText, collapse = "\n")) %>%
    unlist()


  # Create a data frame with question information
  quest <- tibble(
    qid = names(qi),
    qname = purrr::map_chr(qi, "questionName"),
    question = purrr::map_chr(qi, "questionText"),
    force_resp = purrr::map_lgl(qi, ~ .$validation$doesForceResponse),
    response_options = response_options  # Add response options as a string
  )

  quest <- left_join(quest, blocks_tib, by = "qid")

  return(quest)
}
