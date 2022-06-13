#' Download in progress responses from a survey and import it into R
#'
#' Download in progress responses from a Qualtrics survey you own via API and import the survey directly into R.
#'
#' @inheritParams fetch_survey
#'
#' @seealso See <https://api.qualtrics.com/> for documentation on
#' the Qualtrics API.
#' @template retry-advice
#' @importFrom lifecycle deprecated
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
#' # Retrieve a single survey
#' mysurvey <- fetch_in_progress(surveyID = surveys$id[6])
#'
#' mysurvey <- fetch_in_progress(
#'   surveyID = surveys$id[6],
#'   save_dir = tempdir(),
#'   start_date = "2018-01-01",
#'   end_date = "2018-01-31",
#'   limit = 100,
#'   label = TRUE,
#'   unanswer_recode = 999,
#'   verbose = TRUE,
#'   # Manually override EndDate to be a character vector
#'   col_types = readr::cols(EndDate = readr::col_character())
#' )
#'
#' }
#'
fetch_in_progress <- function(surveyID,
                         start_date = NULL,
                         end_date = NULL,
                         unanswer_recode = NULL,
                         unanswer_recode_multi = unanswer_recode,
                         include_display_order = TRUE,
                         limit = NULL,
                         include_questions = NULL,
                         save_dir = NULL,
                         force_request = FALSE,
                         verbose = TRUE,
                         label = TRUE,
                         convert = TRUE,
                         import_id = FALSE,
                         time_zone = NULL,
                         breakout_sets = TRUE,
                         add_column_map = TRUE,
                         add_var_labels = TRUE,
                         col_types = NULL) {

  ## Are the API credentials stored?
  assert_base_url()
  assert_api_key()

  check_params(
    verbose = verbose,
    convert = convert,
    import_id = import_id,
    time_zone = time_zone,
    label = label,
    start_date = start_date,
    end_date = end_date,
    include_questions = include_questions,
    save_dir = save_dir,
    unanswer_recode = unanswer_recode,
    unanswer_recode_multi = unanswer_recode_multi,
    include_display_order = include_display_order,
    limit = limit,
    breakout_sets = breakout_sets,
    add_column_map = add_column_map,
    add_var_labels = add_var_labels
  )

  # See if survey already in tempdir
  if (!force_request) {
    if (paste0(surveyID, "_in_progress.rds") %in% list.files(tempdir())) {
      data <- readRDS(paste0(tempdir(), "/", surveyID, "_in_progress.rds"))
      if (verbose) {
        rlang::inform(paste0(
          "Found an earlier download for survey with id ", surveyID, # nolint
          ". Loading this file.\nSet 'force_request' to TRUE if you want to override this."
        ))
      } # nolint
      return(data)
    }
  }

  # CONSTRUCT API CALL ----

  # fetch URL:
  fetch_url <- generate_url(query = "fetchsurvey",
                            surveyID = surveyID)

  # Create raw JSON payload
  raw_payload <- create_raw_payload(
    label = label,
    start_date = start_date,
    end_date = end_date,
    unanswer_recode = unanswer_recode,
    unanswer_recode_multi = unanswer_recode_multi,
    include_display_order = include_display_order,
    limit = limit,
    time_zone = time_zone,
    include_questions = include_questions,
    breakout_sets = breakout_sets,
    responses_in_progress = TRUE
  )

  # SEND POST REQUEST TO API ----

  # POST request for download
  res <- qualtrics_api_request("POST", url = fetch_url, body = raw_payload)
  # Get id
  if (is.null(res$result$progressId)) {
      stop("Something went wrong. Please re-run your query.")
    } else {
    requestID <- res$result$progressId
  } # NOTE This is not fail safe because ID can still be NULL

  # Download, unzip and return file path
  survey.fpath <- download_qualtrics_export(fetch_url, requestID, verbose = verbose)

  # READ DATA AND SET VARIABLES ----

  # Read data

  data <- read_survey(survey.fpath, import_id = import_id,
                      time_zone = time_zone,
                      col_types = col_types,
                      add_column_map = add_column_map)

  # Add types
  if (convert & label) {
    data <- infer_data_types(data, surveyID)
  }

  # Save survey as RDS file in temp folder so that it can be easily
  # retrieved this session.
  saveRDS(data, paste0(tempdir(), "/", surveyID, "_in_progress.rds"))

  # RETURN ----

  # Remove tmpfiles
  if (!is.null(save_dir)) {
    # Save file to directory
    saveRDS(data, file = paste0(save_dir, "/", surveyID, "_in_progress.rds"))
    # Return
    return(data)
  } else {
    p <- file.remove(survey.fpath)
    # Return
    return(data)
  }
}
