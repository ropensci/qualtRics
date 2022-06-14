#' Download a survey and import it into R
#'
#' Download a Qualtrics survey you own via API and import the survey directly into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#' Returned as `id` by the [all_surveys][qualtRics::all_surveys] function.
#' @param last_response Deprecated.
#' @param start_date String. Filter to only exports responses recorded after the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to `NULL`.
#' @param end_date String. Filter to only exports responses recorded before the
#' specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#' Defaults to `NULL`.
#' @param unanswer_recode Integer. Recode seen but unanswered questions with an
#' integer-like value, such as 999. Defaults to `NULL`.
#' @param unanswer_recode_multi Integer. Recode seen but unanswered multi-select
#' questions with an integer-like value, such as 999. Defaults to value for
#' `unaswer_recode`.
#' @param include_display_order Display order information (such as for
#' surveys with randomization).
#' @param limit Integer. Maximum number of responses exported. Defaults to
#' `NULL` (all responses).
#' @param include_questions Vector of strings (e.g. c('QID1', 'QID2', 'QID3').
#' Export only specified questions. Defaults to `NULL`.
#' @param save_dir String. Directory where survey results will be stored.
#' Defaults to a temporary directory which is cleaned when your R session is
#' terminated. This argument is useful if you'd like to store survey results.
#' The downloaded survey will be stored as an RDS file (see
#' [base::readRDS()]).
#' @param force_request Logical. fetch_survey() saves each survey in a temporary
#' directory so that it can quickly be retrieved later. If force_request is
#' `TRUE`, fetch_survey() always downloads the survey from the API instead
#' of loading it from the temporary directory. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, verbose messages will be printed to
#' the R console. Defaults to `TRUE`.
#' @param label Logical. `TRUE` to export survey responses as Choice Text
#' or `FALSE` to export survey responses as values.
#' @param convert Logical. If `TRUE`, then the
#' [qualtRics::fetch_survey()] function will convert certain question
#' types (e.g. multiple choice) to proper data type in R. Defaults to `TRUE`.
#' @param import_id Logical. If `TRUE`, use Qualtrics import IDs instead of
#' question IDs as column names. Will also alter names in the column map, if
#' used. Defaults to `FALSE`.
#' @param time_zone String. A local timezone to determine response date
#' values. Defaults to `NULL` which corresponds to UTC time. See
#' ["Dates and Times"](https://api.qualtrics.com/) from Qualtrics
#' for more information on format.
#' @param breakout_sets Logical. If `TRUE`, then the
#' [qualtRics::fetch_survey()] function will split multiple
#' choice question answers into columns. If `FALSE`, each multiple choice
#' question is one column. Defaults to `TRUE`.
#' @param add_column_map Logical. If `TRUE`, then a column map data frame
#' will be added as an attribute to the main response data frame.
#' This column map captures Qualtrics-provided metadata associated with the
#' response download, such as an item description and internal ID's. Defaults to
#' `TRUE`.
#' @param add_var_labels Logical. If `TRUE`, then the item description from
#' each variable (equivalent to the one in the column map) will be added as a
#' "label" attribute using [sjlabelled::set_label()]. Useful for
#' reference as well as cross-compatibility with other stats packages (e.g.,
#' Stata, see documentation in `sjlabelled`). Defaults to `TRUE`.
#' @param col_types Optional. This argument provides a way to manually overwrite
#' column types that may be incorrectly guessed. Takes a [readr::cols()]
#' specification. See example below and [readr::cols()] for formatting
#' details. Defaults to `NULL`. Overwritten by `convert = TRUE`.
#'
#' @seealso See <https://api.qualtrics.com/> for documentation on
#' the Qualtrics API.
#' @template retry-advice
#' @importFrom lifecycle deprecated
#' @importFrom purrr compact
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
#' mysurvey <- fetch_survey(surveyID = surveys$id[6])
#'
#' mysurvey <- fetch_survey(
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
fetch_survey <-
  function(surveyID,
           limit = NULL,
           start_date = NULL,
           end_date = NULL,
           time_zone = NULL,
           include_display_order = TRUE,
           include_metadata = NULL,
           include_questions = NULL,
           include_embedded = NULL,
           unanswer_recode = NULL,
           unanswer_recode_multi = unanswer_recode,
           breakout_sets = TRUE,
           import_id = FALSE,
           label = TRUE,
           convert = TRUE,
           add_column_map = TRUE,
           add_var_labels = TRUE,
           col_types = NULL,
           force_request = FALSE,
           verbose = TRUE,
           save_dir = NULL,
           last_response = deprecated()
  ) {

    if (lifecycle::is_present(last_response)) {
      lifecycle::deprecate_warn("3.1.2", "fetch_survey(last_response = )")
    }

    ## Are the API credentials stored (and likely suitable)?
    check_credentials()

    # Check arguments:
    start_date_formatted <-
      checkarg_datetime(start_date, time_zone = time_zone)
    end_date_formatted <-
      checkarg_datetime(end_date, time_zone = time_zone, endofday = FALSE)

    include_metadata_formatted <-
      checkarg_include_metadata(include_metadata)
    include_questions_formatted <-
      checkarg_include_questions(include_questions)
    include_embedded_formatted <-
      checkarg_include_embedded(include_embedded)

    checkarg_col_types(col_types)
    checkarg_limit(limit)
    checkarg_save_dir(save_dir)
    checkarg_convert_label_breakouts(convert, label, breakout_sets)


    checkarg_isintegerish(unanswer_recode)
    checkarg_isintegerish(unanswer_recode_multi)

    checkarg_isboolean(include_display_order)
    checkarg_isboolean(force_request)
    checkarg_isboolean(verbose)
    checkarg_isboolean(import_id)
    checkarg_isboolean(add_column_map)
    checkarg_isboolean(add_var_labels)

    # Generate a location for downloaded file to go:
    file_location <-
      paste0(tempdir(), "/", surveyID, ".rds")

    # See if survey already in temporary directory:
    if (!force_request) {

      if (file.exists(file_location)) {
        data <-
          readRDS(file_location)

        if (verbose) {
          rlang::inform(
            c(glue::glue("Loading saved download for surveyID = {surveyID}.",
                         surveyID = surveyID),
              "Set 'force_request = TRUE' to override this.")
          )
        }
        return(data)
      }
    }

    # Construct initial API request ----

    # fetch URL:
    fetch_url <-
      generate_url(
        query = "fetchsurvey",
        surveyID = surveyID
      )

    # Create raw JSON payload
    raw_payload <-
      create_raw_payload(
        format = "csv",
        useLabels = label,
        startDate = start_date_formatted,
        endDate = end_date_formatted,
        limit = limit,
        seenUnansweredRecode = unanswer_recode,
        multiselectSeenUnansweredRecode = unanswer_recode_multi,
        includeDisplayOrder = include_display_order,
        questionIds = include_questions_formatted,
        embeddedDataIds = include_embedded_formatted,
        surveyMetadataIds = include_metadata_formatted,
        breakoutSets = breakout_sets
      )


    # Send POST request to API ----

    # POST request for download
    res <-
      qualtrics_api_request(
        verb = "POST",
        url = fetch_url,
        body = raw_payload
      )

    # Get progress id
    if (is.null(res$result$progressId)) {
      rlang::abort(
        c("Qualtrics failed to return a progress ID for your download request",
          "Please re-run your query.")
      )
    } else {
      requestID <-
        res$result$progressId
    } # NOTE This is not fail safe because ID can still be NULL


    # Monitor progress & get location of file path ----------------------------

    fileID <-
      fetch_survey_progress(
        surveyID = surveyID,
        requestID = requestID,
        verbose = verbose
      )


    # Download .zip file and unzip it -----------------------------------------

    survey_fpath <-
      fetch_survey_filedownload(
        surveyID = surveyID,
        fileID = fileID
      )


    # Read downloaded .csv & clean -------------------------------------------

    data <-
      read_survey(
        file_name = survey_fpath,
        import_id = import_id,
        time_zone = time_zone,
        col_types = col_types,
        add_column_map = add_column_map
      )

    # Remove pre-cleaned temporary file:
    file.remove(survey_fpath)

    # Add types
    if (convert & label) {
      data <-
        infer_data_types(data, surveyID)
    }

    # Save and return data -----------------------------------------------------

    # Save survey as RDS file in temp folder so that it can be easily
    # retrieved again this session.
    saveRDS(data, file_location)

    # Save file to specified alternative directory if given:
    if (!is.null(save_dir)) {
      saveRDS(data, file = paste0(save_dir, "/", surveyID, ".rds"))
    }

    # Return
    return(data)

  }


#' Monitor progress from response request download, then obtain file download
#' location
#'
#' @param surveyID ID of survey whose responses are being pulled
#' @param requestID exportProgressId from
#'   https://api.qualtrics.com/37e6a66f74ab4-get-response-export-progress
#' @param verbose See [fetch_survey()]
#' @template retry-advice
#' @keywords internal

fetch_survey_progress <-
  function(surveyID,
           requestID,
           verbose = FALSE) {

    # This is the URL to use when checking the progress
    progress_url <-
      generate_url(
        "fetchsurvey_progress",
        surveyID = surveyID,
        requestID = requestID
      )

    # Create a progress bar and monitor when export is ready
    if (verbose) {
      pbar <-
        utils::txtProgressBar(
          min = 0,
          max = 100,
          style = 3
        )
    }

    # Initialize progress
    progress <- 0
    # While download is in progress
    while (progress < 100) {
      # Get percentage complete
      CU <-
        qualtrics_api_request(
          verb = "GET",
          url = progress_url
        )

      progress <-
        CU$result$percentComplete

      # Set progress
      if (verbose) {
        utils::setTxtProgressBar(pbar, progress)
      }
    } # end while loop (progress complete)
    # Kill progress bar
    if (verbose) {
      close(pbar)
    }

    # Get the fileID showing location of the downloadable file:
    fileID <-
      CU$result$fileId
    return(fileID)
  }

#' Downloads response data (.zip of .csv) from location obtained from
#' fetch_survey_progress
#'
#' @param surveyID survey ID
#' @param requestID request ID from fetch_survey
#' @param fileID file ID from fetch_survey_progress
#' @keywords internal

fetch_survey_filedownload <-
  function(surveyID,
           fileID){

    # Construct a url for obtaining the file:
    file_url <-
      generate_url(
        "fetchsurvey_file",
        surveyID = surveyID,
        fileID = fileID
      )

    # Load raw zip file:
    raw_zip <-
      qualtrics_api_request(
        verb = "GET",
        url = file_url,
        as = "raw"
      )

    # To zip file
    tf_path <-
      glue::glue(
        "{temp_dir}/temp.zip",
        # Remove trailing slash if system includes one:
        temp_dir = stringr::str_remove(tempdir(), "/$")
      )

    # Write to temporary file
    writeBin(raw_zip, tf_path)

    # Create error handling around unzipping:
    safeunzip <-
      purrr::possibly(
        utils::unzip,
        NULL
      )

    # Unzip and get the filepath for the csv
    csv_filepath <-
      safeunzip(
        zipfile = tf_path,
        exdir = tempdir()
      )

    if(is.null(csv_filepath)){
      rlang::abort(
        c("Error extracting CSV from zip file",
          "The download may have been corrupted; try re-running your query",
          "Current download file location:",
          tf_path)
      )
    }

    # Remove zipfile
    file.remove(tf_path)


    # Return file location
    return(csv_filepath)
  }
