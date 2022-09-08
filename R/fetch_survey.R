#' Download a survey and import it into R
#'
#' Download a Qualtrics survey you own via API and import the survey directly
#' into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#'   Returned as `id` by the [all_surveys][qualtRics::all_surveys] function.
#' @param limit Integer. Maximum number of responses exported. Defaults to
#'   `NULL` (download all responses).
#' @param start_date,end_date POSIXct, POSIXlt, or Date object, or length-1
#'   string equivalent of form "YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS". ("/" is
#'   also acceptable in place of "-".)  Only export survey responses that were
#'   **recorded** within the range specified by one or both arguments (i.e.
#'   referencing *RecordedDate*). Each defaults to `NULL` (unbounded).  See
#'   Details for important information about both the package and Qualtrics'
#'   handling of start/end times.
#' @param time_zone String. Time zone to use for date/time metadata variables in
#'   response dataframe (e.g. *StartDate*). Must match a time zone name from
#'   [base::OlsonNames()]. Defaults to `NULL`, which uses the current system
#'   timezone (from [base::Sys.timezone()]).  Also applied to arguments
#'   `start_date` and/or `expiration_date` when given Date or string objects
#'   (see above); ignored when these arguments are given POSIXlt/POSIXct
#'   objects.
#' @param include_display_order Logical.  If `TRUE`, download from surveys using
#'   block/question/answer display randomization will include contain additional
#'   variables indicating the randomization pattern used for each case. Defaults
#'   to `FALSE`.
#' @param include_metadata,include_questions,include_embedded Character vector.
#'   Specify variables to include in download.  Defaults to `NULL` (keep all).
#'   `NA` or `character()` excludes all variables for that category. See Details
#'   for more on using each inclusion argument.
#' @param unanswer_recode Integer-like. Recode seen-but-unanswered (usually
#'   skipped) questions using this value. Defaults to `NA`
#' @param unanswer_recode_multi Integer-like. Recode seen-but-unanswered
#'   multi-select questions (checkboxes) using this value. Defaults to value for
#'   `unaswer_recode`.
#' @param breakout_sets Logical. If `TRUE` multi-value fields (e.g. each option
#'   of a multi-select multiple choice questions) will be returned as separate
#'   columns.  If `FALSE`, will be returned as 1 column with each element
#'   containing all values.
#' @param import_id Logical. If `TRUE`, column names will use Qualtrics import
#'   IDs (e.g. "QID123") instead of user-modifiable names (e.g. default names
#'   like "Q3" or custom names).  Defaults to `FALSE` (user-modifiable names).
#'   Note that this also affects (otherwise unmodifiable) names of metadata
#'   columns--see the "`include_metadata`" section in Details below.
#' @param label Logical. If `TRUE` (default), will return text of answer
#'   choices, instead of recoded values (`FALSE`).
#' @param convert Logical. If `TRUE`, then the [qualtRics::fetch_survey()]
#'   function will convert certain question types (e.g. multiple choice) to
#'   proper data type in R. Defaults to `TRUE`.
#' @param add_column_map Logical. Add an attribute to the returned response data
#'   frame containing metadata associated with the response download, including
#'   variable names, question/choice text, and Qualtrics import IDs. This column
#'   map can be subsequently obtained using [qualtRics::extract_colmap()]
#'   Defaults to `TRUE`.
#' @param add_var_labels Logical. If `TRUE`, then the item description from each
#'   variable (equivalent to the one in the column map) will be added as a
#'   "label" attribute using [sjlabelled::set_label()]. Useful for reference as
#'   well as cross-compatibility with other stats packages (e.g., Stata, see
#'   documentation in `sjlabelled`). Defaults to `TRUE`.
#' @param col_types Optional. This argument provides a way to manually overwrite
#'   column types that may be incorrectly guessed. Takes a [readr::cols()]
#'   specification. See example below and [readr::cols()] for formatting
#'   details. Defaults to `NULL`. Overwritten by `convert = TRUE`.
#' @param force_request Logical. fetch_survey() saves each survey in a temporary
#'   directory so that it can quickly be retrieved later. If force_request is
#'   `TRUE`, fetch_survey() always downloads the survey from the API instead of
#'   loading it from the temporary directory. Defaults to `FALSE`.
#' @param save_dir String. Directory where survey results will be stored.
#'   Defaults to a temporary directory which is cleaned when your R session is
#'   terminated. This argument is useful if you'd like to store survey results.
#'   The downloaded survey will be stored as an RDS file (see
#'   [base::readRDS()]).
#' @param verbose Logical. If `TRUE`, verbose messages will be printed to the R
#'   console. Defaults to `TRUE`.
#' @param last_response Deprecated.
#'
#' @seealso See <https://api.qualtrics.com/> for documentation on the Qualtrics
#'   API.
#' @template retry-advice
#' @details
#'
#' # `start_date` & `end_date` arguments
#'
#' The Qualtrics API endpoint for this function treats `start_date` and
#' `end_date` slightly differently; `end_date` is *exclusive*, meaning only
#' responses recorded up to the moment *before* the specified `end_date` will be
#' returned.  This permits easier automation; a previously-used `end_date` can
#' become the `start_date` of a subsequent request without downloading duplicate
#' records.
#'
#' As a convenience for users working interactively, the qualtRics package
#' also accepts Date(-like) input to each argument, which when used implies a
#' time of 00:00:00 on the given date (and time zone). When a Date(-like) is
#' passed to `end_date`, however, the date will be incremented by one before
#' making the API request. This adjustment is intended to provide interactive
#' users with more intuitive results; for example, specifying "2022/06/02" for
#' both `start_date` and `end_date` will return all responses for that day,
#' (instead of the zero responses that would return if `end_date` was not
#' adjusted).
#'
#' # Inclusion/exclusion arguments
#'
#' The three `include_*` arguments each have different requirements:
#'
#' ## `include_metadata`
#'
#' Elements must be one of the 17 Qualtrics metadata variables, listed here in
#' their default order: *StartDate* (*startDate*), *EndDate* (*endDate*),
#' *Status* (*status*), *IPAddress* (*ipAddress*), *Progress* (*progress*),
#' *Duration (in seconds)* (*duration*), *Finished* (*finished*), *RecordedDate*
#' (*recordedDate*), *ResponseId* (*_recordId*), *RecipientLastName*
#' (*recipientLastName*), *RecipientFirstName* (*recipientFirstName*),
#' *RecipientEmail* (*recipientEmail*), *ExternalReference*
#' (*externalDataReference*), *LocationLatitude* (*locationLatitude*),
#' *LocationLongitude* (*locationLongitude*), *DistributionChannel*
#' (*distributionChannel*), *UserLanguage* (*userLanguage*).
#'
#' Names in parentheses are those returned by the API endpoint when `import_id`
#' is set to `TRUE`. The argument `include_metadata` can accept either format
#' regardless of `import_id` setting, and names are not case-sensitive.
#' Duplicate elements passed to `include_metadata` will be silently dropped,
#' with the de-duplicated variable located in the first position.
#'
#' ## `include_questions`
#'
#' Qualtrics uniquely identifies each question with an internal ID that takes
#' the form "QID" followed by a number, e.g. *QID5*.  When using
#' `include_questions`, these internal IDs must be used rather than
#' user-customizable variable names (which need not be unique in Qualtrics).  If
#' needed, a column map linking customizable names to QID's can be quickly
#' obtained by calling:
#'
#' ```
#' my_survey <- fetch_survey(
#'     surveyID = {survey ID},
#'     limit = 1,
#'     add_column_map = TRUE
#' )
#' extract_colmap(my_survey)
#' ```
#'
#' Note that while there is one QID for each "question" in the Qualtrics sense,
#' each QID may still map to multiple columns in the returned data frame. If,
#' for example, a "question" with ID *QID5* is a multiple-choice item with a
#' text box added to the third choice, the returned data frame may have two
#' related columns: *"QID5"*  for the multiple choice selection, and
#' *"QID5_3_TEXT"* for the text box (or, more typically, their custom names).
#' Setting `include_questions = "QID5"` will always return both columns.
#' Similarly, "matrix" style multiple-choice questions will have a column for
#' each separate row of the matrix. Also, when `include_display_order = TRUE`,
#' display ordering variables for any randomization will be included. Currently,
#' separating these sub-questions via the API does not appear possible (e.g.,
#' `include_questions = "QID5_3_TEXT"` will result in an API error).
#'
#' ## `include_embedded`
#'
#' This argument accepts the user-specified names of any embedded data variables
#' in the survey being accessed.
#'
#' @importFrom lifecycle deprecated
#' @importFrom purrr compact
#' @importFrom glue glue
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
#' my_survey <- fetch_survey(surveyID = surveys$id[6])
#'
#' my_survey <- fetch_survey(
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

    # Check/format arguments --------------------------------------------------

    if (lifecycle::is_present(last_response)) {
      lifecycle::deprecate_warn("3.1.2", "fetch_survey(last_response = )")
    }

    # Check if API credentials stored (and likely suitable)
    check_credentials()

    # Check/format date/time arguments:
    start_date_formatted <-
      checkarg_datetime(start_date, time_zone = time_zone)
    end_date_formatted <-
      checkarg_datetime(end_date, time_zone = time_zone, endofday = FALSE)

    # Check/format include_* arguments
    include_metadata_formatted <-
      checkarg_include_metadata(include_metadata)
    include_questions_formatted <-
      checkarg_include_questions(include_questions)
    include_embedded_formatted <-
      checkarg_include_embedded(include_embedded)

    # Check other unique arguments
    checkarg_col_types(col_types)
    checkarg_limit(limit)
    checkarg_save_dir(save_dir)
    checkarg_convert_label_breakouts(convert, label, breakout_sets)

    # Check general argument types:
    checkarg_isintegerish(unanswer_recode)
    checkarg_isintegerish(unanswer_recode_multi)
    checkarg_isboolean(include_display_order)
    checkarg_isboolean(force_request)
    checkarg_isboolean(verbose)
    checkarg_isboolean(import_id)
    checkarg_isboolean(add_column_map)
    checkarg_isboolean(add_var_labels)


    # Re-load existing file if present ----------------------------------------

    # Location for downloaded file to go (or be):
    file_location <-
      glue::glue("{tempdir()}/{surveyID}.rds")

    # See if survey already in temporary directory:
    if (!force_request) {
      download_exists <-
        check_existing_download(file_location = file_location,
                                surveyID = surveyID,
                                verbose = verbose)

      if(download_exists){
        return(readRDS(file_location))
      }
    }


    # Make 3-part request to export-responses endpoint -------------------------------

    # Create raw JSON payload (request body)
    # Names are param names for endpoint, as specified at:
    # https://api.qualtrics.com/6b00592b9c013-start-response-export
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


    survey_fpath <-
      export_responses_request(
        surveyID = surveyID,
        body = raw_payload,
        verbose = verbose
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
      alt_file_location <-
        glue::glue("{save_dir}/{surveyID}.rds")

      saveRDS(data, alt_file_location)
    }

    # Return
    return(data)

  }

