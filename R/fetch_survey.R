#' Download a survey and import it into R
#'
#' Download a Qualtrics survey you own via API and import the survey directly
#' into R.
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#'   Returned as `id` by the [all_surveys][qualtRics::all_surveys] function.
#' @param limit Integer. Maximum number of responses exported. Defaults to
#'   `NULL` (all responses).
#' @param start_date String. Filter to only exports responses recorded after the
#'   specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#'   Defaults to `NULL`.
#' @param end_date String. Filter to only exports responses recorded before the
#'   specified date. Accepts dates as character strings in format "YYYY-MM-DD".
#'   Defaults to `NULL`.
#' @param time_zone String. A local timezone to determine response date values.
#'   Defaults to `NULL`, which corresponds to system local time. See ["Dates and
#'   Times"](https://api.qualtrics.com/) from Qualtrics for more information on
#'   format.
#' @param include_display_order Display order information (such as for surveys
#'   with randomization).
#' @param include_metadata,include_questions,include_embedded Character vector.
#'   Specify variables to include in download.  Defaults to `NULL` (keep all).
#'   `NA` or `character()` excludes all variables for that category. See Details
#'   for unique requirements for each inclusion argument.
#' @param unanswer_recode Integer. Recode seen but unanswered questions with an
#'   integer-like value, such as 999. Defaults to `NULL`.
#' @param unanswer_recode_multi Integer. Recode seen but unanswered multi-select
#'   questions with an integer-like value, such as 999. Defaults to value for
#'   `unaswer_recode`.
#' @param breakout_sets Logical. If `TRUE`, then the [qualtRics::fetch_survey()]
#'   function will split multiple choice question answers into columns. If
#'   `FALSE`, each multiple choice question is one column. Defaults to `TRUE`.
#' @param import_id Logical. If `TRUE`, use Qualtrics import IDs instead of
#'   question IDs as column names. Will also alter names in the column map, if
#'   used. Defaults to `FALSE`.
#' @param label Logical. `TRUE` to export survey responses as Choice Text or
#'   `FALSE` to export survey responses as values.
#' @param convert Logical. If `TRUE`, then the [qualtRics::fetch_survey()]
#'   function will convert certain question types (e.g. multiple choice) to
#'   proper data type in R. Defaults to `TRUE`.
#' @param add_column_map Logical. If `TRUE`, then a column map data frame will
#'   be added as an attribute to the main response data frame. This column map
#'   captures Qualtrics-provided metadata associated with the response download,
#'   such as an item description and internal ID's. Defaults to `TRUE`.
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
#' @details The three `include_*` arguments each have different requirements to
#'   properly specify what  to the Qualtrics API.
#'
#'   *include_metadata*:
#'
#'   Elements must be one of the 17 Qualtrics metadata variables, listed here in
#'   their default order: *StartDate* (*startDate*), *EndDate* (*endDate*),
#'   *Status* (*status*), *IPAddress* (*ipAddress*), *Progress* (*progress*),
#'   *Duration (in seconds)* (*duration*), *Finished* (*finished*),
#'   *RecordedDate* (*recordedDate*), *ResponseId* (*_recordId*),
#'   *RecipientLastName* (*recipientLastName*), *RecipientFirstName*
#'   (*recipientFirstName*), *RecipientEmail* (*recipientEmail*),
#'   *ExternalReference* (*externalDataReference*), *LocationLatitude*
#'   (*locationLatitude*), *LocationLongitude* (*locationLongitude*),
#'   *DistributionChannel* (*distributionChannel*), *UserLanguage*
#'   (*userLanguage*).  Names in parentheses represent those returned when
#'   argument `import_id` is set to `TRUE`. However, the function can always
#'   generate a suitable request from either format, and names are not
#'   case-sensitive. Duplicate names will be silently dropped, and data frame
#'   will contain variables in the order specified after de-duplication.
#'
#'   *include_questions*:
#'
#'   Desired variables must be specified using Qualtrics's own unique internal
#'   ID's, rather than any user-specified names. All such ID's start with "QID"
#'   followed by a number.  If needed, a column map linking user-specified ID's
#'   to QID's can be quickly obtained by calling:
#'
#'   ```
#'
#'   fetch_survey(
#'        surveyID = {survey ID},
#'        limit = 1,
#'        add_column_map = TRUE
#'   ) |>
#'     extract_colmap()
#'
#'   ```
#'
#'   Note that while QID's are unique for each "question" in the Qualtrics
#'   sense, there may not be a 1:1 relationship between QID's and columns in the
#'   response data frame. If, for example, the question with ID *QID5* is a
#'   multiple choice item with a text box added to the third choice, the
#'   downloaded response data frame may include two associated variables:
#'   *"QID5"* (the multiple choice selection) and *"QID5_3_TEXT"* (the text
#'   box). Both variables will be returned by `include_questions = "QID5"`.
#'   Additionally, if randomization is used any associated display ordering
#'   variables will also be present (assuming `include_display_order = TRUE`).
#'   Currently, separating these via the API does not appear possible (e.g.,
#'   `include_questions = "QID5_3_TEXT"` will produce an API error).
#'
#'  `include_embedded`:
#'
#'   This argument accepts the user-specified names of any embedded
#'   data variables in the survey being accessed.
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

