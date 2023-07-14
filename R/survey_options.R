#' Update metadata options
#'
#' @param surveyID String. Unique ID for the survey whose metadata you wish to
#'   update.
#' @param name String. Set survey name.  Defaults to NULL, which leaves existing
#'   name.
#' @param description String. Set survey description. Defaults to `NULL`, which
#'   leaves description unchanged.  Entering `NA` will remove any existing
#'   description.
#' @param active Boolean. Activates/deactivates survey (whether it is open for
#'   data collection).  Defaults to `NULL`, which leaves status unchanged.
#' @param activation_date POSIXlt, POSIXct, Date, or string equivalent.  Sets
#'   date/time for survey becoming active. Ignored with warning if `active` is
#'   set to `TRUE`. See Details.
#' @param expiration_date POSIXlt, POSIXct, Date, or string equivalent.  Sets
#'   date/time for survey becoming inactive. Ignored with warning if `active` is
#'   set to `FALSE`. See Details.
#' @param time_zone String. Desired time zone to use when `start_date` or
#'   `expiration_date` are given as Date or string objects.  Ignored if
#'   argument(s) given as POSIXlt/POSIXct objects. Must match a time zone name
#'   from [base::OlsonNames()]. Defaults to `NULL`, which uses the current
#'   system timezone ([base::Sys.timezone()]).
#' @param confirm Boolean. If `TRUE` (default), will make an additional API call
#'   to confirm that survey options match any changes requested, and print
#'   informative error if not matching.
#' @param verbose Boolean. If `TRUE` (default) and `confirm = TRUE`, will
#'   provide messages indicating successful checks for each survey.  Also will
#'   provide message if API call is skipped, which occurs when all arguments
#'   left empty.
#' @details
#'
#' As a convenience for users, the `*qualtRics* package accepts Date(-like)
#' input both `activation_date` and `expiration_date`, which when used implies a
#' time of 00:00:00 in the specified time zone). When a Date(-like) is passed to
#' `expiration_date`, however, the date will be incremented by one before making
#' the API request. This adjustment is intended to provide users more intuitive
#' results: specifying "2022/06/02" for `expiration_date` will keep the survey
#' open throughout 2022/06/02 (closing at 2022/06/03 00:00:00). Generally,
#' though, using POSIXlt(-like) input is recommended.
#'
#' @export
update_survey_metadata <-
  function(
    surveyID,
    name = NULL,
    description = NULL,
    active = NULL,
    activation_date = NULL,
    expiration_date = NULL,
    time_zone = NULL,
    confirm = TRUE,
    verbose = TRUE
  ) {

    # ARGUMENT CHECKING -----

    check_credentials()
    checkarg_isstring(surveyID, null_okay = FALSE)
    checkarg_isstring(name)
    checkarg_isstring(description, na_okay = TRUE)
    checkarg_isboolean(verbose)

    active_formatted <-
      checkarg_survey_active(active)
    activation_date_formatted <-
      checkarg_activation_date(activation_date, active,
                                  time_zone, surveyID)
    expiration_date_formatted <-
      checkarg_expiration_date(expiration_date, active,
                                  time_zone, surveyID)

    # Convert ISO 8601 datetimes to MySQL datetimes by removing T&Z:
    #
    #  NOTE: process below changes NULL's to length-0 character() vectors,
    #  but create_raw_payload() below handles that fine.
    activation_date_formatted <-
      stringr::str_replace_all(
        string = activation_date_formatted,
        pattern = c("T" = " ", "Z" = "")
      )

    expiration_date_formatted <-
      stringr::str_replace_all(
        string = expiration_date_formatted,
        pattern = c("T" = " ", "Z" = "")
      )

    # BUILD REQUEST -----

    metadata_url <-
      generate_url(query = "surveydefinitions_metadata",
                   surveyID = surveyID)

    update_metadata_body <-
      create_raw_payload(
        SurveyName = name,
        SurveyDescription = description,
        SurveyStatus = active_formatted,
        SurveyStartDate = activation_date_formatted,
        SurveyExpirationDate = expiration_date_formatted,
        .NAtoNULL = c("SurveyDescription", "SurveyExpirationDate")
      )

    # Check that at least one option has been specified, and
    # skip (possibly w/message) if no new options specified for survey:
    call_args <-
      jsonlite::fromJSON(
        update_metadata_body
      )


    if(length(call_args) == 0 && verbose == TRUE){
      rlang::inform(
        glue::glue(
          "No new options specified for {surveyID}, skipping API query"
        )
      )
      return(invisible())
    }

    # QUERY API ----

    # Send PUT request to survey-definitions metadata endpoint:
    qualtrics_api_request(
      verb = "PUT",
      url = metadata_url,
      body = update_metadata_body
    )

    # CHECK FOR SUCCESS ------------------------------------------------------
    if(confirm){
      # Check that options are now matching
      names_call_args <-
        names(call_args)

      # NOTES:
      #  Have to convert any activation/expiration args back to ISO 8601 here.
      #  ONLY the PUT request uses MySQL format. The GET request for the same
      #  thing here that we're now checking against
      #  returns dates in ISO 8601 (like everywhere else in Qualtrics's API).

      call_args <-
        purrr::map_if(
          call_args,
          names(call_args) %in% c("SurveyStartDate", "SurveyExpirationDate"),
          ~lubridate::format_ISO8601(
            lubridate::ymd_hms(.x),
            usetz = "Z"
          )
        )


      # Query the GET survey metadata endpoint for a comparison:
      update_check <-
        .get_survey_metadata(surveyID)[names_call_args]

      if(!identical(update_check, call_args)){
        rlang::abort(
          c(
            glue::glue(
              "Metadata update request for {surveyID} did not produce intended results:"
            ),
            # Give matched list of options to see what didn't work:
            purrr::pmap_chr(
              list(
                names_call_args,
                call_args,
                update_check
              ),
              ~{
                msg <-
                  glue::glue("{..1} - Intended: '{..2}' / Current: '{..3}'")

                # Add a newline if Intended + Current is too long:
                if(nchar(msg) > 80){
                  msg <- stringr::str_replace(msg, "  ", "\n")
                }

                return(msg)
              }
            )

          )
        )
      }

      if(verbose){
        rlang::inform(
          glue::glue(
            "Metadata update for {surveyID} succeeded"
          )
        )
      }
    }

    # Return nothing:
    return(invisible())

  }

#' Get metadata content of survey
#'
#' @param surveyID String. Unique ID for the survey whose metadata you wish to
#'   update.
#'
#' @return Data frame listing survey metadata
#' @export
fetch_survey_metadata <-
  function(
    surveyID
  ) {

    # ARGUMENT CHECKING -----

    check_credentials()
    checkarg_isstring(surveyID, null_okay = FALSE)

    response <-
      .get_survey_metadata(surveyID)

    result <-
      tibble::enframe(
        unlist(
          purrr::map(
            response,
            rlang::`%||%`,
            NA_character_
          )
        )
      )

    return(result)
  }


#' Helper function for supporting both update_ and fetch_ versions
#'
#' @param surveyID String. Unique ID for the survey whose metadata you wish to
#'   update.
#'
#' @keyword internal
#' @return named vector of survey metadata
#'
.get_survey_metadata <-
  function(
    surveyID
  ) {

    # BUILD REQUEST -----

    metadata_url <-
      generate_url(query = "surveydefinitions_metadata",
                   surveyID = surveyID)


    # QUERY API ----

    # Send PUT request to survey-definitions metadata endpoint:
    resp <-
      qualtrics_api_request(
        verb = "GET",
        url = metadata_url,
      )

    return(resp$result)
  }
