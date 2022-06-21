#' Update metadata options
#'
#' @param surveyID String. Unique ID for the survey whose metadata you wish to
#'   update.
#' @param name String. Set survey name.  Defaults to NULL, which leaves existing
#'   name.
#' @param description String. Set survey description. Defaults to `NULL`, which
#'   leaves existing description.  Entering `NA` will remove any existing
#'   description.
#' @param active Boolean. Set survey activity status (whether open for
#'   collection).  Defaults to `NULL`, which leaves status unchanged.
#' @param active_start_date POSIXlt, POSIXct, Date, or string equivalent.  Sets
#'   date/time for survey becoming active.  See Details.
#' @param active_end_date POSIXlt, POSIXct, Date, or string equivalent.  Sets
#'   date/time for survey becoming inactive  See Details.
#' @param time_zone String. Desired time zone to use when `active_start_date` or
#'   `active_end_date` are given as Date or string objects.  Ignored if argument(s)
#'   given as POSIXlt/POSIXct objects. Must match a time zone name from
#'   [base::OlsonNames()]. Defaults to `NULL`, which uses the current
#'   system timezone ([base::Sys.timezone()]).
#'
#' @export
update_survey_metadata <-
  function(
    surveyID,
    name = NULL,
    description = NULL,
    active = NULL,
    active_start_date = NULL,
    active_end_date = NULL,
    time_zone = NULL
  ) {

    # ARGUMENT CHECKING -----

    check_credentials()
    checkarg_isstring(surveyID, null_okay = FALSE)
    checkarg_isstring(name)
    checkarg_isstring(description, na_okay = TRUE)

    active_formatted <-
      checkarg_survey_active(active)
    active_start_date_formatted <-
      checkarg_datetime(active_start_date, time_zone)
    active_end_date_formatted <-
      checkarg_datetime(active_end_date, time_zone)

    # BUILD REQUEST -----

    metadata_url <-
      generate_url(query = "surveydefinitions_metadata",
                   surveyID = surveyID)

    update_metadata_body <-
      create_raw_payload(
        SurveyName = name,
        SurveyDescription = description,
        SurveyStatus = active_formatted,
        SurveyStartDate = active_start_date_formatted,
        SurveyExpirationDate = active_end_date_formatted,
        .NAtoNULL = c("SurveyDescription")
      )

    # QUERY API ----



    # Send PUT request to survey-definitions metadata endpoint:
    qualtrics_api_request(
      verb = "PUT",
      url = metadata_url,
      body = update_metadata_body
    )

    invisible()
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

    result <-
      tibble::enframe(
        unlist(
          purrr::map(resp$result,
                     rlang::`%||%`,
                     NA_character_)
        )
      )

    return(result)
  }
