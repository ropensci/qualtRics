#' Download complete survey description using the Qualtrics v3 "Get Survey"
#' API endpoint.
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as "id" by the [all_surveys] function.
#' @param elements A character vector. Lists elements of survey definition to be
#' maintained.  Possible elements are "metadata", "surveyoptions", "flow",
#' "blocks", "questions", "responsesets", and/or "scoring" (case-insensitive).
#' If `legacy = TRUE`, then possible elements are "metadata", "questions",
#' "responsecounts", "blocks", "flow", "embedded_data", and/or "comments".
#' @param legacy Logical.  If TRUE, will use older Get Survey API endpoint
#' via a call to legacy function [metadata].
#' @param version A string, or `NULL` (the default). Which version of the
#' survey definition to download. `NULL` returns the current state of the
#' survey in the editor, which may include unpublished changes. A version ID
#' (the `versionID` column of [fetch_survey_versions()]) returns that saved
#' version. The keyword `"published"` returns the version currently published
#' to respondents, found by listing the survey's versions with
#' [fetch_survey_versions()] and taking the one marked `published`. Cannot be
#' combined with `legacy = TRUE`.
#' @param ... Additional options, only used when `legacy = TRUE`. User may pass
#' an argument called `questions`, a vector containing the names of questions
#' for which you want to return metadata.
#'
#' @template retry-advice
#' @return A list containing survey description metadata. The contents of the
#' returned list depend on `elements`.
#'
#' @importFrom purrr map
#' @importFrom purrr pluck<-
#' @importFrom tibble enframe
#' @importFrom dplyr bind_rows
#' @importFrom rlang %||%
#' @export
#'
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
#' # Get description for a survey
#' descrip <- fetch_description(surveyID = surveys$id[6])
#'
#' # Get metadata with specific elements
#' descrip_specific <- fetch_description(
#'   surveyID = id,
#'   elements = c("questions", "flow")
#' )
#'
#' # Get the description of the version currently published to respondents
#' descrip_published <- fetch_description(
#'   surveyID = surveys$id[6],
#'   version = "published"
#' )
#'
#' }
fetch_description <-
  function(surveyID,
           elements = NULL,
           legacy = FALSE,
           version = NULL,
           ...
  ) {

    # Check version (before possibly reverting to legacy, which ignores it):
    checkarg_version(version, legacy)

    # Revert to old metadata if desired:
    if(legacy){
      return(metadata(surveyID = surveyID,
                      get = elements,
                      ...)
      )
    }

    # OPTIONS AND PREP ----

    # Check params
    check_credentials()
    checkarg_isstring(surveyID)
    checkarg_isboolean(legacy)

    # Check and format elements argument:
    elements <-
      checkarg_elements(elements)


    # QUERY API ----

    # Resolve the "published" keyword to the published version's ID:
    if(identical(version, "published")){
      version <- fetch_published_version_id(surveyID)
    }

    # Function-specific API stuff
    if(is.null(version)){
      description_url <-
        generate_url(query = "fetchdescription",
                     surveyID = surveyID)
    } else {
      description_url <-
        generate_url(query = "fetchdescription_version",
                     surveyID = surveyID,
                     versionID = version)
    }

    # Send GET request to survey-definitions endpoint:
    resp <-
      qualtrics_api_request("GET", description_url)

    # Extract result
    result <-
      resp$result



    # PROCESS RESULTS ----

    # METADATA

    # Mark the elements that are a part of metadata:
    metadata_names <-
      c(
        "BrandID",
        "BrandBaseURL",
        "SurveyName",
        "SurveyStatus",
        "SurveyID",
        "OwnerID",
        "CreatorID",
        "LastModified",
        "LastAccessed",
        "LastActivated",
        "QuestionCount"
      )
    # Convert NULLs to NAs and make into tidy dataframe:
    metadata <-
      tibble::enframe(
        unlist(
          purrr::map(result[metadata_names],
                     rlang::`%||%`,
                     NA_character_)
        ))

    # Remove redundant SurveyID from ProjectInfo:
    pluck(result, "ProjectInfo", "SurveyID") <- NULL

    # Convert NULLs to NAs and make into tidy dataframe:
    projectinfo <-
      tibble::enframe(
        unlist(
          purrr::map(result[["ProjectInfo"]],
                     rlang::`%||%`,
                     NA_character_)
        ))

    # Combine to make a complete metadata output:
    metadata <-
      bind_rows(
        metadata,
        projectinfo
      )

    # SURVEY OPTIONS

    # Convert NULLs to NAs and make into tidy dataframe:
    surveyoptions <-
      tibble::enframe(
        unlist(
          purrr::map(result$SurveyOptions,
                     rlang::`%||%`,
                     NA_character_)
        ))

    # FINAL CLEANING AND OUTPUT:

    # Taking other items as-is, combine into a simplified list and output
    output <-
      list(
        metadata = metadata,
        surveyoptions = surveyoptions,
        flow = result$SurveyFlow,
        blocks = result$Blocks,
        questions = result$Questions,
        responsesets = result$ResponseSets,
        scoring = result$Scoring
      )

    # Keep only those elements desired:
    output <-
      output[names(output) %in% elements]

    return(output)
  }

#' Find the version ID of the currently published version of a survey
#'
#' @param surveyID A string. Unique ID for the survey.
#' @return The `versionID` (a string) of the version with `published == TRUE`.
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
fetch_published_version_id <-
  function(surveyID){

    versions <-
      fetch_survey_versions(surveyID)

    published_id <-
      versions$versionID[versions$published %in% TRUE]

    if(length(published_id) == 0){
      rlang::abort(
        c(glue::glue("Survey {surveyID} has no published version."),
          i = "Publish the survey in Qualtrics, or set `version = NULL` to fetch the current (unpublished) survey.")
      )
    }

    if(length(published_id) > 1){
      rlang::abort(
        c(glue::glue("Qualtrics API reported more than one published version for survey {surveyID}:"),
          glue::glue("Version IDs: {paste(published_id, collapse = ', ')}"),
          i = "Pass one of these IDs to `version` directly.")
      )
    }

    return(published_id)
  }
