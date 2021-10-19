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
#' @param ... Additional options, only used when `legacy = TRUE`. User may pass
#' an argument called `questions`, a vector containing the names of questions
#' for which you want to return metadata.
#'
#' @return A list containing survey description metadata. The contents of the
#' returned list depend on `elements`.
#'
#' @importFrom purrr map
#' @importFrom purrr pluck<-
#' @importFrom tibble enframe
#' @importFrom dplyr bind_rows
#' @importFrom rlang %||%
#' @importFrom assertthat assert_that
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
#' }
fetch_description <-
  function(surveyID,
           elements = NULL,
           legacy = FALSE,
           ...
  ) {

    # Revert to old metadata if desired:
    if(legacy){
      return(metadata(surveyID = surveyID,
                      get = elements,
                      ...)
      )
    }

    # OPTIONS AND PREP ----

    # Check params
    assert_base_url()
    assert_api_key()

    if (is.null(elements)) {
      elements <-
        c(
          "metadata",
          "surveyoptions",
          "flow",
          "blocks",
          "questions",
          "responsesets",
          "scoring"
        )

    } else {
      # case insensitivity:
      elements <- tolower(elements)

      # Check if bad elements were passed by user:
      allowed <-
        c(
          "metadata",
          "surveyoptions",
          "flow",
          "blocks",
          "questions",
          "responsesets",
          "scoring"
        )

      assertthat::assert_that(
        all(tolower(elements) %in% allowed),
        msg = "One or more entries in 'elements' are not valid. Please check your\ninput and try again."
      )
    }


    # QUERY API ----

    # Function-specific API stuff
    description_url <-
      generate_url(query = "fetchdescription",
                   surveyID = surveyID)

    # Send GET request to survey-definitions endpoint:
    resp <- qualtrics_api_request("GET", description_url)

    # Extract result
    result <- resp$result



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
    output <- output[names(output) %in% elements]

    return(output)
  }
