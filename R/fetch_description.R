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
#' surveys <-
#'   all_surveys()
#'
#' # Get description for a survey
#' descrip <-
#'   fetch_description(surveyID = surveys$id[6])
#'
#' # Get metadata with specific elements
#' descrip_specific <-
#'   fetch_description(
#'     surveyID = id,
#'     elements = c("questions", "flow")
#'   )
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
    check_credentials()
    checkarg_isstring(surveyID)
    checkarg_isboolean(legacy)

    # Check and format elements argument:
    elements <-
      checkarg_elements(elements)


    # QUERY API ----

    # Function-specific API stuff
    description_url <-
      generate_url(query = "fetchdescription",
                   surveyID = surveyID)

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



#'Download (and potentially save) a description of a survey in Qualtrics Survey
#'Format (QSF)
#'
#'@param surveyID String. Unique ID for the survey you want to download.
#'  Returned as "id" by the [all_surveys] function.
#'@param formatting String. Adjustments to the downloaded QSF JSON to be either
#'  more human-readable or more compact.  Options are "none" (default, no
#'  adjustment), "pretty" (more human-readable, using
#'  \code{\link[jsonlite]{prettify}}), or "compact" (more compact, using
#'  \code{\link[jsonlite]{minify}})
#'@param save Boolean. Should downloaded QSF be output to R as a json object
#'  (\code{FALSE}, default), or to a file (\code{TRUE})? Even when TRUE,
#'  function still invisibly returns results.
#'@param file String. File to write to (including path if needed).  Typically,
#'  would end in .qsf, though not required.  Ignored if \code{save == FALSE}.
#'
#'
#'@template retry-advice
#'@return A JSON object equivalent of a Qualtrics QSF.
#'
#'@importFrom jsonlite toJSON
#'@importFrom jsonlite prettify
#'@importFrom jsonlite minify
#'@importFrom rlang arg_match
#'
#'
#'@export
#'
fetch_qsf <-
  function(surveyID,
           formatting = c("none", "pretty", "compact"),
           save = FALSE,
           file = NULL
  ) {

    # OPTIONS AND PREP ----

    # Check params
    check_credentials()
    checkarg_isstring(surveyID)
    formatting <- rlang::arg_match(formatting)
    checkarg_isboolean(save)
    if(save){
      checkarg_isstring(file)
    }

    # QUERY API ----

    # Function-specific API stuff
    description_url <-
      generate_url(
        query = "fetchdescription",
        surveyID = surveyID
      )

    # Send GET request to survey-definitions endpoint:
    resp <-
      qualtrics_api_request(
        verb = "GET",
        url = description_url,
        query = list(format = "qsf"),
        as = "parsed"
      )

    result <-
      resp[["result"]]

    qsf <-
      toJSON(
        x = result,
        auto_unbox = TRUE,
        digits = NA,
        null = "null"
      )

    # Format result as requested for output/saving:
    if(formatting == "pretty"){
      qsf <-
        jsonlite::prettify(txt = qsf)
    } else if (formatting == "compact") {
      qsf <-
        jsonlite::minify(txt = qsf)
    }

    if(save){
      # Make connection to specified file and save:
      connection <-
        file(file)
      writeLines(text = qsf, con = connection)
      close(connection)

      # Return results invisibly:
      return(invisible(qsf))
    }

    # Return standard output:
    return(qsf)
  }
