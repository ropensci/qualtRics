# utils.R contains helper functions for the qualtRics package. These functions should not be called directly by the user and should not be exported.


# Constructing/making/checking API requests ---------------------------



#' Checks responses against Qualtrics response codes and returns error message.
#'
#' @param res results object from httr
#' @keywords internal

qualtrics_response_codes <-
  function(res){

    # Exit if fine:
    if(res$status_code == 200){
      return()
    }

    #  Get error message
    error_message <-
      switch(
        as.character(res$status_code),
        `401` =
          c("Qualtrics API reported an authentication error (401):",
            "You may not have the required authorization",
            "Please check your API key and base URL."),
        `403` =
          c("Qualtrics API reported an forbidden error (403):",
            "You may have a valid API key that lacks API query permissions",
            "Please check your settings and/or talk to your administrators."),
        `400` =
          c("Qualtrics API reported a bad request error (400):",
            "Please report this on https://github.com/ropensci/qualtRics/issues"),
        `404` =
          c("Qualtrics API reported a not found error (404):",
            "Please check if you are using the correct survey ID."),
        `413` =
          c("Qualtrics API reported a 413 error:",
            "The request body was likely too large.",
            "Can also occur when a multipart/form-data request is malformed."),
        `429` =
          c("Qualtrics API reported a 429 error:",
            "You have reached the concurrent request limit."),
        `500` =
          c("After 4 attempts, Qualtrics API reported a temporary internal server error (500):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        `503` =
          c("After 4 attempts, Qualtrics API reported a temporary internal server error (503):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        `504` =
          c("After 4 attempts, Qualtrics API reported a gateway timeout error (504):",
            "Please contact Qualtrics Support or retry your query",
            glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
            glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")),
        # Default response for unknown status code:
        c(glue::glue("Qualtrics API reported the atypical status code {res$status_code}"),
          "A dictionary of status codes can be found here: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status",
          "Please check your request, and report at https://github.com/ropensci/qualtRics/issues if reoccurring:")
      )

    # Report the error message:
    rlang::abort(error_message)

  }


#' Construct a header to send to Qualtrics API
#'
#' @param API_TOKEN API token. Available in your Qualtrics account (see:
#'   <https://api.qualtrics.com/>)
#' @keywords internal

construct_header <-
  function(API_TOKEN) {

    # Check again that API token is properly formatted:
    checkarg_isstring(API_TOKEN)

    # Construct and return
    headers <- c(
      "X-API-TOKEN" = API_TOKEN,
      "Content-Type" = "application/json",
      "Accept" = "*/*",
      "accept-encoding" = "gzip, deflate"
    )
    return(headers)
  }

#' Check if httr GET result contains a warning
#'
#' @param resp object returned by [qualtrics_response_codes()]
#' @importFrom purrr pluck
#' @keywords internal

check_for_warnings <-
  function(resp) {
    # Raise warning if resp contains notice
    notice <-
      purrr::pluck(resp, "content", "meta", "notice")
    if (!is.null(notice)) {
      warning(notice)
    }
    return(NULL)
  }

#' Generate URL for specific API query by type and (if appropriate) ID
#'
#' @param query string.  The specific API query desired.  Generally named the
#'   same as associated functions but without underscores, so the request for
#'   `fetch_survey()` would be be "fetchsurvey".
#' @param ... Named elements of URL for specific query desired, such as
#'   `surveyID` or `mailinglistID`
#'
#' @importFrom glue glue
#'
#' @return Endpoint URL to be passed to querying tools
#' @keywords internal
#' @export
generate_url <-
  function(query, ...){

    args <- list(...)
    list2env(args, envir = environment())

    # Get the user's specific base URL from environment
    # (and check it again in case the user has modified it externally somehow):
    base_url <-
      checkarg_base_url(
        Sys.getenv("QUALTRICS_BASE_URL")
      )
    # Construct URL root for the v3 api endpoint:
    root_url <- glue_api_v3(base_url)

    # List of templates for how to build URLs
    # (add to this when new functions made):
    endpoint_template <-
      switch(
        query,
        allsurveys = "{rooturl}/surveys",
        allmailinglists = "{rooturl}/mailinglists/",
        metadata = "{rooturl}/surveys/{surveyID}/",
        exportresponses = "{rooturl}/surveys/{surveyID}/export-responses/",
        exportresponses_progress = "{rooturl}/surveys/{surveyID}/export-responses/{requestID}",
        exportresponses_file = "{rooturl}/surveys/{surveyID}/export-responses/{fileID}/file",
        fetchdescription = "{rooturl}/survey-definitions/{surveyID}/",
        fetchmailinglist = "{rooturl}/mailinglists/{mailinglistID}/contacts/",
        fetchdistributions = "{rooturl}/distributions?surveyId={surveyID}",
        fetchdistributionhistory = "{rooturl}/distributions/{distributionID}/history",
        listdistributionlinks = "{rooturl}/distributions/{distributionID}/links?surveyId={surveyID}",
        rlang::abort("Internal error: invalid URL generation query")
      )

    # Construct the actual URL:
    glue::glue(endpoint_template, rooturl = root_url, ...)

  }

glue_api_v3 <- function(base_url) {
  glue::glue("https://{base_url}/API/v3")
}

#' Create properly-formatted JSON payload for API calls.  Removes NULLS
#'
#'
#' @importFrom jsonlite toJSON
#' @importFrom purrr map
#'
#' @return JSON file with options to send to API
#' @keywords internal

create_raw_payload <-
  function(...) {

    # Make list of params, dropping NULL's:
    params <-
      purrr::discard(
        list(...),
        is.null
        )

    # Selectively mark length-1 parameters for unboxing, following the API scheme:
    params_ub <-
      purrr::map_if(
        params,
        # The element is a length-1 entry:
        purrr::map_lgl(params, ~length(.x) == 1) &
        # It's not one of these must-box arguments:
        # (can add other names if function used for future features)
        !names(params) %in%
          c("questionIds", "embeddedDataIds", "surveyMetadataIds"),
        ~jsonlite::unbox(.x)
      )

    # convert to JSON payload:
    payload <-
      jsonlite::toJSON(
        params_ub,
        auto_unbox = FALSE
      )

    return(payload)
  }


#' Send httr requests to Qualtrics API
#'
#' @param verb Type of request to be sent (@seealso [httr::VERB()])
#' @param url Qualtrics endpoint URL created by [generate_url()] functions
#' @param query Optional query parameters used by some endpoints
#' @param body Options created by [create_raw_payload()] function
#' @param as type of content to return, passed to `as` in httr::content().
#' current options "parsed" (since we get JSON mostly), "raw" (response .zips)
#' @param ... arguments passed to httr::content when parsing
#' @template retry-advice
#' @keywords internal

qualtrics_api_request <-
  function(verb = c("GET", "POST"),
           url = url,
           query = NULL,
           body = NULL,
           as = c("parsed", "raw"),
           ...
           ) {
    # Match args
    verb <- rlang::arg_match(verb)
    as <- rlang::arg_match(as)
    # Construct header
    headers <-
      construct_header(
        Sys.getenv("QUALTRICS_API_KEY")
      )

    # Prepare list of args:
    arglist <-
      list(
        verb = verb,
        url = url,
        config = httr::add_headers(headers),
        body = body,
        times = 4,
        terminate_on = 400:451,
        quiet = TRUE
      )

    # if needed, add query arg in proper place:
    # (adding query = NULL directly breaks some requests)
    if(!is.null(query)){
      arglist <-
        append(x = arglist,
               values = list(query = query),
               after = 3
        )
    }

    # Send request to Qualtrics API
    res <-
      do.call(httr::RETRY, arglist)

    # Check if response type is OK
    qualtrics_response_codes(res)

    # Get content out:
    cnt <-
      httr::content(
        x = res,
        as = as,
        ...
      )

    if(as == "parsed"){
    # If notice occurs, raise warning
    check_for_warnings(cnt)
    }

    # return content
    return(cnt)
  }

paginate_api_request <- function(fetch_url) {
  elements <- list()

  while(!is.null(fetch_url)){
    res <- qualtrics_api_request("GET", url = fetch_url)
    elements <- append(elements, res$result$elements)
    # check for "string" placeholder from mock server:
    if (res$result$nextPage == "string") {
      fetch_url <- NULL
    } else {
      fetch_url <- res$result$nextPage
    }    
  }
  
  elements
}

#' Set proper data types on survey data.
#'
#' @param data Imported Qualtrics survey
#' @param surveyID ID of survey
#' @param verbose Flag
#'
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @keywords internal


# Amending downloaded responses -------------------------------------------



infer_data_types <- function(data,
                             surveyID,
                             verbose = FALSE) {

  # Download survey metadata
  md <- md <- tibble::enframe(metadata(surveyID, get = "questions")[[1]])

  # Check which questions are of allowed types
  md_parsed <- dplyr::mutate(md,
                             question_type = map(value, "questionType"),
                             question_name = map_chr(value, "questionName"),
                             type_supp = map_chr(question_type, "type"),
                             selector_supp = map_chr(question_type, "selector"),
                             type_supp = type_supp %in% c("MC"),
                             selector_supp = selector_supp %in% c("SAVR"),
                             name_in_survey = question_name %in% names(data),
                             supported = type_supp & selector_supp & name_in_survey)

  mc <- dplyr::pull(dplyr::filter(md_parsed, supported), name)

  # Conversion process (next) removes labels, so get them first to keep
  lab <- sjlabelled::get_label(data)

  # For each question we have appropriate metadata for, convert type
  for (m in mc) {
    question_meta <- dplyr::pull(dplyr::filter(md, name == m), value)[[1]]
    data <- wrapper_mc(data, question_meta)
  }

  # Put labels back on
  data <- sjlabelled::set_label(data, lab)

  # Check if warning given
  if (Sys.getenv("QUALTRICS_WARNING_DATE_GIVEN") == "") {
    rlang::inform(
      c("'StartDate', 'EndDate', and 'RecordedDate' were converted without a specific timezone",
        "To set a timezone, visit https://www.qualtrics.com/support/survey-platform/managing-your-account/",
        "Timezone information is under 'User Settings'",
        "See https://api.qualtrics.com/instructions/docs/Instructions/dates-and-times.md for more")
    )
    Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN" = TRUE)
  }
  # Return data
  return(data)
}

#' Convert multiple choice questions to ordered factors
#'
#' @param data Imported Qualtrics survey
#' @param question_meta Question metadata
#'
#' @importFrom rlang ':='
#' @keywords internal

wrapper_mc <- function(data, question_meta) {
  # TODO: add ORDER = TRUE/FALSE if user wants factors to be ordered
  # TODO: add REVERSE = TRUE/FALSE if user wants the factor levels to be reversed

  # Get question details from metadata
  col_name <- rlang::sym(question_meta$questionName)
  meta <- tibble::enframe(question_meta$choices)

  # Level names
ln <-
  dplyr::pull(
    dplyr::mutate(meta,
      meta_levels = ifelse(
        purrr::map(value, function(x) is.null(x$variableName)),
        purrr::map_chr(value, "choiceText"),
        purrr::map_chr(value, "variableName")
      )
    ),
    meta_levels
  )

  ln <- remove_html(ln)

  # Convert
  dplyr::mutate(
    data,
    !!col_name := as.character(!!col_name),
    !!col_name := readr::parse_factor(!!col_name,
                                      levels = ln,
                                      ordered = TRUE,
                                      include_na = FALSE)
  )
}

## simple HTML stripping
remove_html <- function(string) {
  stringr::str_remove_all(string, '<[^>]+>')
}



