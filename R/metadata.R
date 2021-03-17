#' Download metadata for a survey
#'
#' Using this function, you can retrieve metadata about your survey. This
#' information includes question metadata (type, options, choices, etc), number
#' of responses, general metadata, survey flow, etc.
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as "id" by the [all_surveys] function.
#' @param get A character vector containing any of the following: "metadata",
#' "questions", "responsecounts", "blocks", "flow", "embedded_data",
#' or "comments". Will return included elements. By default, the function
#' returns the "metadata", "questions", and "responsecounts" elements.
#' See examples below for more information.
#' @param ... Additional options. User may pass an argument called `questions`,
#' a vector containing the names of questions for which you want to
#' return metadata.
#'
#' @importFrom assertthat assert_that
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
#' # Get metadata for a survey
#' md <- metadata(surveyID = surveys$id[6])
#'
#' # Get metadata with specific elements
#' md_specific <- metadata(
#'   surveyID = id,
#'   get = c("flow")
#' )
#'
#' # Get specific question metadata
#' question_specific <- metadata(
#'   surveyID = id,
#'   get = c("questions"),
#'   questions = c("Q1", "Q2")
#' )
#'
#' # Example of a metadata file
#' file <- system.file("extdata", "metadata.rds", package = "qualtRics")
#'
#' # Load
#' metadata_ex <- readRDS(file = file)
#' }
#'
metadata <- function(surveyID,
                     get = NULL,
                     ...) {

  # OPTIONS AND PREP ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Deal with old list format for get parameter:
  if(is.list(get)){
    message("Use of logical lists for argument 'get' has been deprecated.
    In future, use a character vector including desired elements")

    # Pull out the TRUE elements of the list:
    get_true <- names(get)[unlist(get)]
    # Pull out the FALSE elements of the list:
    get_false <- names(get)[!unlist(get)]

    # Restore old behavior when using lists (metadata, questions, responsecounts
    # included unless specifically specified as FALSE):
    get <-
      setdiff(
        union(c("metadata", "questions", "responsecounts"),
                 get_true),
        get_false)

  }

  if(is.null(get)){

    # Set default options for argument 'get'
    get <- c("metadata", "questions", "responsecounts")

  } else {

    # Check if illegal options were passed to get by user
    allowed <- c(
      "metadata", "questions", "responsecounts",
      "blocks", "flow", "embedded_data", "comments"
    )

    # Make case insensitive:
    get <- tolower(get)

    assertthat::assert_that(
      all(get %in% allowed), # nolint
      msg = "One or more entries in 'get' are not valid. Please check your\ninput and try again."
    ) # nolint

  }
  # Other options
  opts <- list(...)
  if ("questions" %in% names(opts)) {
    # Check that is a vector
    assertthat::assert_that(is.vector(opts$questions),
                            msg = "'questions' argument must be a vector"
    )
    q_select <- opts$questions
  } else {
    q_select <- NULL
  }

  # QUERY API ----

  # Function-specific API stuff
  survey_url <- create_survey_url(Sys.getenv("QUALTRICS_BASE_URL"), surveyID)

  # Send GET request to specific survey
  resp <- qualtrics_api_request("GET", survey_url)

  # Filter
  resp_filt <- resp$result

  # RESHAPE DATA ----

  # Metadata
  metadata <- data.frame(
    "surveyID" = resp_filt$id,
    "name" = resp_filt$name,
    "ownerId" = resp_filt$ownerId,
    "organizationId" = resp_filt$organizationId,
    "isActive" = resp_filt$isActive,
    "creationDate" = resp_filt$creationDate,
    "lastModifiedDate" = resp_filt$lastModifiedDate,
    "expiration_startdate" = ifelse(is.null(resp_filt$expiration$startDate),
                                    NA,
                                    resp_filt$expiration$startDate
    ),
    "expiration_endDate" = ifelse(is.null(resp_filt$expiration$endDate),
                                  NA,
                                  resp_filt$expiration$endDate
    )
  )
  # Response counts
  responsecounts <- data.frame(
    "auditable" = resp_filt$responseCounts$auditable,
    "generated" = resp_filt$responseCounts$generated,
    "deleted" = resp_filt$responseCounts$deleted
  )

  # Metadata about questions
  if (!is.null(q_select)) {
    qnames <- vapply(resp_filt$questions, function(x) {
      x$questionName
    }, "")
    if (all(q_select %in% qnames)) {
      questions <- resp_filt$questions[which(qnames %in% q_select)]
    } else {
      rlang::warn(
        paste0("One or more questions you queried are not present in your survey.",
               "\nReturning all questions instead.")
      ) # nolint
      questions <- resp_filt$questions
    }
  } else {
    questions <- resp_filt$questions
  }

  # WRAP UP AND RETURN ----

  # Construct metadata
  met <- list(
    "metadata" = metadata,
    "questions" = questions,
    "responsecounts" = responsecounts,
    "blocks" = resp_filt$blocks,
    "flow" = resp_filt$flow,
    "embedded_data" = resp_filt$embeddedData,
    "comments" = resp_filt$comments
  )
  # Make subset
  met_ss <- met[names(met) %in% get]

  # RETURN ----

  return(met_ss)
}
