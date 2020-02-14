#' Download metadata for a survey
#'
#' Using this function, you can retrieve metadata about your survey. This
#' information includes question metadata (type, options, choices, etc), number
#' of responses, general metadata, survey flow, etc.
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#' Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param get A list containing `TRUE`/`FALSE` values of one of the following:
#' `metadata`, `questions`, `responsecounts`, `blocks`, `flow`, `embedded_data`,
#' or `comments`. A `TRUE` value will return that specific element. If you leave
#' this empty, the function will return the `metadata`, `questions`, and
#' `responsecounts` elements. See examples below for more information.
#' @param ... Additional options. User may pass an argument called `questions`,
#' which should be a vector containing the names of questions for which you
#' want to return metadata.
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
#'   get = list(questions = FALSE, flow = TRUE)
#' )
#'
#' # Get specific question metadata
#' question_specific <- metadata(
#'   surveyID = id,
#'   get = list(questions = TRUE),
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
                     get = list(),
                     ...) {

  # OPTIONS AND PREP ----

  # Check params
  assert_base_url()
  assert_api_key()

  # Check if illegal options were passed by user
  allowed <- c(
    "metadata", "questions", "responsecounts",
    "blocks", "flow", "embedded_data", "comments"
  )
  check <- union(allowed, names(get))
  assertthat::assert_that(length(check) <= length(allowed), # nolint
    msg = "One or more options you passed to 'get' are not valid. Please check your\ninput and try again."
  ) # nolint

  # Change standard options if any
  standard_list <- list(
    "metadata" = TRUE,
    "questions" = TRUE,
    "responsecounts" = TRUE,
    "blocks" = FALSE,
    "flow" = FALSE,
    "embedded_data" = FALSE,
    "comments" = FALSE
  )
  # Cycle over each argument and change
  if (length(get) > 0) {
    for (g in names(get)) {
      standard_list[[g]] <- get[[g]]
    }
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
      warning(paste0("One or more questions you queried are not present in your survey.\nReturning all questions instead.")) # nolint
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
  met_ss <- met[names(standard_list[vapply(
    standard_list,
    function(x) x == TRUE, TRUE
  )])] # nolint

  # RETURN ----

  return(met_ss)
}
