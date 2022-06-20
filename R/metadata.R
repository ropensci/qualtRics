#' Download metadata for a survey
#'
#' Using this function, you can retrieve metadata about your survey. This
#' information includes question metadata (type, options, choices, etc), number
#' of responses, general metadata, survey flow, etc.
#'
#' @param surveyID A string. Unique ID for the survey you want to download.
#'   Returned as "id" by the [all_surveys] function.
#' @param get A character vector containing any of the following: "metadata",
#'   "questions", "responsecounts", "blocks", "flow", "embedded_data", or
#'   "comments". Will return included elements. By default, the function returns
#'   the "metadata", "questions", and "responsecounts" elements. See examples
#'   below for more information.
#' @param questions Character vector containing the names of questions for which
#'   you want to return metadata.  Defaults to NULL (all questions).
#'
#' @template retry-advice
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
                     questions = NULL) {

  # OPTIONS AND PREP ----

  # Check params
  check_credentials()
  checkarg_isstring(surveyID)

  get <- checkarg_get(get)
  checkarg_ischaracter(questions)

  # QUERY API ----

  # Function-specific API stuff
  survey_url <- generate_url(query = "metadata",
                             surveyID = surveyID)

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
  if (!is.null(questions)) {
    qnames <- vapply(resp_filt$questions, function(x) {
      x$questionName
    }, "")
    if (all(questions %in% qnames)) {
      qs_toreturn <-
        resp_filt$questions[which(qnames %in% questions)]
    } else {
      rlang::warn(
        paste0("One or more questions you queried are not present in your survey.",
               "\nReturning all questions instead.")
      ) # nolint
      qs_toreturn <- resp_filt$questions
    }
  } else {
    qs_toreturn <- resp_filt$questions
  }

  # WRAP UP AND RETURN ----

  # Construct metadata
  met <- list(
    "metadata" = metadata,
    "questions" = qs_toreturn,
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
