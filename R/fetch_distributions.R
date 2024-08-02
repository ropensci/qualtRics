
#' Download distribution data for a survey from Qualtrics
#'
#' @param surveyID String. Unique survey ID for the distribution data you want to download.
#'
#' @template retry-advice
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
#' surveys <- all_surveys()
#' distributions <- fetch_distributions(surveys$id[1])
#'}
#'

fetch_distributions <- function(surveyID){

  # qualtrics distribution parameters can be found at
  # https://api.qualtrics.com/docs/getting-information-about-distributions-1#get-distribution

  check_credentials()
  checkarg_isstring(surveyID)

  fetch_url <- generate_url(query = "fetchdistributions",
                            surveyID = surveyID)

  elements <- paginate_api_request(fetch_url)
  headers <- purrr::map(elements, "headers", .default = NA_character_)
  subjectMessage <- purrr::map(headers, "subjectMessage", .default = NA_character_)
  recipients <- purrr::map(elements, "recipients", .default = NA_character_)
  message <- purrr::map(elements, "message", .default = NA_character_)
  surveyLink <- purrr::map(elements, "surveyLink", .default = NA_character_)
  stats <- purrr::map(elements, "stats", .default = NA_character_)

  x <- tibble::tibble(id = purrr::map_chr(elements, "id", .default = NA_character_),
                      parentDistributionId = purrr::map_chr(elements, "parentDistributionId", .default = NA_character_),
                      ownerId = purrr::map_chr(elements, "ownerId", .default = NA_character_),
                      organizationId = purrr::map_chr(elements, "organizationId", .default = NA_character_),
                      requestStatus = purrr::map_chr(elements, "requestStatus", .default = NA_character_),
                      requestType = purrr::map_chr(elements, "requestType", .default = NA_character_),
                      sendDate = lubridate::ymd_hms(purrr::map_chr(elements, "sendDate", .default = NA_character_)),
                      createdDate = lubridate::ymd_hms(purrr::map_chr(elements, "createdDate", .default = NA_character_)),
                      modifiedDate = lubridate::ymd_hms(purrr::map_chr(elements, "modifiedDate", .default = NA_character_)),
                      customHeaders = purrr::map_chr(elements, "customHeaders", .default = NA_character_),
                      headers_fromEmail = purrr::map_chr(headers, "fromEmail", .default = NA_character_),
                      headers_replyToEmail = purrr::map_chr(headers, "replyToEmail", .default = NA_character_),
                      headers_fromName = purrr::map_chr(headers, "fromName", .default = NA_character_),
                      subjectMessage_messageId = purrr::map_chr(subjectMessage, "messageId", .default = NA_character_),
                      subjectMessage_libraryId = purrr::map_chr(subjectMessage, "libraryId", .default = NA_character_),
                      recipients_mailingListId = purrr::map_chr(recipients, "mailingListId", .default = NA_character_),
                      recipients_contactId = purrr::map_chr(recipients, "contactId", .default = NA_character_),
                      recipients_libraryId = purrr::map_chr(recipients, "libraryId", .default = NA_character_),
                      recipients_sampleId = purrr::map_chr(recipients, "sampleId", .default = NA_character_),
                      message_libraryId = purrr::map_chr(message, "libraryId", .default = NA_character_),
                      message_messageId = purrr::map_chr(message, "messageId", .default = NA_character_),
                      message_messageType = purrr::map_chr(message, "messageType", .default = NA_character_),
                      surveyLink_surveyId = purrr::map_chr(surveyLink, "surveyId", .default = NA_character_),
                      surveyLink_expirationDate = purrr::map_chr(surveyLink, "expirationDate", .default = NA_character_),
                      surveyLink_linkType = purrr::map_chr(surveyLink, "linkType", .default = NA_character_),
                      stats_sent = purrr::map_int(stats, "sent", .default = NA_integer_),
                      stats_failed = purrr::map_int(stats, "failed", .default = NA_integer_),
                      stats_started = purrr::map_int(stats, "started", .default = NA_integer_),
                      stats_bounced = purrr::map_int(stats, "bounced", .default = NA_integer_),
                      stats_opened = purrr::map_int(stats, "opened", .default = NA_integer_),
                      stats_skipped = purrr::map_int(stats, "skipped", .default = NA_integer_),
                      stats_finished = purrr::map_int(stats, "finished", .default = NA_integer_),
                      stats_complaints = purrr::map_int(stats, "complaints", .default = NA_integer_),
                      stats_blocked = purrr::map_int(stats, "blocked", .default = NA_integer_))

  return(x)

}

