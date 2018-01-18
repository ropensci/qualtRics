#   Download qualtrics data into R
#    Copyright (C) 2018 Jasper Ginn

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Download metadata about a survey
#'
#' Using this function, you can retrieve metadata about your survey. This information includes question metadata (type, options, choices etc), number of responses, general metadata, survey flow etc.
#'
#' @param surveyID String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.
#' @param get list containing TRUE/FALSE values of one of the following: metadata, questions,responsecounts, blocks, flow, embedded_data or comments. A TRUE value will return that specific element. If you leave this empty, the function will return the metadata, questions and responsecounts elements. See examples below for more information
#' @param ... additional options. User may pass an argument called 'questions', which should be a vector containing the names of questions for which you want to return metadata.
#'
#' @author Jasper Ginn
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' # Note that you need to pass both the 'api_token' and 'root_url'
#' # parameters if you call this function for the first time.
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
#' # Get metadata for a survey
#' md <- metadata(surveyID = id)
#' # Get metadata with specific elements
#' md_specific <- metadata(surveyID= id, get=list(questions=FALSE, flow=TRUE))
#' # Get specific question metadata
#' question_specific <- metadata(surveyID=id, get=list(questions=TRUE), questions=c("Q1","Q2"))
#' }
#'

metadata <- function(surveyID,
                     get = list(),
                     ...) {
  # Check params
  cp <- checkParams()
  # Check if illegal options were passed by user
  allowed <- c("metadata","questions","responsecounts",
               "blocks","flow","embedded_data","comments")
  check <- union(allowed, names(get))
  assertthat::assert_that(length(check) <= length(allowed),
                          msg="One or more options you passed to 'get' are not valid. Please check your input and try again.")
  # Change standard options if any
  standard_list <- list("metadata"=TRUE,
                        "questions"=TRUE,
                        "responsecounts"=TRUE,
                        "blocks"=FALSE,
                        "flow"=FALSE,
                        "embedded_data"=FALSE,
                        "comments"=FALSE)
  if(length(get) > 0) {
    for(g in names(get)) {
      standard_list[[g]] <- get[[g]]

    }
  }
  # Other options
  opts <- list(...)
  if("questions" %in% names(opts)) {
    # Check that is a vector
    assertthat::assert_that(is.vector(opts$questions), msg="'questions' argument must be a vector")
    q_select <- opts$questions
  } else {
    q_select <- NULL
  }

  ### Get metadata

  # Function-specific API stuff
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "surveys")
  # Append survey ID
  root_url <- paste0(root_url, surveyID)
  # Send GET request to list all surveys
  resp <- qualtricsApiRequest("GET", root_url)
  # Filter
  resp_filt <- resp$result

  ### Reshape

  # Metadata
  metadata <- data.frame(
    "id" = resp_filt$id,
    "name"= resp_filt$name,
    "ownerId" = resp_filt$ownerId,
    "organizationId"=resp_filt$organizationId,
    "isActive" = resp_filt$isActive,
    "creationDate" = resp_filt$creationDate,
    "lastModifiedDate"=resp_filt$lastModifiedDate,
    "expiration_startdate"=ifelse(is.null(resp_filt$expiration$startDate),NA,resp_filt$expiration$startDate),
    "expiration_endDate"=ifelse(is.null(resp_filt$expiration$endDate),NA,resp_filt$expiration$endDate)
  )
  # Response counts
  responsecounts <- data.frame(
    "auditable"=resp_filt$responseCounts$auditable,
    "generated"=resp_filt$responseCounts$generated,
    "deleted"=resp_filt$responseCounts$deleted
  )
  # Questions
  if(!is.null(q_select)) {
    qnames <- sapply(resp_filt$questions, function(x) {
      x$questionName
    })
    if(all(q_select %in% qnames)) {
      questions <- resp_filt$questions[which(qnames %in% q_select)]
    } else {
      warning(paste0("One or more questions you queried are not present in your survey. Returning all questions instead."))
      questions <- resp_filt$questions
    }
  } else {
    questions <- resp_filt$questions
  }
  # Construct metadata
  met <- list("metadata"=metadata,
              "questions"=questions,
              "responsecounts"=responsecounts,
              "blocks"=resp_filt$blocks,
              "flow"=resp_filt$flow,
              "embedded_data"=resp_filt$embeddedData,
              "comments"=resp_filt$comments)
  # Make subset
  met_ss <- met[names(get[vapply(get,function(x) x==TRUE, TRUE)])]
  # Return
  return(met_ss)
}
