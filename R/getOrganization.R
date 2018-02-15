#' Retrieve a data frame of general information about an organization
#'
#' @seealso See \url{https://api.qualtrics.com/docs} for documentation on the Qualtrics API.
#'
#' @param organizationId This is the Qualtrics assigned Organization Id available from the Qualtrics IDs console.
#'
#' @importFrom dplyr bind_cols rename_at
#' @importFrom purrr map_df
#' @importFrom stringr str_to_title
#' @return Returns a dataframe with general information about an organization.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
#' # Retrieve a data frame with organizational details
#' org <- getOrganization("organizationId")}
getOrganization <- function(organizationId) {
  # Check params
  cp <- checkParams()
  # Function-specific API stuff
  root_url <- appendRootUrl(Sys.getenv("QUALTRICS_ROOT_URL"), "organizations")
  # Add OrganizationId as url parameter
  root_url <- paste0(root_url,"/", organizationId)
  # Send GET request to list organization details
  resp <- qualtricsApiRequest("GET", root_url)
  # Flatten response to dataframe
  resp <- resp$result
  # Build data frame
  org <- tibble(
    id = resp$id,
    name = resp$name,
    baseUrl = resp$baseUrl,
    type = resp$type,
    status = resp$status,
    creationDate = resp$creationDate,
    expirationDate = ifelse(is.null(resp$expirationDate), NA, resp$expirationDate)) %>%
    # Add stats columns
    ## responseCounts
    dplyr::bind_cols(purrr::map_df(resp, "responseCounts") %>% dplyr::rename_at(vars(auditable:deleted), funs(paste0("responseCounts", stringr::str_to_title(.))))) %>%
    ## surveyCounts
    dplyr::bind_cols(purrr::map_df(resp, "surveyCounts") %>% dplyr::rename_at(vars(totalSurveys:activeSurveys), funs(paste0("surveyCounts",stringr::str_to_title(.))))) %>%
    ## loginActivity
    dplyr::bind_cols(purrr::map_df(resp, "loginActivity") %>% dplyr::rename_at(vars(totalUsers:never), funs(paste0("loginActivity", stringr::str_to_title(.)))))
  return(org)
}


