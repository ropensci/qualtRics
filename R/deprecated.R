#' Register Qualtrics API Key, Base Url and Other Options
#'
#' This function is soft deprecated; use \code{\link[qualtRics]{qualtrics_api_credentials}}
#' instead. This function registers the user's qualtrics API key, base url and
#' other options for the remainder of the R session. This function only needs to
#' be called once (at the beginning of each R session). You may also use a
#' configuration file. See \code{\link{qualtRicsConfigFile}}. Note that you must
#' pass both an api token and a base url if you call this function for the first
#' time in a session and you're not using a config file. Thereafter, you can
#' pass these options individually.
#'
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertVariables Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param dateWarning Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.
#' @param ... Either one or both of 'api_token' and 'base_url' to register the Qualtrics api key and institution-specific root url manually. (see example). See also \code{\link{qualtRicsConfigFile}} for an explanation of the root_url and api_token parameters.
#'
#' @seealso See \url{https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file. See: \url{https://api.qualtrics.com/docs/authentication} to find your Qualtrics API key and \url{https://api.qualtrics.com/docs/root-url} for more information about the institution-specific root url.
#'
#' @importFrom yaml yaml.load_file
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.flag
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' # Note that you need to pass both the 'api_token' and 'base_url'
#' # parameters if you call this function for the first time.
#' registerOptions(api_token = "<YOUR-API-TOKEN>",
#'                 base_url = "<YOUR-ROOT-URL>")
#' # Register a different root url
#' registerOptions(base_url = "<YOUR-OTHER-ROOT-URL>")
#' # Retrieve a list of surveys
#' surveys <- getSurveys()
#' # Retrieve a single survey
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       verbose = TRUE)
#' # You can use the same parameters as those found in the qualtrics API documentation
#' # Found here: https://api.qualtrics.com/docs/csv
#' mysurvey <- getSurvey(surveyID = surveys$id[6],
#'                       save_dir = tempdir(),
#'                       startDate = "2017-01-01",
#'                       endDate = "2017-01-31",
#'                       limit = 100,
#'                       seenUnansweredRecode = "UNANS")
#' }
#'

registerOptions <- function(verbose=TRUE,
                            useLabels=TRUE,
                            convertVariables=TRUE,
                            useLocalTime=FALSE,
                            dateWarning=TRUE,
                            ...) {

  # START UP: CHECK ARGUMENTS PASSED BY USER ----

  .Deprecated("qualtrics_api_credentials()")

  # Take additional arguments
  args <- list(...)
  # Store base_url/api_token. Else give NA as value
  root_url <- ifelse("base_url" %in% names(args), args$base_url, NA)
  api_token <- ifelse("api_token" %in% names(args), args$api_token, NA)
  # Show deprecated warning
  calls <- names(vapply(match.call(), deparse, "character"))[-1]
  # Check if deprecated params passed
  if(any("root_url" %in% calls)) {
    warning("'root_url' is deprecated and will be removed in qualtRics 4.0. Please use 'base_url' instead.")
    # Save to new param
    root_url <- args$root_url
  }

  # OPTION 1: USER ALREADY SET ENV VARIABLES AND WANTS TO CHANGE OPT. VARIABLES ----

  # If environment variables are already set and user just wants to
  # change verbose argument
  if(Sys.getenv("QUALTRICS_ROOT_URL") != "" & Sys.getenv("QUALTRICS_API_KEY") != "") { # nolint

    # Check options
    assertthat::assert_that(assertthat::is.flag(verbose),
                            msg=paste0("'verbose' must be either TRUE or FALSE.")) # nolint
    assertthat::assert_that(assertthat::is.flag(convertVariables),
                            msg=paste0("'convertvariables' must be either TRUE or FALSE.")) # nolint
    assertthat::assert_that(assertthat::is.flag(useLabels),
                            msg=paste0("'uselabels' must be either TRUE or FALSE.")) # nolint
    assertthat::assert_that(assertthat::is.flag(useLabels),
                            msg=paste0("'uselabels' must be either TRUE or FALSE.")) # nolint
    assertthat::assert_that(assertthat::is.flag(dateWarning),
                            msg=paste0("'dateWarning' must be either TRUE or FALSE.")) # nolint

    # If user just wants to pass options, do:
    options(
      "QUALTRICS_VERBOSE" = verbose,
      "QUALTRICS_USELABELS" = useLabels,
      "QUALTRICS_CONVERTVARIABLES" = convertVariables,
      "QUALTRICS_USELOCALTIME" = useLocalTime
    )

    # Set warning
    invisible(ifelse(dateWarning == FALSE,
                     Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE),
                     TRUE))

    # Set root url and api token if not NA
    if(!is.na(root_url)) Sys.setenv("QUALTRICS_ROOT_URL" = root_url)
    if(!is.na(api_token)) Sys.setenv("QUALTRICS_API_KEY" = api_token)

    # Quietly quit
    return(invisible(NULL))

  }

  # OPTION 2: IF API TOKEN AND ROOT URL NOT PASSED, LOOK FOR CONFIG FILE ----

  # If API token and root url are NA, then look for .qualtRics.yml
  #in the current directory
  if(is.na(api_token) & is.na(root_url)) {

    # Check if config file exists
    ex <- file.exists(".qualtRics.yml")
    # Throw error if file does not exist
    assertthat::assert_that(ex == TRUE, msg = "No .qualtRics.yml configuration file found in working directory. Either set your \napi token and root url using the 'registerOptions' function or create a configuration file.\nExecute 'qualtRicsConfigFile()' to view an example of a configuration file.") # nolint

    # Load file
    cred <- yaml::yaml.load_file(".qualtRics.yml")

    # Assert that names are "api_token" and "root_url"
    assertthat::assert_that((all(c("api_token", "root_url") %in% names(cred))) |
                              (all(c("api_token", "base_url") %in% names(cred))), msg="Either the 'api_token' or 'base_url' arguments are missing in your .qualtRics.yml\nconfiguration file. Execute 'qualtRicsConfigFile()' to view an example of the configuration file.\nExecute 'file.edit('.qualtRics.yml')' to edit your configuration file.") # nolint

    # If verbose, print message
    if(verbose) message(paste0("Found a .qualtRics.yml configuration file in ", getwd(), ". Using these credentials.")) # nolint

    # Set vars
    api_token <- cred$api_token
    # Check if deprecated params passed
    if(any("root_url" %in% names(cred))) {
      message("\nWarning: 'root_url' is deprecated and will be removed in qualtRics 4.0. Please use 'base_url' instead.")
      # Save to new param
      root_url <- cred$root_url
    } else if("base_url" %in% names(cred)) {
      root_url <- cred$base_url
    }

    # Set optional vars
    if("verbose" %in% names(cred)) {
      verbose <- cred$verbose
      assertthat::assert_that(assertthat::is.flag(verbose), msg=paste0("'verbose' must be either TRUE or FALSE but is ", as.character(verbose), " in your config file.")) # nolint
    }
    # If 'convertStandardColumns' is found in credentials then emit a warning
    if('convertstandardcolumns' %in% names(cred) & !'convertvariables' %in% names(cred)) { # nolint
      message("'convertstandardcolumns' has been deprecated and will be ignored. Please replace it\nwith 'convertvariables' in your '.qualtRics.yml' file. Visit <https://github.com/ropensci/qualtRics>\nfor more information.") # nolint
      convertVariables <- TRUE
    } else if(all(c('convertstandardcolumns', 'convertvariables') %in% names(cred))) { # nolint
      message("'convertstandardcolumns' has been deprecated and will be ignored. Please remove it\nfrom your '.qualtRics.yml' file. Visit <https://github.com/ropensci/qualtRics> for\nmore information.") # nolint
      convertVariables <- cred$convertvariables
    } else {
      convertVariables <- cred$convertvariables
    }

    # Check if variables are correct types
    assertthat::assert_that(assertthat::is.flag(convertVariables),
                            msg=paste0("'convertvariables' must be either TRUE or FALSE but is ", # nolint
                                       as.character(convertVariables),
                                       " in your config file."))
    if("uselabels" %in% names(cred)) {
      useLabels <- cred$uselabels
      assertthat::assert_that(assertthat::is.flag(useLabels), msg=paste0("'uselabels' must be either TRUE or FALSE but is ", as.character(useLabels), " in your config file.")) # nolint
    }
    if("uselocaltime" %in% names(cred)) {
      useLocalTime <- cred$uselocaltime
      assertthat::assert_that(assertthat::is.flag(useLocalTime), msg=paste0("'useLocalTime' must be either TRUE or FALSE but is ", as.character(useLocalTime), " in your config file.")) # nolint
    }
    if("datewarning" %in% names(cred)) {
      dateWarning <- cred$datewarning
      assertthat::assert_that(assertthat::is.flag(dateWarning), msg=paste0("'dateWarning' must be either TRUE or FALSE but is ", as.character(dateWarning), " in your config file.")) # nolint
    }

  }

  # IF OPTION 1 & 2 FAIL, THROW ERRORS ----

  # Check arguments
  # If either is still NA AND environment is empty, throw error
  assertthat::assert_that(!is.na(root_url) | Sys.getenv("QUALTRICS_ROOT_URL") != "", msg="'root_url' parameter must either be specified in the .qualtRics.yml configuration file\nor passed to the 'registerOptions' function. To view an example of a configuration file, execute\n'qualtRicsConfigFile()'.") # nolint
  assertthat::assert_that(!is.na(api_token) | Sys.getenv("QUALTRICS_API_KEY") != "", msg="'api_token' parameter must either be specified in the .qualtRics.yml configuration file\nor passed to the 'registerOptions' function. To view an example of a configuration file, execute\n'qualtRicsConfigFile()'.") # nolint

  # REGISTER VALUES AND WRAP-UP ----

  # Set environment variables
  if(!is.na(api_token)) Sys.setenv("QUALTRICS_API_KEY" = api_token)
  if(!is.na(root_url)) Sys.setenv("QUALTRICS_ROOT_URL" = root_url)
  # Set flag options
  options(
    "QUALTRICS_VERBOSE" = verbose,
    "QUALTRICS_USELABELS" = useLabels,
    "QUALTRICS_CONVERTVARIABLES" = convertVariables,
    "QUALTRICS_USELOCALTIME" = useLocalTime
  )
  # Set warning
  invisible(ifelse(dateWarning == FALSE,
                   Sys.setenv("QUALTRICS_WARNING_DATE_GIVEN"=TRUE),
                   TRUE))


}

#' Prints an Example of a QualtRics Configuration File to the Console.
#'
#' This function is soft deprecated; use \code{\link[qualtRics]{qualtrics_api_credentials}} instead.
#'
#' @param api_token String. API token. Available in your qualtrics account (see: \url{https://api.qualtrics.com/docs/authentication})
#' @param base_url String. Base url for your institution (see: \url{https://api.qualtrics.com/docs/root-url})
#' @param verbose Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
#' @param useLabels Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
#' @param convertVariables Logical. If TRUE, then the \code{\link[qualtRics]{getSurvey}} function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE.
#' @param useLocalTime Logical. Use local timezone to determine response date values? Defaults to FALSE. See \url{https://api.qualtrics.com/docs/dates-and-times} for more information.
#' @param dateWarning Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.
#' @param root_url String. Deprecated. Use `base_url` instead. This will be removed in future versions.
#' @seealso See \url{https://api.qualtrics.com/docs/root-url} for documentation on the Qualtrics API. See \url{https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file} for more information about the qualtRics configuration file.
#' @export
#' @examples
#' \dontrun{
#' # Execute this line to get instructions on how to make a .qualtrics.yml config file.
#' qualtRicsConfigFile()
#' }
#'

qualtRicsConfigFile <- function(api_token = NULL,
                                base_url = NULL,
                                verbose = TRUE,
                                useLabels = TRUE,
                                convertVariables = TRUE,
                                useLocalTime = FALSE,
                                dateWarning = TRUE,
                                root_url = NULL) {

  .Deprecated("qualtrics_api_credentials()")

  # Check for deprecated arguments
  calls <- names(vapply(match.call(), deparse, "character"))[-1]
  # Check if deprecated params passed
  if(any("root_url" %in% calls)) {
    warning("'root_url' is deprecated and will be removed in qualtRics 4.0.\n Please use 'base_url' instead.")
    base_url <- root_url
  }
  # Temporary
  root_url <- base_url

  # Paste together a message to cat to console
  msg <- paste0(
    "Copy-paste the lines between the dashes into a new plain text file, replace the
values for the api_token and root_url if they are not yet filled out. and save it in
your working directory as '.qualtRics.yml'. Execute '?qualtRics::qualtRicsConfigFile'
to view an explanation of the additional arguments. Visit
https://github.com/ropensci/qualtRics/blob/master/README.md#using-a-configuration-file
for more information.", "\n\n",# nolint
    "--------------","\n",
    'api_token: ', ifelse(is.null(api_token), '<YOUR-API-TOKEN-HERE>',
                          paste0(api_token)), "\n",
    'base_url: ', ifelse(is.null(root_url), '<YOUR-ROOT-URL-HERE>',
                         paste0(root_url)), "\n",
    'verbose: ', verbose, "\n",
    'uselabels: ', useLabels, "\n",
    'convertvariables: ', convertVariables, "\n",
    'uselocaltime: ', useLocalTime, "\n",
    'datewarning: ', dateWarning, "\n",
    "--------------"
  )

  # Cat to console
  cat(msg)

}
