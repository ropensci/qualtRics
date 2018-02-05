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

# Test whether API key is stored in environment
assert_apikey_stored <- function() {
  # Key should be stored by "registerApiKey"
  assertthat::assert_that(Sys.getenv("QUALTRICS_API_KEY") != "",
                          msg = "You need to register your qualtrics API key and root url using the 'registerOptions()' function.")
}

# Check if root_url is stored
assert_rootUrl_stored <- function() {
  assertthat::assert_that(Sys.getenv("QUALTRICS_ROOT_URL") != "",
                          msg = "You need to register your qualtrics API key and root url using the 'registerOptions()' function.")
  # Test if root url ends with '.qualtrics.com'
  assertthat::assert_that(endsWith(Sys.getenv("QUALTRICS_ROOT_URL"), '.qualtrics.com'),
                          msg=paste0("The qualtrics root url must end with '.qualtrics.com'. Your root url looks like this: '",
                                     Sys.getenv("QUALTRICS_ROOT_URL"),
                                     "'. Please visit https://api.qualtrics.com/docs/root-url for instructions about the qualtrics root url."))
}

# Check if save directory exists
assert_saveDir_exists <- function(save_dir) {
  assertthat::assert_that(ifelse((!file.info(save_dir)$isdir | is.na(file.info(save_dir)$isdir) == TRUE),
                                 FALSE, TRUE),
                          msg = paste0("The directory ", save_dir, " does not exist."))
}

# Check if seenUnansweredRecode is a string
assert_seenUnansweredRecode_string <- function(seenUnansweredRecode) {
  assertthat::assert_that(assertthat::is.string(seenUnansweredRecode))
}

# Check if lastResponseId is a string
assert_lastResponseId_string <- function(lastResponseId) {
  assertthat::assert_that(assertthat::is.string(lastResponseId))
}

# Check if startDate/EndDate are strings
assert_startDate_string <- function(startDate) {
  assertthat::assert_that(assertthat::is.string(startDate))
}
assert_endDate_string <- function(endDate) {
  assertthat::assert_that(assertthat::is.string(endDate))
}

# Check if includedQuestionIds are string(s)
assert_includedQuestionIds_string <- function(includedQuestionIds) {
  assertthat::assert_that(mode(includedQuestionIds) == "character",
                          msg="'includedQuestionIds' must be a character vector.")
}

# Check if limit > 0
assert_limit_abovezero <- function(limit) {
  assertthat::assert_that(limit > 0, msg="Limit parameter should be at least 1.")
}

# Check if survey file exists
assert_surveyFile_exists <- function(file_name) {
  assertthat::assert_that(file.exists(file_name),
                          msg=paste0("File ",
                                     file_name,
                                     " does not exist. Please check if you passed the right file path."))
}

# Check if these arguments are logical
assert_options_logical <- function(verbose, convertVariables,
                                   useLocalTime, useLabels) {
  assertthat::assert_that(assertthat::is.flag(verbose),
                          msg="'verbose' must be TRUE or FALSE.")
  assertthat::assert_that(assertthat::is.flag(convertVariables),
                          msg="'convertVariables' must be TRUE or FALSE.")
  assertthat::assert_that(assertthat::is.flag(useLocalTime),
                          msg="'useLocalTime' must be TRUE or FALSE.")
  assertthat::assert_that(assertthat::is.flag(useLabels),
                          msg="'useLabels' must be TRUE or FALSE.")
}
