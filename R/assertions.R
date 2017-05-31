#   Download qualtrics data into R
#    Copyright (C) 2017 Jasper Ginn

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


### GETSURVEY

# Test whether API key is stored
assert_apikey_stored <- function(dir) {
  # Key should be stored by "registerApiKey"
  assertthat::assert_that("qualtRics_header.rds" %in% list.files(dir),
                          msg = "You need to register your qualtrics API key first using the 'registerApiKey()' function.")
}

# Check if save directory exists
assert_saveDir_exists <- function(save_dir) {
  assertthat::assert_that(ifelse((!file.info(save_dir)$isdir | is.na(file.info(save_dir)$isdir) == TRUE), FALSE, TRUE),
                          msg = paste0("The directory ", save_dir, " does not exist."))
}

# Check if root_url is a string
assert_rootUrl_string <- function(root_url) {
  assertthat::assert_that(assertthat::is.string(root_url))
}

# Check if seenUnansweredRecode is a string
assert_seenUnansweredRecode_string <- function(seenUnansweredRecode) {
  assertthat::assert_that(assertthat::is.string(seenUnansweredRecode))
}

# Check if limit > 0
assert_limit_abovezero <- function(limit) {
  assertthat::assert_that(limit > 0, msg="Limit parameter should be at least 1.")
}

### READSURVEY

assert_surveyFile_exists <- function(file_name) {
  assertthat::assert_that(file.exists(file_name))
}
