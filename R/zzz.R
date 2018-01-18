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
#    along with this program.  If not, see <http://www.gnu.org/licenses/>

'
Loads qualtRics credentials automatically when package is loaded and ".qualtRics.yml" file is present in working directory. User needs to have qualtRics API key and root url stored in a configuration file in working directory. For an example of a configuration file, execute "qualtRicsConfigFile()". See: https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file
'

.onLoad <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {
  if(file.exists(".qualtRics.yml")) {
    suppressWarnings(registerOptions()) # This throws a warning
  }
  # Set global qualtrics options
  options(
    "QUALTRICS_GLOBAL_OPTIONS" = list("question_types_supported" = c("MC"))
  )
}

.onUnload <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {
  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_ROOT_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")
}

.onDetach <- function(libname = find.package("qualtRics"), pkgname="qualtRics") {
  # If user unloads/detaches package make sure that these values are erased
  Sys.setenv("QUALTRICS_ROOT_URL" = "")
  Sys.setenv("QUALTRICS_API_KEY" = "")
}
