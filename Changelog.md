**[development branch]**

- convertstandardcolumns deprecated since readr::read_csv does this automagically. 
- Added .onUnload and .onDetach conditions so that environment variables (root url and API key) are removed when package is unloaded. This prevents issues if user decides to load the package again.
- Cleaned up code

**[master branch]**

- We found that surveys that use new lines in the questions break the readSurvey function.
The problem is, that read.csv (and read.table as well as the readr library implementation) ignore the quote = "\"" option when a skip = 2 or skip = 3 parameter is set. As a result the read function slices off the questions row somewhere in the middle when first importing just the table body using skip.

**[v2.2]**

- `readSurvey()` now takes an additional argument, fileEncoding, so that user can import surveys using a specific encoding. 'fileEncoding' can also be passed as optional argument in `getSurvey()`. Added new parameter that reads legacy data format.
- Better argument checking.
- `getSurveyQuestions()` now returns additional information. h/t @phoebewong
- Fixes several bugs and stability issues
- More informative error messages

**[v2.0]**

- `registerOptions()` now takes more arguments. User can now set global options. See `qualtRicsConfigFile()` for more information. Same options are now passed through `...` in specific functions.
- Added appveyor testing.
- Added support for a configuration file to store API key and root url in the working directory.
- `registerApiKey()` has been replaced by `registerOptions()`. This function stores both a user's API key and root url. Function also scans for a configuration file `.qualtRics.yml` that contains this information.
- Added a new script called `zzz.R`. When the package is loaded, the .onLoad() function in this file scans the working directory for a `.qualtRics.yml` configuration file so that the user doesn't have to register this information manually.
- Added a new function `qualtRicsConfigFile()` that prints instructions for the user on how to set up a configuration file to the R Console.
- Removed the `root_url` parameter from all functions that required it.
- Dates are now converted without a specific timezone.
- Added a new function `getSurveyQuestions()` that allows the user to download a data frame containing question labels and IDs.
- Added parameter **includedQuestionIds** so user can select questions they want to download. Need to use the QID value from `getSurveyQuestions()`.
- Updated examples and documentation of functions.
- Added the following parameters to `getSurvey()`:
  - **seenUnansweredRecode:**  String. Recode seen but unanswered questions with a string value.
  - **limit:** Integer. Maximum number of responses exported. Defaults to NULL (all responses).
  - **useLocalTime:** Boolean. Use local timezone to determine response date values. 
- `getSurveys()` now retrieves > 100 results.

**[v1.0]**

- Added a new function `readSurvey()`. This function is used in the `getSurvey()` function but will also work with surveys downloaded manually from Qualtrics. Standard columns (completed survey/startDate/endDate etc.) are now converted to their proper data types. HT Adrian Brugger & Stefan Borer.
- User can only download surveys in CSV format, no longer in XML or JSON. 
- Added several new parameters to `getSurvey()` function. HT @samuelkaminsky & @eknud
  * *LastResponseId*: If used, only responses that were filled out later than this ID will be downloaded. 
  * *UseLabels*: If TRUE, download will contain character labels. Else, download will contain choice labels.
  * *StartDate*: Only download responses after this date.
  * *EndDate*: Only download responses before this date.
- Survey downloads should be faster now; `getSurvey()` no longer sleeps when checking download status. Also added progress bar.

**[v0.03]**

- User can choose to save results directly in a folder through 'save_dir' parameter in `getSurvey()`
- Results can now be requested in .csv, .json or .xml format. The packages `jsonlite` and `XML` are added to 'Suggests' in DESCRIPTION.
- `constructHeader()` is now deprecated and should no longer be used. Instead, users need to call `registerApiKey()`.
- Added a new function `registerApiKey()` which saves the user's API key and header information in the `tempdir()` environment. 

**[v0.02]**

- Renamed 'base url' to 'root url' such that it corresponds to Qualtrics documentation.
- The root url no longer requires API-specific endpoints. So e.g. 'https://leidenuniv.eu.qualtrics.com' now works for all functions. The API-specific endpoints are added in the functions itself.
- Institution-specific root url is now required by `getSurvey()`

**[v0.01]**

- Added first three functions (`constructHeader`, `getSurvey`, `getSurveyIDs`)
- base_url parameter is now uniform across functions. Parameter is called 'root url' to bring it in line with Qualtrics documentation.
