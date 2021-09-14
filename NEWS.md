# qualtRics (development version)

# qualtRics 3.1.5

- Add `fetch_description()` to download complete survey description metadata from v3 API endpoint (more up-to-date than older `metadata()`) thanks to @jmobrien (#207)
- Warn users about possible incorrect results from API when `breakout_sets` and `label` are both FALSE
- Refactor internal URL creation for API calls thanks to @jmobrien (#225)
- Add `fetch_id()` to return a `surveyID` based on a unique survey name as it appears in the Qualtrics UI thanks to @markjrieke (#230).  

# qualtRics 3.1.4

- Add `fetch_distributions()` to access distribution data for a specific survey thanks to @dsen6644 (#169)
- Handle mailing list embedded data better thanks to @dsen6644 (#175)
- Updated links to API documentation
- Create unique column names for questions using `choiceId` thanks to @lyh970817 (#182)
- Fix bug when `include_questions` only contains one QID thanks to @lyh970817 (#197)
- Generate correct/updated column mapping for survey responses thanks to @jmobrien (#199). These column mappings are available as an attribute on survey results or via the new `extract_colmap()` function. 

# qualtRics 3.1.3

- Update `include_questions` argument to use correct name in API request.
- Build API payloads with jsonlite (#155) thanks to @jmobrien
- Convert tests to webmockr and vcr (#140 and #161) thanks to @shaun-jacks and @dsen6644
- Allow user to specify column types for both `fetch_survey()` and `read_survey()` (#162) thanks to @jntrcs 

# qualtRics 3.1.2

- For empty surveys, return zero row dataframe (#127)
- Remove unnecessary dependency on yaml and deprecate `qualtRicsConfigFile()`, to avoid unexpected behavior
- Deprecate old versions of functions: `getSurveys()`, `getSurveyQuestions()`, `getSurvey()`, `readSurvey()`
- Move to updated version of Qualtrics API (#130) thanks to @jmobrien
- Correctly handle time zone conversions (#137) thanks to @jmobrien
- Add `breakout_sets` parameter thanks to @shaun-jacks
- Fix bug in `infer_data_types()` for answers choices that include HTML
- Deprecate `last_response` argument no longer used by API (#153)

# qualtRics 3.1.1

- Fix bug in `infer_data_types()` to avoid errors with factors/numeric values
- Improvements to documentation, error checking
- Allow user to access column mapping for questions and IDs (#115)
- Deprecate `registerOptions()` to avoid unexpected behavior with options
- Make data import more robust with more condition and error checking, as well as better defaults

# qualtRics 3.1.0

- New maintainer: Julia Silge
- Add all previous contributors to DESCRIPTION as `ctb`
- Declare testthat dependency in DESCRIPTION (reason for previous archiving from CRAN)
- Simpler approach for storing API credentials as environment variables with `qualtrics_api_credentials()` (`registerOptions()` is now soft deprecated with a warning)
- Simplify README (keep all existing detailed workflow documentation in vignette)
- Relicense from GPL-3 to MIT. See [consent from authors here](https://github.com/ropensci/qualtRics/issues/95).
- Improvements to documentation throughout
- Renaming (with warnings on old versions) of key functions for clarity and reduction in confusion, plus improvements:
  - `all_surveys()` (from old version of `getSurveys()`)
  - `survey_questions()` (from old version of `getSurveyQuestions()`)
  - `fetch_survey()` (from old version of `getSurvey()`)
  - `read_survey()` (from old version of `readSurvey()`)


qualtRics 3.0 (2018-02-03)
=========================

### NEW FEATURES

- Added 'metadata' function that allows the user to retrieve detailed metadata about survey.
- User can now convert specific question types automatically. See [this page](https://github.com/JasperHG90/qualtRics#automatic-conversion-of-variables) for more information.

### MINOR IMPROVEMENTS

- Using package [httptest](https://CRAN.R-project.org/package=httptest) for mock API requests so that API calls can be tested. 
- `getSurveys()` and `getSurveyQuestions()` now return a [tibble](https://CRAN.R-project.org/package=tibble)

### BUG FIXES

- Added .onDetach conditions so that environment variables (root url and API key) are removed when package is unloaded. This prevents issues if user decides to load the package again.
- We found that surveys that use new lines in the questions break the readSurvey function.
The problem is, that read.csv (and read.table as well as the readr library implementation) ignore the quote = "\"" option when a skip = 2 or skip = 3 parameter is set. As a result the read function slices off the questions row somewhere in the middle when first importing just the table body using skip.

### DEPRECATED AND DEFUNCT

- convertstandardcolumns deprecated since readr::read_csv does this automagically. It has been changed in config file to 'convertvariables'. 

qualtRics 2.2 (2017-10-27)
=========================

- `readSurvey()` now takes an additional argument, fileEncoding, so that user can import surveys using a specific encoding. 'fileEncoding' can also be passed as optional argument in `getSurvey()`. Added new parameter that reads legacy data format.
- Better argument checking.
- `getSurveyQuestions()` now returns additional information. h/t @phoebewong
- Fixes several bugs and stability issues
- More informative error messages

qualtRics 2.0 (2017-06-16)
=========================

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

qualtRics 1.0 (2016-10-13)
=========================

- Added a new function `readSurvey()`. This function is used in the `getSurvey()` function but will also work with surveys downloaded manually from Qualtrics. Standard columns (completed survey/startDate/endDate etc.) are now converted to their proper data types. HT Adrian Brugger & Stefan Borer.
- User can only download surveys in CSV format, no longer in XML or JSON. 
- Added several new parameters to `getSurvey()` function. HT @samuelkaminsky & @eknud
  * *LastResponseId*: If used, only responses that were filled out later than this ID will be downloaded. 
  * *UseLabels*: If TRUE, download will contain character labels. Else, download will contain choice labels.
  * *StartDate*: Only download responses after this date.
  * *EndDate*: Only download responses before this date.
- Survey downloads should be faster now; `getSurvey()` no longer sleeps when checking download status. Also added progress bar.

qualtRics 0.03 [beta] 
=========================

- User can choose to save results directly in a folder through 'save_dir' parameter in `getSurvey()`
- Results can now be requested in .csv, .json or .xml format. The packages `jsonlite` and `XML` are added to 'Suggests' in DESCRIPTION.
- `constructHeader()` is now deprecated and should no longer be used. Instead, users need to call `registerApiKey()`.
- Added a new function `registerApiKey()` which saves the user's API key and header information in the `tempdir()` environment. 

qualtRics 0.02 [beta] 
=========================

- Renamed 'base url' to 'root url' such that it corresponds to Qualtrics documentation.
- The root url no longer requires API-specific endpoints. So e.g. 'https://leidenuniv.eu.qualtrics.com' now works for all functions. The API-specific endpoints are added in the functions itself.
- Institution-specific root url is now required by `getSurvey()`

qualtRics 0.01 [beta] 
=========================

- Added first three functions (`constructHeader`, `getSurvey`, `getSurveyIDs`)
- base_url parameter is now uniform across functions. Parameter is called 'root url' to bring it in line with Qualtrics documentation.
