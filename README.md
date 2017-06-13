# qualtRics

[![Build Status](https://travis-ci.org/JasperHG90/qualtRics.svg?branch=master)](https://travis-ci.org/JasperHG90/qualtRics) [![CRAN STATUS](https://www.r-pkg.org/badges/version/qualtRics)](https://cran.r-project.org/web/packages/qualtRics/index.html) [![DOWNLOADS](https://cranlogs.r-pkg.org/badges/qualtRics)](https://www.r-pkg.org/pkg/qualtRics) [![CODECOV](https://codecov.io/gh/JasperHG90/qualtRics/branch/master/graphs/badge.svg)](https://codecov.io/gh/JasperHG90/qualtRics)

This project contains an R package to download surveys from  [Qualtrics](https://www.qualtrics.com/) using the API. 

Note that your institution must support API access and that it must be enabled for your account. Whoever manages your Qualtrics account can help you with this. Please refer to the [Qualtrics documentation](https://api.qualtrics.com/docs/authentication) to find your API token.

## Installation

To install this package, execute the following in R:

```r
install.packages("qualtRics")
```

Or, if you want to install the latest development version, run:

```r
install.packages("devtools")
devtools::install_github("JasperHG90/qualtRics")
```

## Dependencies

All dependencies will be installed when you install `qualtRics`.

## Updates

Periodically check this repository for updates and execute `devtools::install_github("JasperHG90/qualtRics")` to update.

## Usage

Currently, the package contains three core functions:

  1. *getSurveys()* fetches a list of all surveys that you own or have access to from Qualtrics.
  2. *getSurvey()* downloads a survey from Qualtrics and loads it into R.
  3. *readSurvey()* allows you to read CSV files you download manually from Qualtrics.
  
It further contains two helper functions:

  1. *registerApiKey()* stores your qualtRics API key in a temporary file
  2. *getSurveyQuestions()* retrieves a data frame containing questions and question IDs for a survey

Note that you can only export surveys that you own, or to which you have been given administration rights.

### Commands

Load the package:

```r
library(qualtRics)
```

Register your Qualtrics API key. You need to do this once every R session:

```r
registerApiKey(API.TOKEN = "<yourapitoken>")
```

Get a data frame of surveys:

```r
surveys <- getSurveys(root_url="https://leidenuniv.eu.qualtrics.com") # URL is for my own institution
```

Note that, while requests will work without a [root url](https://api.qualtrics.com/docs/root-url) for the `getSurveys` function, it is desirable that you supply it. Supplying the correct url will reduce the number of errors you'll experience. The `getSurvey` function requires you to pass this root url. Please refer to the [official documentation](https://api.qualtrics.com/docs/root-url) to find out your institution-specific root url.

Export a survey and load it into R:

```r
mysurvey <- getSurvey(surveyID = surveys$id[6], 
                      root_url = "https://leidenuniv.eu.qualtrics.com", 
                      verbose = TRUE)
```

You can add a from/to date to only retrieve responses between those dates:

```r
surv <- getSurvey(survs$id[4],
                  root_url = "https://leidenuniv.eu.qualtrics.com",
                  startDate = "2016-09-18",
                  endDate = "2016-10-01",
                  useLabels = FALSE,
                  verbose = TRUE)
```

You may also reference a response ID. `getSurvey` will then download all responses that were submitted after that response:

```r
surv <- getSurvey(survs$id[4],
                  root_url = "https://leidenuniv.eu.qualtrics.com",
                  lastResponseId = "R_3mmovCIeMllvsER",
                  useLabels = FALSE,
                  verbose = TRUE)
```

You can filter a survey for specific questions:

```r
# Retrieve questions for a survey
questions <- getSurveyQuestions(surveyID = surveys$id[6],
                                root_url = "https://leidenuniv.eu.qualtrics.com")
# Retrieve a single survey, filtering for questions I want.
mysurvey <- getSurvey(surveyID = surveys$id[6],
                      root_url = "https://leidenuniv.eu.qualtrics.com",
                      save_dir = tempdir(),
                      includedQuestionIds = c("QID1", "QID2", "QID3"),
                      verbose=TRUE)
```

You can store the results in a specific location if you like:

```r
mysurvey <- getSurvey(surveyID = surveys$id[6], 
                      save_dir = "/users/jasper/desktop/", 
                      root_url = "https://leidenuniv.eu.qualtrics.com", 
                      verbose = TRUE)
```

Note that surveys that are stored in this way will be saved as an [RDS](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html) file rather than e.g. a CSV. Reading an RDS file is as straightforward as this:

```r
mysurvey <- readRDS(file = "/users/jasper/desktop/mysurvey.rds")
```

You can read a survey that you downloaded manually using `readSurvey`:

```r
mysurvey <- readSurvey("/users/jasper/desktop/mysurvey.csv")
```

To avoid special characters (mainly periods) in header names, `readSurvey` uses question labels as the header names. The question belonging to that label is then added using the [sjmisc](https://CRAN.R-project.org/package=sjmisc) package. Qualtrics gives names to these labels automatically, but you can easily change them.

![](/vignettes/qualtricsdf.png)

## Other information

For specific information about the Qualtrics API, you can refer to the [official documentation](https://api.qualtrics.com/docs/overview).

### Issues

Should you encounter any bugs or issues, please report them [here](https://github.com/JasperHG90/qualtRics/issues)

### Requests

If you have a request (like adding a new argument), please leave it [here](https://github.com/JasperHG90/qualtRics/issues)

### Changelog

**[development branch]**

- Added support for a configuration file to store API key and root url in the working directory.
- `registerApiKey()` has been replaced by `registerOptions()`. This function stores both a user's API key and root url. Function also scans for a configuration file `.qualtRics.yml` that contains this information.
- Added a new script called `zzz.R`. When the package is loaded, the .onLoad() function in this file scans the working directory for a `.qualtRics.yml` configuration file so that the user doesn't have to register this information manually.
- Added a new function `qualtRicsConfigFile()` that prints instructions for the user on how to set up a configuration file to the R Console.
- Removed the `root_url` parameter from all functions that required it.

**[master branch]**

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
