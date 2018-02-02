# qualtRics

[![Build Status](https://travis-ci.org/JasperHG90/qualtRics.svg?branch=master)](https://travis-ci.org/JasperHG90/qualtRics) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/fv9bmqcmty1wwrgu?svg=true)](https://ci.appveyor.com/project/JasperHG90/qualtrics-g2a4u) [![CRAN STATUS](https://www.r-pkg.org/badges/version/qualtRics)](https://cran.r-project.org/web/packages/qualtRics/index.html) [![CRAN LICENSE](https://img.shields.io/cran/l/qualtRics.svg)](https://img.shields.io/cran/l/qualtRics.svg) [![DOWNLOADSPM](https://cranlogs.r-pkg.org/badges/qualtRics)](https://www.r-pkg.org/pkg/qualtRics) [![DOWNLOADSTOTAL](https://cranlogs.r-pkg.org/badges/grand-total/qualtRics)](https://cranlogs.r-pkg.org/badges/grand-total/qualtRics) [![CODECOV](https://codecov.io/gh/JasperHG90/qualtRics/branch/master/graphs/badge.svg)](https://codecov.io/gh/JasperHG90/qualtRics)

This project contains an R package to download surveys from  [Qualtrics](https://www.qualtrics.com/) using the API.  

Note that your institution must support API access and that it must be enabled for your account. Whoever manages your Qualtrics account can help you with this. Please refer to the [Qualtrics documentation](https://api.qualtrics.com/docs/authentication) to find your API token.

I am not affiliated with Qualtrics and they do not offer support for this package.

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
  
It further contains four helper functions:

  1. *registerOptions()* stores your API key and root url in environment variables.
  2. *getSurveyQuestions()* retrieves a data frame containing questions and question IDs for a survey.
  3. *qualtRicsConfigFile()* prints information on how to make a .qualtRics.yml configuration file that stores your qualtRics API key, root url and other options in your working directory.
  4. *metadata()* retrieves metadata about your survey, such as questions, survey flow, number of responses etc.

Note that you can only export surveys that you own, or to which you have been given administration rights.

## Registering your Qualtrics credentials

There are two ways to register your Qualtrics credentials (your [API key](https://api.qualtrics.com/docs/finding-qualtrics-ids) and [institution-specific root url](https://api.qualtrics.com/docs/root-url)) and other options in R. As in earlier versions of the qualtRics package, you can register your credentials at the start of each R session:

```r
registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
```

You can set some global options via the `registerOptions()` function:

1. **verbose:** Logical. If TRUE, verbose messages will be printed to the R console. Defaults to TRUE.
2. [useLabels](https://api.qualtrics.com/docs/create-response-export): Logical. TRUE to export survey responses as Choice Text or FALSE to export survey responses as values.
3. **convertvariables:** Logical. If TRUE, then the function will convert certain question types (e.g. multiple choice) to proper data type in R. Defaults to TRUE. (see [below](#automatic-conversion-of-variables) for more information)
4. [useLocalTime](https://api.qualtrics.com/docs/dates-and-times): Logical. Use local timezone to determine response date values? Defaults to FALSE.
5. **dateWarning:** Logical. Once per session, qualtRics will emit a warning about date conversion for surveys. You can turn this warning off by changing the flag to FALSE. Defaults to TRUE.

You can change some of these options without having to pass the `api_token` or `root_url` parameters every time as long as you have registered the api token and root url previously:

```r
registerOptions(verbose=FALSE, useLocalTime=TRUE)
```

The second method involves placing a configuration file called `.qualtRics.yml` in your working directory. 

### Using a configuration file

qualtRics supports the use of a configuration file to store your Qualtrics credentials. Executing `qualtRicsConfigFile()` returns information that explains how you can do this:

```
Copy-paste the lines between the dashes into a new plain text file, replace the values for the
api_token and root_url if they are not yet filled out. and save it in your working directory 
as '.qualtRics.yml'. Execute '?qualtRics::qualtRicsConfigFile' to view an explanation of the additional arguments. Visit https://github.com/JasperHG90/qualtRics/blob/master/README.md#using-a-configuration-file 
for more information.

--------------
api_token: <YOUR-API-TOKEN-HERE>
root_url: <YOUR-ROOT-URL-HERE>
verbose: TRUE
uselabels: TRUE
convertvariables: TRUE
uselocaltime: FALSE
datewarning: TRUE
--------------
```

You can also call this function while passing `api_token` and `root_url` values to the function, in which case `<YOUR-API-TOKEN-HERE>` and `<YOUR-ROOT-URL-HERE>` will be replaced by your credentials. After saving the file, you can register your credentials by calling `registerOptions()` without passing any parameters.

When you load the qualtRics package, it will automatically look for a `.qualtRics.yml` file in the working directory, in which case you don't need to call the `registerOptions()` function to register your qualtRics credentials at the beginning of your session.

You can override your configuration file settings by calling `registerOptions()` with the changes you want to make:

```r
registerOptions(verbose=FALSE, useLabels=FALSE, root_url="myinstitution.qualtrics.com")
```

#### Setting up a config file

Open an existing R project or start a new one. Then, open up an empty text file:

![](https://raw.githubusercontent.com/JasperHG90/qualtRics/master/img/config_step1.png)

Execute `qualtRicsConfigFile(api_token="<YOUR-API-TOKEN-HERE>", root_url="<YOUR-ROOT-URL-HERE>")` and copy-paste the text between the dashes to the empty text file:

![](https://raw.githubusercontent.com/JasperHG90/qualtRics/master/img/config_step2.png)

Save the file as `.qualtRics.yml` and execute `registerOptions()` or restart your R session and execute `library(qualtRics)` to load the configuration file.

You can edit your configuration file by executing `file.edit(".qualtRics.yml")` in the R console.

## Automatic conversion of variables

From version 2.5, qualtRics supports the automatic conversion of specific variable types (see table below) since users already specify most information needed for such a conversion when they design their survey. 

For example, using the `metadata()` function, you can pull in metadata about your survey questions:

```r
# Get metadata for a survey
md <- metadata(id)
# Filter for questions
md.f <- md$questions
# Pick specific question
QOI <- md.f$QID172807686
# View question type
QOI$questionType
```

```
$type
[1] "MC"

$selector
[1] "SAVR"

$subSelector
[1] "TX"
```

We see that this is a multiple choice question ("MC") with a single answer ("SAVR"). The data supplied also includes the different answers a user can give:

```r
# Return question description
lapply(QOI$choices, function(x) x$description)
```

```
$`1`
[1] "Extremely useful"

$`2`
[1] "Very useful"

$`3`
[1] "Moderately useful"

$`4`
[1] "Slightly useful"

$`5`
[1] "Not useful at all"
```

This data can be used to turn the variable into a factor automatically.

### Supported types

Currently, the following data types are supported for automatic conversion:

| Type            | Selector      | Abbreviation | Version |
|-----------------|---------------|--------------|---------|
| Multiple choice | Single answer | "MC", "SAVR" | 2.5     |

## Commands

Load the package:

```r
library(qualtRics)
```

Register your Qualtrics credentials if required:

```r
registerOptions(api_token="<YOUR-API-TOKEN>", root_url="<YOUR-ROOT-URL>")
```

Get a data frame of surveys:

```r
surveys <- getSurveys() 
```

Export a survey and load it into R:

```r
mysurvey <- getSurvey(surveyID = surveys$id[6], 
                      verbose = TRUE) # You can set this option globally
                                      # or pass it to the function.
```

You can add a from/to date to only retrieve responses between those dates:

```r
surv <- getSurvey(survs$id[4],
                  startDate = "2016-09-18",
                  endDate = "2016-10-01",
                  useLabels = FALSE) # You can set this option 
                                     # globally or pass it to this
                                     # function.
```

Note that your date and time settings may not correspond to your own timezone. You can find out how to do this [here](https://www.qualtrics.com/support/survey-platform/getting-started/managing-your-account/#user-settings). See [this page](https://api.qualtrics.com/docs/dates-and-times) for more information about how Qualtrics handles dates and times. **Keep in mind that this is important if you plan on using times / dates as cut-off points to filter data**.

You may also reference a response ID. `getSurvey` will then download all responses that were submitted after that response:

```r
surv <- getSurvey(survs$id[4],
                  lastResponseId = "R_3mmovCIeMllvsER",
                  useLabels = FALSE,
                  verbose = TRUE)
```

You can filter a survey for specific questions:

```r
# Retrieve questions for a survey
questions <- getSurveyQuestions(surveyID = surveys$id[6])
# Retrieve a single survey, filtering for questions I want.
mysurvey <- getSurvey(surveyID = surveys$id[6],
                      save_dir = tempdir(),
                      includedQuestionIds = c("QID1", "QID2", "QID3"),
                      verbose=TRUE)
```

Note that dates are converted without a specific timezone in mind. You can specify your own timezone using [these instructions](https://www.qualtrics.com/support/survey-platform/getting-started/managing-your-account/).

You can store the results in a specific location if you like:

```r
mysurvey <- getSurvey(surveyID = surveys$id[6], 
                      save_dir = "/users/jasper/desktop/", 
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

To avoid special characters (mainly periods) in header names, `readSurvey` uses question labels as the header names. The question belonging to that label is then added using the [sjlabelled](https://CRAN.R-project.org/package=sjlabelled) package. Qualtrics gives names to these labels automatically, but you can easily change them.

![](https://raw.githubusercontent.com/JasperHG90/qualtRics/master/img/qualtricsdf.png)

## Other information

For specific information about the Qualtrics API, you can refer to the [official documentation](https://api.qualtrics.com/docs/overview).

### Issues

Should you encounter any bugs or issues, please report them [here](https://github.com/JasperHG90/qualtRics/issues)

### Requests

If you have a request (like adding a new argument), please leave it as an issue  [here](https://github.com/JasperHG90/qualtRics/issues)

### Contributing

Contributions are welcome from anyone subject to the following rules:

- Abide by the [code of conduct](CONDUCT.md).
- Follow the style guide from Hadley Wickham's [R Packages](http://r-pkgs.had.co.nz)
- All contributions must be consistent with the package license (GPL-3)
- Submit contributions as a pull request. Clearly state what the changes are and try to keep the number of changes per pull request as low as possible.

### Related work

- [Jason Bryer](https://github.com/jbryer/qualtrics) wrote an R package to work with the previous version of the Qualtrics API
- [QualtricsTools](https://github.com/emmamorgan-tufts/QualtricsTools/) creates automatic reports in shiny.
- [qsurvey](https://github.com/jamesdunham/qsurvey) by James Dunham focuses on testing and review of surveys before fielding, and analysis of responses afterward.

### News and changes

View news about qualtRics [here](https://github.com/JasperHG90/qualtRics/blob/master/news.md)

### Thanks!

Thanks to everyone who lets me know about issues, bugs etc. I appreciate your help a lot. Special thanks to those who add code! h/t @phoebewong, @samuelkaminsky, @eknud, @strengejacke, Adrian Brugger and Stefan Borer. 
