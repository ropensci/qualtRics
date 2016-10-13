# qualtRics

This project contains an R package to interact with the [Qualtrics](https://www.qualtrics.com/) API. It is currently under development. I will add more functionality later.

Note that your institution must support API access and that it must be enabled for your account. Whoever manages your Qualtrics account can help you with this. Please refer to the [Qualtrics documentation](https://api.qualtrics.com/docs/authentication) on how to find your API token.

## Installation

To install this package, execute the following in R:

```r
install.packages("devtools")
devtools::install_github("JasperHG90/qualtRics")
```

## Dependencies

This package depends on `httr` and `stringr`. Both dependencies will be installed when you install `qualtRics`.

## Updates

Periodically check this repository for updates and execute `devtools::install_github("JasperHG90/qualtRics")` to update.

## Usage

Currently, the package contains three functions. It supports fetching a list of courses and their IDs from qualtrics, as well as downloading and reading a survey export. 

Note that, while requests will work without a [base url](https://api.qualtrics.com/docs/root-url), it is desirable that you supply it to the functions when needed. Supplying the correct url will reduce the number of errors you'll experience. Please refer to the [official documentation](https://api.qualtrics.com/docs/root-url) to find out your institution-specific base url.

Note that you can only export surveys that you own, or to which you have been given explicit administration rights.

### Commands

Load the package:

```r
library(qualtRics)
```

Construct the header information:

```r
head <- constructHeader(API.TOKEN = "<yourapitoken>")
```

Get a list of surveys:

```r
surveys <- getSurveys(head, survey_baseurl="https://leidenuniv.eu.qualtrics.com/API/v3/responseexports/") # URL is for my own institution
```

Export a survey and load it into R:

```r
mysurvey <- getSurvey(surveyID = surveys[[6]]$id, headers = head, base_url = "https://leidenuniv.eu.qualtrics.com/API/v3/responseexports/", verbose = TRUE)
```

## Other information

For specific information about the Qualtrics API, you can refer to the [official documentation](https://api.qualtrics.com/docs/overview).

### Issues

Should you encounter any bugs or issues, please report them [here](https://github.com/JasperHG90/qualtRics/issues)

### Requests

If you have a request (like adding a new argument), please leave it [here](https://github.com/JasperHG90/qualtRics/issues)

### Changelog

- Added first three functions (`constructHeader`, `getSurvey`, `getSurveyIDs`)
