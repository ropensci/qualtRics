---
title: 'qualtRics: retrieve survey data using the Qualtrics API'
authors:
- affiliation: 1
  name: Jasper Ginn
  orcid: 0000-0002-5019-2923
date: "2 February 2018"
output: pdf_document
bibliography: paper.bib
tags:
- R
- survey data
- API
- Qualtrics
affiliations:
- index: 1
  name: Ecole Polytechnique Federale de Lausanne (EPFL)
---

# Summary

Qualtrics [@qualtrics_main] allows users to create and disseminate online surveys. It is used by researchers to field responses for academic research. While users can manually download survey responses from Qualtrics, importing this data into R is cumbersome. The R package `qualtRics` [@ginn_qualtrics_2017] focuses on the retrieval of survey data using the Qualtrics API and aims to reduce the pre-processing steps needed to prepare this data for analysis. Currently, the package is the only package on CRAN that offers such functionality, and is included in the official Qualtrics API documentation [@noauthor_getting_nodate].

The primary goal of the package is to provide a bridge between the Qualtrics user interface (where the survey is designed) and R (where the results can be analyzed) by using as few steps as possible. Users can store their API credentials in a file in an R project root directory that automatically loads when the library is called. This eliminates the need to remember API credentials and prevents the user from accidentally sharing sensitive information should they want to share their work. The package contains three core functions to retrieve survey data. The first of these functions - `getSurveys()` - retrieves a data frame containing an overview of surveys to which the user has access. Using a unique survey id, the user can download and import their data using the second core function (`getSurvey()`). This function takes care of requesting, downloading and unpacking the data. It is then imported into R with the `readSurvey()` function. This last function can also be used to import manual data exports and supports both current and legacy data formats.

Apart from the above functionality, the package supports the automatic conversion of single-choice multiple choice questions. Using the rich metadata that Qualtrics provides about surveys, it is possible to automatically convert ordinal data to ordered factors. This functionality will be expanded on an ongoing basis to include other variable types.    

# References
