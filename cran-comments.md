## Release summary

This is effectively a new submission, as qualtRics was previously archived on CRAN with a different maintainer. This release for qualtRics is the 7th, if we include the previous version on CRAN before archiving. This new release transfers the package to a new maintainer (Julia Silge), adds all previous contributors to DESCRIPTION as `ctb`, and relicenses from GPL-3 to MIT. See [consent from authors here](https://github.com/ropensci/qualtRics/issues/95).

Other changes include declaring the testthat dependency in DESCRIPTION (the reason for previous archiving from CRAN), new handling for API authentication, and changes to functions for handling returned Qualtrics data.

## Test environments

* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission of a package that was previously archived.
* "Qualtrics" is noted as a possibly misspelled word, but is correct.
* Qualtrics is the first word of the `Description` field, in reference to the survey platform rather than the R package.
