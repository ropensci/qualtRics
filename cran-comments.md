## Release summary

There was some feedback on my initial submission. I have changed the DESCRIPTION file as instructed to use single quotes around 'Qualtrics' and avoid starting with this word. The other two points brought up I believe are not things I can't change.

- CRAN commented on how `\dontrun{}` is used for examples. These examples access the API and require credentials, so cannot be run on CRAN machines. This is why they are surrounded by `\dontrun{}`.
- CRAN reminded me to ensure that the package does not write to the user's home filespace. No function, vignette, or any part of this package writes to the home filespace; only tempdir() is used.


**Previous comments here:**

This is effectively a new submission, as qualtRics was previously archived on CRAN with a different maintainer. This release for qualtRics is the 7th, if we include the previous versions on CRAN before archiving. This new release transfers the package to a new maintainer (Julia Silge), adds all previous contributors to DESCRIPTION as `ctb`, and relicenses from GPL-3 to MIT. See [consent from authors here](https://github.com/ropensci/qualtRics/issues/95).

Other changes include declaring the testthat dependency in DESCRIPTION (the reason for previous archiving from CRAN), new handling for API authentication, and changes to functions for handling returned Qualtrics data.


## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
