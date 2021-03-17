context("fetch_descriptionI(), updating metadata() to v3, with legacy access")

test_that("fetch_description() retrieves an appropriate survey description", {

  vcr::use_cassette("fetch_description", {
    x <- fetch_description("SV_6s93xhVtm1e4j3v")
  })

  expect_type(x, "list")
  expect_named(x, c("metadata", "surveyoptions", "flow", "blocks", "questions",
                    "responsesets", "scoring"))
  expect_s3_class(x$metadata, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(x$surveyoptions, c("tbl_df", "tbl", "data.frame"))
  expect_type(x$flow, "list")
  expect_type(x$blocks, "list")
  expect_type(x$questions, "list")
  expect_type(x$responsesets, "list")
  expect_type(x$scoring, "list")

})


test_that("fetch_description() with elements set works", {

  vcr::use_cassette("fetch_description", {
    x <- fetch_description("SV_6s93xhVtm1e4j3v",
                           elements = c("surveyoptions", "flow", "blocks"))
  })

  expect_type(x, "list")
  expect_named(x, c("surveyoptions", "flow", "blocks"))


})

test_that("fetch_description() with legacy = TRUE reverts to metadata()", {

  vcr::use_cassette("fetch_description_legacy", {
    x <- fetch_description("SV_6s93xhVtm1e4j3v", legacy = TRUE)
  })

  expect_type(x, "list")
  expect_named(x, c("metadata", "questions", "responsecounts"))
  expect_s3_class(x$metadata, "data.frame")
  expect_type(x$questions, "list")
  expect_s3_class(x$responsecounts, "data.frame")

})


test_that("setting elements works with legacy = TRUE ", {


  vcr::use_cassette("fetch_description_legacy", {
    x <- fetch_description("SV_6s93xhVtm1e4j3v",
                           legacy = TRUE,
                           elements = c("metadata", "questions",
                                        "responsecounts", "blocks",
                                        "flow", "embedded_data", "comments")
                           )
  })

  expect_type(x, "list")
  expect_named(x, c("metadata", "questions",
                    "responsecounts", "blocks",
                    "flow", "embedded_data", "comments"))
  expect_s3_class(x$metadata, "data.frame")
  expect_type(x$questions, "list")
  expect_s3_class(x$responsecounts, "data.frame")
  expect_type(x$blocks, "list")
  expect_type(x$flow, "list")
  expect_type(x$embedded_data, "list")
  expect_type(x$comments, "list")

})

test_that("passing a list to elements when legacy = TRUE succeeds with message ", {

  element_list <-
    list(
      "metadata" = TRUE,
      "questions" = FALSE,
      "responsecounts" = TRUE,
      "blocks" = FALSE,
      "flow" = TRUE,
      "embedded_data" = FALSE,
      "comments" = TRUE
    )

 expect_message(
    vcr::use_cassette("fetch_description_legacy", {
     x <- fetch_description("SV_6s93xhVtm1e4j3v",
                             legacy = TRUE,
                             elements = element_list
      )
    }),
    "Use of logical lists for argument"
  )

  expect_type(x, "list")
  expect_named(x, c("metadata", "responsecounts", "flow", "comments"))

})

