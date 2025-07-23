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

test_that("passing a list to elements when legacy = TRUE succeeds with warning ", {

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

  expect_warning(
    vcr::use_cassette("fetch_description_legacy", {
      x <- fetch_description("SV_6s93xhVtm1e4j3v",
                             legacy = TRUE,
                             elements = element_list
      )
    }),
    "Use of logical lists"
  )

  expect_type(x, "list")
  expect_named(x, c("metadata", "responsecounts", "flow", "comments"))

})



test_that("write_qsf saves a properly formatted qsf file", {

  # Outputs invisible:
  vcr::use_cassette("write_qsf", {
    x <-
      expect_invisible(
        write_qsf("SV_6s93xhVtm1e4j3v",
                  file = "test_qsf.qsf")
      )
  })

  # Re-loads properly:
  expect_type(
    jsonlite::read_json("test_qsf.qsf"),
    "list"
  )
  # clean up:
  invisible(file.remove("test_qsf.qsf"))

  # Output visible w/o writing:
  vcr::use_cassette("write_qsf", {
    x_pretty <-
      expect_visible(
        write_qsf("SV_6s93xhVtm1e4j3v",
                  formatting = "pretty",
                  save = FALSE)
      )
  })

  # Prettification worked (got longer)
  expect_gt(
    nchar(x_pretty),
    nchar(x)
  )

})

