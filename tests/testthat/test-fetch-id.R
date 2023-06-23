# create toy all_surveys() tibble for testing
surveys <-
  tidyr::tribble(
    ~id, ~name, ~ownerId, ~lastModified, ~creationDate, ~isActive,
    "SV_yvele4xrSM5GrN8", "Unique Survey Name", "UR_5ioX3NU4dw6x7UX", "2021-09-13", "2021-09-13", TRUE,
    "SV_N8Blxl84lCgRdQW", "Copied Survey Name", "UR_5ioX3NU4dw6x7UX", "2021-09-13", "2021-09-13", TRUE,
    "sv_K6C8vmqin3FFABN", "Copied Survey Name", "UR_5ioX3NU4dw6x7UX", "2021-09-13", "2021-09-13", TRUE
  )

test_that("fetch_id() returns a surveyID for a unique survey_name", {

  x <- unname(fetch_id(surveys, "Unique Survey Name"))

  expect_type(x, "character")
  expect_equal(x, "SV_yvele4xrSM5GrN8")

})

test_that("Dataframe passed to fetch_id() matches all_surveys() format", {

  z <-
    tidyr::tribble(
      ~a, ~b, ~c,
      1, TRUE, "One",
      0, FALSE, "Zero"
    )

  expect_error(
    fetch_id(z, "Survey Name"),
    "needs a dataframe",
    fixed = TRUE
  )

})

test_that("survey_name exists", {

  expect_error(
    fetch_id(surveys, "Nonexistant Survey Name"),
    "Please verify that `survey_name` is correct and try again."
  )

})

test_that("survey_name is unique", {

  expect_error(
    fetch_id(surveys, "Copied Survey Name"),
    "Multiple survey IDs matched"
  )

})



