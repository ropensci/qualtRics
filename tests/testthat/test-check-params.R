context("Arguments should be properly checked")

test_that("logical params should throw error if any not logical", {
  expect_error(check_params(
    verbose = "a",
    convert = TRUE,
    import_id = TRUE,
    label = TRUE,
    include_display_order=TRUE
  ))

  expect_error(check_params(
    verbose = TRUE,
    convert = "a",
    import_id = 123,
    label = "hello",
    include_display_order="yay"
  ))

  expect_error(check_params(
    verbose = TRUE,
    convert = TRUE,
    import_id = 123,
    label = TRUE,
    include_display_order=TRUE
  ))

  expect_error(check_params(
    verbose = TRUE,
    convert = TRUE,
    import_id = TRUE,
    label = 123,
    include_display_order=TRUE
  ))

  expect_error(check_params(
    verbose = TRUE,
    convert = TRUE,
    import_id = TRUE,
    label = TRUE,
    include_display_order=123
  ))
})

test_that("should error if save_dir does not exist", {
expect_error(check_params(
    convert = FALSE,
    save_dir="/i/do/not/exist123"
  ))
})

test_that("unanswer_recode should error if not integer-like", {
  expect_error(check_params(
    convert = FALSE,
    unanswer_recode="a1"
  ))

  expect_error(check_params(
    convert = FALSE,
    unanswer_recode=1.123
  ))
})

test_that("unanswer_recode should not error if integer-like", {
  expect_true(is.null(check_params(
    convert = FALSE,
    unanswer_recode=123
  )))
})

test_that("unanswer_recode_multi should error if not integer-like", {
  expect_error(check_params(
    convert = FALSE,
    unanswer_recode_multi="a1"
  ))

  expect_error(check_params(
    convert = FALSE,
    unanswer_recode_multi=1.123
  ))
})

test_that("unanswer_recode_multi should not error if integer-like", {
  expect_true(is.null(check_params(
    convert = FALSE,
    unanswer_recode_multi=123
  )))
})

test_that("limit should error if less than 0", {
  expect_error(check_params(
    convert = FALSE,
    limit=-1
  ))
})


test_that("include_questions should error if not a string", {
  expect_error(check_params(
    convert = FALSE,
    include_questions=-1
  ))
})