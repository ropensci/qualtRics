test_that("logical params should throw error if not TRUE/FALSE", {
  log_NA <- NA
  log_NULL <- NULL
  log_notlog <- "what's that?"

  expect_error(checkarg_isboolean(log_NA), "must be a single `TRUE` or `FALSE`")
  expect_error(checkarg_isboolean(log_NULL), "must be a single `TRUE` or `FALSE`")
  expect_error(checkarg_isboolean(log_notlog), "must be a single `TRUE` or `FALSE`")

})

test_that("integer-likes should error only if present and not integer-like", {
  int_NULL <- NULL
  int_int <- 1
  int_NA <- NA
  int_string <- "what"
  int_nonint <- 1.1
  int_multiple <- c(1,4)

  expect_null(checkarg_isintegerish(int_NULL))
  expect_null(checkarg_isintegerish(int_int))
  expect_error(checkarg_isintegerish(int_NA), "a single integer")
  expect_error(checkarg_isintegerish(int_string), "a single integer")
  expect_error(checkarg_isintegerish(int_nonint), "a single integer")
  expect_error(checkarg_isintegerish(int_multiple), "a single integer")
})



test_that("limit should error if present and not integer >= 1", {
  limit_integer <- 2
  limit_nonint <- 2.5
  limit_nonnum <- "what"
  limit_toolow <- 0

  expect_null(checkarg_limit(limit_integer))
  expect_error(checkarg_limit(limit_nonint), "a single integer")
  expect_error(checkarg_limit(limit_nonnum), "a single integer")
  expect_error(checkarg_limit(limit_toolow), "must be 1 or greater")

})


test_that("include_questions should error if not character vector of QID[nums] w/no missings", {
  qs_NULL <- NULL #acceptable
  qs_NA <- NA #acceptable, will exclude with character()
  qs_qid <- c("QID54", "qid12") #acceptable, case insensitive, will toupper()
  qs_names <- c("Q4", "Income") #wrong, needs QIDs
  qs_missing <- c("QID54", NA, "qid12") #wrong, no missings
  qs_wrongtype <- c(1, 3, 4) #wrong type

  expect_null(checkarg_include_questions(qs_NULL))
  expect_equal(checkarg_include_questions(qs_NA), character())
  expect_equal(checkarg_include_questions(qs_qid), c("QID54", "QID12"))
  expect_error(checkarg_include_questions(qs_names), "Qualtrics internal IDs")
  expect_error(checkarg_include_questions(qs_missing), "missing values")
  expect_error(checkarg_include_questions(qs_wrongtype), "character vector")

})

test_that("include_metadata should error if not character vector of correct names", {
  ms_NULL <- NULL #acceptable
  ms_NA <- NA #acceptable, will exclude with character()
  #acceptable, case insensitive, removed duplicates:
  ms_good <- c("responseid", "RECIPIENTEMAIL", "recipientemail")
  ms_names <- c("responseid", "Income") #wrong, includes nonmatching
  ms_missing <- c("responseid", NA, "recipientemail") #wrong, no missings
  ms_wrongtype <- c(1, 3, 4) #wrong type

  expect_null(checkarg_include_questions(ms_NULL))
  expect_equal(checkarg_include_metadata(ms_NA), character())
  expect_equal(checkarg_include_metadata(ms_good), c("_recordId", "recipientEmail"))
  expect_error(checkarg_include_metadata(ms_names), "invalid names used")
  expect_error(checkarg_include_metadata(ms_missing), "missing values")
  expect_error(checkarg_include_metadata(ms_wrongtype), "character vector")

})

test_that("include_embedded should error if not character vector names", {
  es_NULL <- NULL #acceptable
  es_NA <- NA #acceptable, will exclude with character()
  es_good <- c("consent", "group", "WeekEnd") #acceptable, returns as-is
  es_missing <- c("consent", NA, "WeekEnd") #wrong, no missings
  es_wrongtype <- c(1, 3, 4) #wrong type

  expect_null(checkarg_include_embedded(es_NULL))
  expect_equal(checkarg_include_embedded(es_NA), character())
  expect_equal(checkarg_include_embedded(es_good), es_good)
  expect_error(checkarg_include_embedded(es_missing), "missing values")
  expect_error(checkarg_include_embedded(es_wrongtype), "character vector")

})
