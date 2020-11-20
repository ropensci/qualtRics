context("Get metadata for a survey")

qualtrics_api_credentials(api_key = "1234", base_url = "t.qualtrics.com")

test_that("metadata() should throw an error if passing invalid options to input", {

  expect_error(
    metadata("mockId1", get = list(invalidKey = TRUE))
  )

})

test_that("metadata() should throw warning if input questions are not a vector", {

  expect_error(
    metadata("mockId1", questions = "I am not a vector")
  )

})

test_that("metadata() should return metadata + questions + responsecounts", {

  vcr::use_cassette("metadata", {
    x <- metadata("SV_3gbwq8aJgqPwQDP")
  })

  expect_type(x, c("list"))
  expect_named(x, c("metadata", "questions", "responsecounts"))
  expect_equal(length(x$questions), 6)
  expect_equal(length(x$responsecounts), 3)
  expect_s3_class(x$metadata, "data.frame")

})


test_that("metadata() returns flow if specified", {

  vcr::use_cassette("metadata_flow", {
    x <- metadata("SV_3gbwq8aJgqPwQDP", get = list(flow = TRUE))
  })

  expect_type(x, c("list"))
  expect_named(x, c("metadata", "questions", "responsecounts", "flow"))
  expect_equal(length(x$questions), 6)
  expect_equal(length(x$responsecounts), 3)
  expect_s3_class(x$metadata, "data.frame")
  expect_equal(length(x$flow), 4)
})
