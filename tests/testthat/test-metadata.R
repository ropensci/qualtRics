skip_on_cran()

test_that("metadata() should throw an error if passing invalid options to input", {
  expect_warning(
    expect_error(
      metadata("mockId1", get = list(invalidKey = TRUE))
    )
  )
})

test_that("metadata() should throw error if input questions are not a character vector", {
  expect_error(
    metadata("mockId1", questions = 1:3)
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

test_that("metadata() returns flow if specified in new way (char vec)", {
  vcr::use_cassette("metadata_flow", {
    x <- metadata("SV_3gbwq8aJgqPwQDP", get = "flow")
  })

  expect_type(x, c("list"))
  expect_named(x, "flow")
  expect_equal(length(x$flow), 4)
})

test_that("metadata() ADDS flow if specified in old way (logical list)", {
  expect_warning(
    vcr::use_cassette("metadata_flow", {
      x <- metadata("SV_3gbwq8aJgqPwQDP", get = list(flow = TRUE))
    })
  )

  expect_type(x, c("list"))
  expect_named(x, c("metadata", "questions", "responsecounts", "flow"))
  expect_equal(length(x$questions), 6)
  expect_equal(length(x$responsecounts), 3)
  expect_s3_class(x$metadata, "data.frame")
  expect_equal(length(x$flow), 4)
})
