
test_that("all_mailinglists returns a tbl_df with expected column names and types", {

  vcr::use_cassette("all_mailinglists", {
    x <- all_mailinglists()
  })

  expect_that(x, is_a("tbl_df"))
  expect_that(attr(x, "class"), is_identical_to(c("tbl_df","tbl","data.frame")))
  expect_that(names(x), is_identical_to(c("libraryId", "id", "name", "category", "folder")))
  expect_true(class(x$libraryId) == "character")
  expect_true(class(x$id) == "character")
  expect_true(class(x$name) == "character")
  expect_true(class(x$category) == "character")
  expect_true(class(x$folder) == "character")

})
