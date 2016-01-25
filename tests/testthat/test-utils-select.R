context("utils-select")

choices <- c("a", "b", "c")

test_that("update selection works", {
  expect_null(update_selected("d", choices))
  expect_null(update_selected("d", NULL))
  expect_equal(update_selected(c("a", "b"), choices), c("a", "b"))
  expect_equal(update_selected(c("a", "b", "d"), choices), c("a", "b"))
  expect_equal(update_selected("d", choices, index = 1), "a")
})
