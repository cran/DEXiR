test_that("is_single() works", {
  expect_true(is_single(NA))
  expect_true(is_single(""))
  expect_true(is_single("a"))
  expect_true(is_single(c("a")))
  expect_true(is_single(1))
  expect_true(is_single(c(1)))
  expect_true(is_single(list(a=1)))

  expect_false(is_single(NULL))
  expect_false(is_single(c(1,2)))
  expect_false(is_single(c("a", "b")))
  expect_false(is_single(list("a", "b")))
})

test_that("is_all_integer() works", {
  expect_true(is_all_integer(1))
  expect_true(is_all_integer(c(1, 2)))
  expect_true(is_all_integer(c(1.0, 0.0, 2)))
  expect_true(is_all_integer(c(1.0, 0.0, NA, 2)))
  expect_true(is_all_integer(NA))

  expect_false(is_all_integer(1.1))
  expect_false(is_all_integer(c(1.1, 2, 3)))
  expect_false(is_all_integer(NA, na.rm = FALSE))
  expect_false(is_all_integer(c(1,NA), na.rm = FALSE))
})

test_that("is_single_character() works", {
  expect_true(is_single_character(""))
  expect_true(is_single_character("a"))

  expect_false(is_single_character(NULL))
  expect_false(is_single_character(1))
  expect_false(is_single_character(c(1, 2)))
  expect_false(is_single_character(list()))
  expect_false(is_single_character(NA))
  expect_false(is_single_character(c("a", "b")))
})

test_that("is_single_character_or_null() works", {
  expect_true(is_single_character_or_null(NULL))
  expect_true(is_single_character_or_null(""))
  expect_true(is_single_character_or_null("a"))

  expect_false(is_single_character_or_null(1))
  expect_false(is_single_character_or_null(c(1, 2)))
  expect_false(is_single_character_or_null(list()))
  expect_false(is_single_character_or_null(NA))
  expect_false(is_single_character_or_null(c("a", "b")))
})


test_that("is_single_numeric() works", {
  expect_true(is_single_numeric(0))
  expect_true(is_single_numeric(1.1))
  expect_true(is_single_numeric(c(2.2)))

  expect_false(is_single_numeric(""))
  expect_false(is_single_numeric("a"))
  expect_false(is_single_numeric(NULL))
  expect_false(is_single_numeric(NA))
  expect_false(is_single_numeric(c(1, 2)))
  expect_false(is_single_numeric(list(1, 2)))
  expect_false(is_single_numeric(list()))
  expect_false(is_single_numeric(c("a", "b")))
})

test_that("is_empty() works", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(c()))
  expect_true(is_empty(list()))
  expect_true(is_empty(c(NULL, NULL)))
  expect_true(is_empty(c(NULL, NA)))

  expect_false(is_empty(c(NA, NA)))
  expect_false(is_empty(c(3)))
  expect_false(is_empty(list(2)))
  expect_false(is_empty(list(2, 3)))
})

test_that("is_class() works", {
  x <- structure(1, class = "TestClass")
  expect_true(is_class(x, "TestClass"))
  expect_true(is_class(x, c("TestClass")))
  expect_true(is_class(x, c("TestClass", "OtherClass")))
  expect_true(is_class(x, c("TestClass")))
  expect_true(is_class(x, c("TestClass", "OtherClass")))
  expect_true(is_class(x, c("OtherClass", "TestClass")))

  expect_error(is_class(NULL))
  expect_error(is_class(NULL, NULL))
  expect_error(is_class(x))
  expect_error(is_class(x, 1))
  expect_error(is_class(x, c(1, 2)))
  expect_false(is_class(NULL, "Whatever"))
  expect_false(is_class(x, "OtherClass"))
  expect_false(is_class(x, c("a", "b")))
})

test_that("is_class_or_null() works", {
  x <- structure(1, class = "TestClass")
  expect_true(is_class_or_null(NULL))
  expect_true(is_class_or_null(NULL, NULL))
  expect_true(is_class_or_null(NULL, "Whatever"))
  expect_true(is_class_or_null(x, "TestClass"))
  expect_true(is_class_or_null(x, c("TestClass")))
  expect_true(is_class_or_null(x, c("TestClass", "OtherClass")))
  expect_true(is_class_or_null(x, c("TestClass")))
  expect_true(is_class_or_null(x, c("TestClass", "OtherClass")))
  expect_true(is_class_or_null(x, c("OtherClass", "TestClass")))

  expect_error(is_class_or_null(x))
  expect_error(is_class_or_null(x, 1))
  expect_error(is_class_or_null(x, c(1, 2)))
  expect_false(is_class_or_null(x, "OtherClass"))
  expect_false(is_class_or_null(x, c("a", "b")))
})

test_that("catch_error() works", {
  expect_equal(catch_error(1), 1)
  expect_equal(catch_error(1 + 1), 2)
  expect_null(catch_error(x))
  expect_null(catch_error(x[[1]]))
  expect_equal(catch_error(x, on_error = 99), 99)
})

test_that("stopif() works", {
  expect_error(stopif(TRUE))
  expect_silent(stopif(FALSE))
})

test_that("seq_increasing() works", {
  expect_null(seq_increasing(1, 0))
  expect_equal(seq_increasing(1, 0), c())
  expect_null(seq_increasing(5, 2))
  expect_equal(seq_increasing(1, 1), 1)
  expect_equal(seq_increasing(1, 3), 1:3)
})

test_that("seq_decreasing() works", {
  expect_null(seq_decreasing(0, 1))
  expect_equal(seq_decreasing(5, 8), c())
  expect_null(seq_decreasing(5, 8))
  expect_equal(seq_decreasing(1, 1), 1)
  expect_equal(seq_decreasing(3, 1), 3:1)
})

test_that("is_in_range() works", {
  expect_false(is_in_range(-1, 0, 2))
  expect_true(is_in_range(1, 0, 2))
  expect_true(is_in_range(0, 0, 2))
  expect_true(is_in_range(2, 0, 2))
  expect_false(is_in_range(3, 0, 2))

  expect_true(is_in_range(0, 0, 2, lassoc = "up", hassoc = "down"))
  expect_true(is_in_range(0, 0, 2, lassoc = "up", hassoc = "up"))
  expect_false(is_in_range(0, 0, 2, lassoc = "down", hassoc = "up"))
  expect_true(is_in_range(2, 0, 2, lassoc = "up", hassoc = "down"))
  expect_false(is_in_range(2, 0, 2, lassoc = "down", hassoc = "up"))

  expect_error(is_in_range(2, 0, 2, lassoc = "down", hassoc = "x"))
})

test_that("unique_names() works", {
  expect_true(is.na(unique_names(NULL)))
  expect_equal(unique_names(c("a", "b", "c")), c("a", "b", "c"))
  expect_equal(unique_names(c("a", "a", "b", "c")), c("a", "a.1", "b", "c"))
  expect_equal(unique_names(c("a", "b", "c"), reserved = "a"), c("a.1", "b", "c"))
})

test_that("flat_text() works", {
  expect_equal(flat_text(NULL), "")
  expect_equal(flat_text("a b"), "a b")
  expect_equal(flat_text(c("a","b")), "a b")
  expect_equal(flat_text(c(1, 2.2)), "1 2.2")
  expect_equal(flat_text(c(1, NA, 2.2)), "1 NA 2.2")
})

test_that("lin_map() works", {
  expect_equal(lin_map(2, 1, 3), 0.5)
  expect_equal(lin_map(1, 1, 3), 0)
  expect_equal(lin_map(3, 1, 3), 1)
  expect_equal(lin_map(4, 1, 3), 1.5)
  expect_equal(lin_map(4, 1, 3, 1, 5), 7)
  expect_equal(lin_map(2, 1, 3, 1, 5), 3)
  expect_equal(lin_map(1, 1, 3, 1, 5), 1)
  expect_equal(lin_map(1, 1, 3, 5, 1), 5)
})

test_that("reverse_value() works", {
  expect_equal(reverse_value(1, 1, 5), 5)
  expect_equal(reverse_value(3, 1, 5), 3)
  expect_equal(reverse_value(5, 1, 5), 1)
  expect_equal(reverse_value(c(1, 3, 5), 1, 5), c(5, 3, 1))
})
