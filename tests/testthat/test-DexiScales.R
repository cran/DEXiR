test_that("default_quality() works", {
  expect_equal(default_quality("ascending", 1), c("none"))
  expect_equal(default_quality("ascending", 2), c("bad", "good"))
  expect_equal(default_quality("ascending", 3), c("bad", "none", "good"))
  expect_equal(default_quality("descending", 1), c("none"))
  expect_equal(default_quality("descending", 2), c("good", "bad"))
  expect_equal(default_quality("descending", 3), c("good", "none", "bad"))
})

test_that("Creating DexiScale", {
  s <- DexiScale$new()
  expect_silent(s$verify())
  expect_equal(s$order, "ascending")

  s <- DexiScale$new("descending")
  expect_silent(s$verify())
  expect_equal(s$order, "descending")
  expect_true(is.na(s$count()))
  expect_true(is.na(s$value_index(2)))
  expect_equal(s$value_quality(2), "none")

  expect_error(DexiScale$new(order = "wrong_order"))
  expect_error(DexiScale$new(list(1,2)))
})

test_that("Creating DexiContinuousScale", {
  s <- DexiContinuousScale$new()
  expect_silent(s$verify())
  expect_equal(s$order, "ascending")
  expect_equal(s$low_point, -Inf)
  expect_equal(s$high_point, +Inf)
  expect_equal(s$count(), 0)
  expect_true(is.na(s$value_index(2)))
  expect_equal(s$value_quality(2), "none")

  s <- DexiContinuousScale$new(order = "descending", low_point = -1, high_point = 1)
  expect_silent(s$verify())
  expect_equal(s$order, "descending")
  expect_equal(s$low_point, -1)
  expect_equal(s$high_point, +1)
  expect_equal(s$count(), 0)
  expect_true(is.na(s$value_index(2)))
  expect_equal(s$value_quality(2), "bad")

  expect_error(DexiContinuousScale$new(order = "wrong_order"))
  expect_error(DexiContinuousScale$new(list(1,2)))
})


test_that("Creating DexiDiscreteScale", {
  s <- DexiDiscreteScale$new()
  expect_error(s$verify())
  expect_equal(s$order, "ascending")
  expect_equal(s$values, character(0))
  expect_equal(s$descriptions, character(0))
  expect_equal(s$quality, character(0))
  expect_equal(s$count(), 0)
  expect_true(is.na(s$value_index(2)))
  expect_true(is.na(s$value_quality(2)))

  s <- DexiDiscreteScale$new(order = "descending", values = c("a", "b", "c"))
  expect_silent(s$verify())
  expect_equal(s$order, "descending")
  expect_equal(s$values, c("a", "b", "c"))
  expect_equal(s$descriptions, c("", "", ""))
  expect_equal(s$quality, c("good", "none", "bad"))
  expect_equal(s$count(), 3)
  expect_equal(s$value_index("b"), 2)
  expect_equal(s$value_quality(3), "bad")

  s <- DexiDiscreteScale$new(values = list(1,2))
  expect_silent(s$verify())

  s <- DexiDiscreteScale$new(values = integer(0))
  expect_error(s$verify())

  s <- DexiDiscreteScale$new(values = NULL)
  expect_error(s$verify())

  expect_error(DexiDiscreteScale$new(order = "wrong_order"))
})


test_that("DexiScale$scale_count() works", {
  s <- DexiContinuousScale()
  expect_equal(s$count(), 0)

  s <- DexiDiscreteScale(values = c("a", "b"))
  expect_equal(s$count(), 2)

  s <- DexiDiscreteScale(values = c("a", "b", "c"))
  expect_equal(s$count(), 3)
})

test_that("DexiScale$value_index() works", {
  s <- DexiContinuousScale()
  expect_true(is.na(s$value_index(2)))

  s <- DexiDiscreteScale(values = c("a", "b"))
  expect_equal(s$value_index("a"), 1)
  expect_equal(s$value_index("b"), 2)
  expect_true(is.na(s$value_index("ab")))

  s <- DexiDiscreteScale(values = c("a", "b", "ab"))
  expect_equal(s$value_index("a"), 1)
  expect_equal(s$value_index("b"), 2)
  expect_equal(s$value_index("ab"), 3)
  expect_true(is.na(s$value_index("ba")))
  expect_error(is.na(s$value_index()))
  expect_true(is.na(s$value_index("")))
  expect_true(is.na(s$value_index(NULL)))
  expect_true(is.na(s$value_index(TRUE)))
})

test_that("DexiCountinuousScale$value_quality() works", {
  s <- DexiContinuousScale()
  expect_equal(s$value_quality(0), "none")
  expect_equal(s$value_quality(-Inf), "none")
  expect_equal(s$value_quality(+Inf), "none")
  expect_equal(s$value_quality("med"), NA)

  s <- DexiContinuousScale(order = "ascending", low_point = -1, high_point = 1)
  expect_equal(s$value_quality(0), "none")
  expect_equal(s$value_quality(-Inf), "bad")
  expect_equal(s$value_quality(-1.1), "bad")
  expect_equal(s$value_quality(-1), "none")
  expect_equal(s$value_quality(+1), "none")
  expect_equal(s$value_quality(+1.1), "good")
  expect_equal(s$value_quality(+Inf), "good")

  s <- DexiContinuousScale(order = "descending", low_point = -1, high_point = 1)
  expect_equal(s$value_quality(0), "none")
  expect_equal(s$value_quality(-Inf), "good")
  expect_equal(s$value_quality(-1.1), "good")
  expect_equal(s$value_quality(-1), "none")
  expect_equal(s$value_quality(+1), "none")
  expect_equal(s$value_quality(+1.1), "bad")
  expect_equal(s$value_quality(+Inf), "bad")
})

test_that("DexiDiscreteScale$value_quality() works", {
  s <- DexiDiscreteScale(values = c("low", "med", "high"))
  expect_equal(s$value_quality(1), "bad")
  expect_equal(s$value_quality(2), "none")
  expect_equal(s$value_quality(3), "good")
  expect_equal(s$value_quality("low"), "bad")
  expect_equal(s$value_quality("med"), "none")
  expect_equal(s$value_quality("high"), "good")
  expect_true(is.na(s$value_quality(0)))
  expect_true(is.na(s$value_quality(4)))
  expect_true(is.na(s$value_quality("undef")))
})
