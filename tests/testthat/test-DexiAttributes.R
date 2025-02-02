test_that("Creating simple DexiAttribute", {
  a <- DexiAttribute("name", "descr")
  expect_equal(a$name, "name")
  expect_equal(a$description, "descr")
  expect_null(a$link)
  expect_null(a$parent)
  expect_null(a$scale)
  expect_null(a$funct)
  expect_silent(a$verify())
  expect_equal(a$ninp(), 0)
  expect_equal(a$count(), 0)
  expect_null(a$dim())
  expect_null(a$model())
  expect_false(a$is_discrete())
  expect_false(a$is_continuous())
})

test_that("Creating complex DexiAttribute", {
  ai <- DexiAttribute("name0", "descr0")
  expect_silent(ai$verify())
  a <- DexiAttribute("name", "descr", parent = ai, inputs = list(ai, ai), link = ai)
  expect_silent(a$verify())
  expect_equal(a$name, "name")
  expect_equal(a$description, "descr")
  expect_equal(a$parent, ai)
  expect_equal(a$inputs, list(ai, ai))
  expect_equal(a$link, ai)
  expect_null(a$scale)
  expect_null(a$funct)
  expect_equal(a$ninp(), 2)
  expect_equal(a$count(), 2)
  expect_null(a$model())
  expect_equal(a$dim(), c(NA, NA))
  expect_false(a$is_discrete())
  expect_false(a$is_continuous())
})

test_that("DexiAttribute methods", {
  ai1 <- DexiAttribute("name1", "descr1")
  ai2 <- DexiAttribute("name2", "descr2")
  ai3 <- DexiAttribute("name3", "descr3")
  al <- DexiAttribute("link", "link")
  ap <- DexiAttribute("parent", "parent")
  expect_silent(ai1$verify())
  expect_silent(ai2$verify())
  expect_silent(ai3$verify())
  expect_silent(al$verify())
  expect_silent(ap$verify())
  a <- DexiAttribute("name", "descr", parent = ap, inputs = c(ai1, ai2, ai3), link = al)
  ai1$parent <- a
  ai2$parent <- a
  ai3$parent <- a
  ap$inputs <- list(a)

  expect_silent(a$verify())
  expect_equal(a$name, "name")
  expect_equal(a$description, "descr")
  expect_equal(a$parent, ap)
  expect_equal(a$inputs, list(ai1, ai2, ai3))
  expect_equal(a$link, al)
  expect_null(a$scale)
  expect_null(a$funct)
  expect_equal(a$ninp(), 3)
  expect_equal(a$count(), 3)
  expect_null(a$model())
  expect_equal(a$dim(), c(NA, NA, NA))
  expect_equal(a$parent, ap)
  expect_null(ap$parent)

  expect_true(ai1$is_basic())
  expect_false(ai1$is_aggregate())
  expect_false(ai1$is_link())
  expect_identical(ai1$parent, a)

  expect_false(a$is_basic())
  expect_true(a$is_aggregate())
  expect_true(a$is_link())

  expect_false(ap$is_basic())
  expect_true(ap$is_aggregate())
  expect_false(ap$is_link())

  expect_true(al$is_basic())
  expect_false(al$is_aggregate())
  expect_false(al$is_link())

  expect_equal(ap$level(), 0)
  expect_equal(a$level(), 1)
  expect_equal(ai2$level(), 2)

  expect_true(a$affects(ap))
  expect_false(a$affects(ai2))
  expect_false(a$affects(al))

  expect_true(ai3$affects(a))
  expect_true(ai3$affects(ap))
  expect_false(ai3$affects(al))

  model <- DexiModel(name = "model", root = ap)
  ap$parent <- model

  expect_identical(ai3$model(), model)
  expect_identical(a$model(), model)
  expect_identical(ap$model(), model)

  expect_equal(a$inp_index(ai1), 1)
  expect_equal(a$inp_index(ai2), 2)
  expect_equal(a$inp_index(ai3), 3)
  expect_identical(a$inp_index(ap), NA)
  expect_equal(ap$inp_index(a), 1)
  expect_identical(ap$inp_index(ai1), NA)

  expect_equal(ap$tree_indent(), "")
  expect_equal(a$tree_indent(), "+")
  expect_equal(ai1$tree_indent(), " *")
  expect_equal(ai2$tree_indent(), " *")
  expect_equal(ai3$tree_indent(), " +")
  expect_equal(al$tree_indent(), "")
})

test_that("att_names() works", {
  ai1 <- DexiAttribute("name1", "descr1")
  ai2 <- DexiAttribute("name2", "descr2")
  ai3 <- DexiAttribute("name3", "descr3")
  ap <- DexiAttribute("parent", "parent")
  a <- DexiAttribute("name", "descr", id = "name_id", parent = ap, inputs = c(ai1, ai2, ai3))
  ai1$parent <- a
  ai2$parent <- a
  ai3$parent <- a
  ap$inputs <- list(a)

  expect_identical(att_names(NULL), list())
  expect_identical(att_names(c(NULL, NULL)), list())
  expect_identical(att_names(NA), NA)
  expect_identical(att_names(c(NA, NA)), c(NA, NA))
  expect_identical(att_names(c(NA, NULL)), c(NA))
  expect_identical(att_names(c(NULL, NA)), c(NA))
  expect_equal(att_names(ai1, use_id = FALSE), "name1")
  expect_equal(att_names(ai2, use_id = FALSE), "name2")
  expect_equal(att_names(a, use_id = FALSE), "name")
  expect_equal(att_names(a), "name_id")
  expect_equal(att_names(c(ai1, ai2, ai3), use_id = FALSE), c("name1", "name2", "name3"))
})
