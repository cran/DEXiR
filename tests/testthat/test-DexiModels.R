test_that("Reading DEXi models from Xml", {
  expect_silent(CarDxi <<- read_dexi(CarXml))
  expect_silent(Car2Dxi <<- read_dexi(Car2Xml))
  expect_silent(LinkedDxi <<- read_dexi(LinkedXml))
  expect_silent(ContinuousOldDxi <<- read_dexi(ContinuousOldXml))
  expect_silent(ContinuousNewDxi <<- read_dexi(ContinuousNewXml))
  expect_silent(ContinuousNewDxiNoAlt <<- read_dexi(ContinuousNewXmlNoAlt))
})

test_that("Reading of DEXi models succeeded", {
  expect_true(inherits(CarDxi, DexiModelClass))
  expect_true(inherits(Car2Dxi, DexiModelClass))
  expect_true(inherits(LinkedDxi, DexiModelClass))
  expect_true(inherits(ContinuousOldDxi, DexiModelClass))
  expect_true(inherits(ContinuousNewDxi, DexiModelClass))

  expect_equal(CarDxi$att_stat(), list(all=11, basic=6, aggregate=4, link=0))
  expect_equal(Car2Dxi$att_stat(), list(all=16, basic=9, aggregate=6, link=0))
  expect_equal(LinkedDxi$att_stat(), list(all=11, basic=6, aggregate=4, link=4))
  expect_equal(ContinuousOldDxi$att_stat(), list(all=6, basic=2, aggregate=3, link=0))
  expect_equal(ContinuousNewDxi$att_stat(), list(all=6, basic=2, aggregate=3, link=0))
})

test_that("DexiModel methods on Car2Dxi", {
  m <- Car2Dxi
  expect_equal(m$name, "DEXi Model")
  expect_equal(m$description, "")
  expect_false(m$linking)
  expect_equal(att_names(m$root), "root")
  expect_equal(att_names(m$first()), "CAR")
  expect_equal(m$att_index("PRICE"), 3)
  expect_equal(m$att_index("PRICE", use_id = FALSE), c(3, 13))
  expect_equal(m$att_index(c("PRICE", "SAFETY")), c(3, 11))
  expect_equal(m$attrib("PRICE"), m$attributes[[3]])
  expect_true(is_empty(m$attrib("PRICELESS")))

  a <- m$attrib(list(2, 3, NA, "PRICE", -1, "PRICEx"))
  expect_equal(att_names(a), c("CAR", "PRICE", NA, "PRICE", NA, NA))
})

test_that("DexiModel methods on LinkedDxi", {
  m <- LinkedDxi
  expect_equal(m$name, "DEXi Model")
  expect_equal(m$description, "")
  expect_true(m$linking)
  expect_equal(att_names(m$root), "root")
  expect_equal(m$att_index("A"), 4)
  expect_equal(m$att_index("A", use_id = FALSE), c(4, 7, 10))
  expect_equal(m$attrib("A"), m$attributes[[4]])
  expect_identical(m$attrib("A.2"), m$attributes[[10]])
  expect_equal(m$attrib("-A"), NA)
  expect_identical(m$attrib("A")$link, m$attrib("A.2"))
  expect_identical(m$attrib("A.1")$link, m$attrib("A.2"))

  a <- m$attrib("MAX")
  expect_false(is.null(a))
  expect_false(identical(a, NA))
  expect_false(is.null(a$scale))
  expect_false(is.null(a$funct))

  a <- m$attrib("A")
  expect_false(is.null(a))
  expect_false(identical(a, NA))
  expect_false(is.null(a$scale))
  expect_true(is.null(a$funct))

  a <- m$attrib("~A")
  expect_false(is.null(a))
  expect_true(identical(a, NA))
  expect_true(is.na(a))

  a <- m$attrib(c("MAX", "MIN"))
  expect_true(is.list(a))
  expect_equal(length(a), 2)
  expect_equal(a[[1]]$id, "MAX")
  expect_equal(a[[2]]$id, "MIN")

  a <- m$attrib(c("A", "undef"))
  expect_true(is.list(a))
  expect_equal(length(a), 2)
  expect_equal(a[[1]]$id, "A")
  expect_equal(a[[2]], NA)

  a <- m$attrib(list(2, 3, NA, "MAX", -1, "MAXx"))
  expect_equal(att_names(a), c("LinkedBoundsTest", "MIN", NA, "MAX", NA, NA))

})

