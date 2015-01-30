context("%operators%")

test_that("nin does what it suggests", {
  expect_that(1 %nin% 2:4, is_true())
  expect_that("a" %nin% letters[2:4], is_true())
})

t1 <- c(letters[1:5], NA)
c1 <- c(letters[4:7], NA)
t2 <- c(1:5, NA)
c2 <- c(4:6, NA)

test_that("nin is !in", {
  expect_that(t1 %nin% c1, is_identical_to(! t1 %in% c1))
  expect_that(t2 %nin% c2, is_identical_to(! t2 %in% c2))
})
