context("Compiled code and wrapper functions")

## -----------
## groupProgID
## -----------
test_that("groupProgID works", {

  ## integers (with NA) 
  expect_identical(groupProgID(rep( c(1:3, NA), each = 3)),
                   c(rep(c(1:3), 3), rep(NA, 3)))

})
