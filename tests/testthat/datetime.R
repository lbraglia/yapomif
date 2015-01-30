context("Date and time functions")

today <- Sys.Date()
now <- Sys.Time()
Today <- dateMDY(Months(today, string=FALSE),
                 Days(today),
                 Years(today))
## Now <- 

test_that("dateMDY, Months, Days, Years", {
  expect_that(today, is_identical_to(Today))
  ## expect_that(now, is_identical_to(Now))
})
