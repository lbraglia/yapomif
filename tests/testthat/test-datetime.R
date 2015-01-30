context("Date and time functions")

test_that("dateMDY, Months, Days, Years", {
  today <- Sys.Date()
  ## now <- Sys.time()
  Today <- dateMDY(Months(today, string=FALSE),
                   Days(today),
                   Years(today))

  expect_that(today, is_identical_to(Today))
  ## expect_that(now, is_identical_to(Now))
})
