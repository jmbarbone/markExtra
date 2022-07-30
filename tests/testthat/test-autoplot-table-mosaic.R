test_that("autoplot_table_mosaic() doesn't fail", {
  x <- as.table(
    matrix(
      c(1, 2, 3, 4),
      nrow = 2,
      dimnames = list(a = list(TRUE, FALSE), b = list(TRUE, FALSE))
    )
  )

  expect_error(autoplot_table_mosaic(x), NA)
})
