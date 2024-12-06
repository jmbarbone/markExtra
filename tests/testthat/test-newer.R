test_that("%newer% works", {
  # make temp files and get times
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  file.create(f1)
  file.create(f2)
  Sys.sleep(2)     # wait a couple seconds so the times are different
  file.create(f3)

  # get times for comparisons as well2
  t1 <- file.mtime(f1)
  t2 <- file.mtime(f2)
  t3 <- file.mtime(f3)

  # nolint start: spaces_inside_linter.
  expect_false(f1 %newer% t1)
  expect_false(f2 %newer% t2)
  expect_false(f3 %newer% t3)
  expect_false(f1 %newer% f2)
  expect_false(f1 %newer% f3)
  expect_false(f3 %newer% f3)
  expect_true( f3 %newer% f2)
  expect_true( f3 %newer% f1)
  expect_true( f1 %newer% tempfile())
  # nolint end: spaces_inside_linter.

  expect_error(tempfile() %newer% f1, "exist")

  file.remove(c(f1, f2, f3))
})
