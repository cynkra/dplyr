test_that("set operations use coercion rules (#799)", {
  df1 <- tibble(x = 1:2, y = c(1, 1))
  df2 <- tibble(x = 1:2, y = 1:2)

  expect_equal(nrow(union(df1, df2)), 3L)
  expect_equal(nrow(intersect(df1, df2)), 1L)
  expect_equal(nrow(setdiff(df1, df2)), 1L)

  df1 <- tibble(x = factor(letters[1:10]))
  df2 <- tibble(x = letters[6:15])
  res <- intersect(df1, df2)
  expect_equal(res, tibble(x = letters[6:10]), ignore_attr = TRUE)

  res <- intersect(df2, df1)
  expect_equal(res, tibble(x = letters[6:10]), ignore_attr = TRUE)

  res <- union(df1, df2)
  expect_equal(res, tibble(x = letters[1:15]), ignore_attr = TRUE)
  res <- union(df2, df1)
  expect_equal(res, tibble(x = letters[c(6:15, 1:5)]), ignore_attr = TRUE)

  res <- setdiff(df1, df2)
  expect_equal(res, tibble(x = letters[1:5]), ignore_attr = TRUE)
  res <- setdiff(df2, df1)
  expect_equal(res, tibble(x = letters[11:15]), ignore_attr = TRUE)
})

test_that("setdiff handles factors with NA (#1526)", {
  df1 <- tibble(x = factor(c(NA, "a")))
  df2 <- tibble(x = factor("a"))

  res <- setdiff(df1, df2)
  expect_s3_class(res$x, "factor")
  expect_equal(levels(res$x), "a")
  expect_true(is.na(res$x[1]))
})

test_that("intersect does not unnecessarily coerce (#1722)", {
  df <- tibble(a = 1L)
  res <- intersect(df, df)
  expect_type(res$a, "integer")
})

test_that("set operations reconstruct grouping metadata (#3587)", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% group_by(g)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equal(setdiff(df1, df2), filter(df1, x < 3))
  expect_equal(intersect(df1, df2), filter(df1, x >= 3))
  expect_equal(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)) %>% group_by(g))

  expect_equal(setdiff(df1, df2) %>% group_rows(), list_of(1:2))
  expect_equal(intersect(df1, df2) %>% group_rows(), list_of(1:2))
  expect_equal(union(df1, df2) %>% group_rows(), list_of(1:2, 3:4, 5:6))
})

test_that("set operations keep the ordering of the data (#3839)", {
  rev_df <- function(df) {
    df[rev(seq_len(nrow(df))), ]
  }

  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2))
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equal(setdiff(df1, df2), filter(df1, x < 3), ignore_attr = TRUE)
  expect_equal(setdiff(rev_df(df1), df2), filter(rev_df(df1), x < 3), ignore_attr = TRUE)
  expect_equal(intersect(df1, df2), filter(df1, x >= 3), ignore_attr = TRUE)
  expect_equal(intersect(rev_df(df1), df2), filter(rev_df(df1), x >= 3), ignore_attr = TRUE)
  expect_equal(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)), ignore_attr = TRUE)
  expect_equal(union(rev_df(df1), df2), tibble(x = c(4:1, 5:6), g = rep(c(2:1, 3L), each = 2)), ignore_attr = TRUE)
  expect_equal(union(df1, rev_df(df2)), tibble(x = c(1:4, 6:5), g = rep(1:3, each = 2)), ignore_attr = TRUE)
})

test_that("set operations remove duplicates", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% bind_rows(., .)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equal(setdiff(df1, df2), filter(df1, x < 3) %>% distinct(), ignore_attr = TRUE)
  expect_equal(intersect(df1, df2), filter(df1, x >= 3) %>% distinct(), ignore_attr = TRUE)
  expect_equal(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)), ignore_attr = TRUE)
})

test_that("set equality", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% group_by(g)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_true(setequal(df1, df1))
  expect_true(setequal(df2, df2))
  expect_false(setequal(df1, df2))
  expect_false(setequal(df2, df1))
})

test_that("set operations enforce empty ... (#5891)", {
  a <- tibble(var = 1:3)
  b <- tibble(var = 2:4)
  c <- tibble(var = c(1, 3, 4, 5))

  expect_error(intersect(a, b, c))
  expect_error(setdiff(a, b, c))
  expect_error(setequal(a, b, c))
  expect_error(union(a, b, c))
  expect_error(union_all(a, b, c))
})

# Errors ------------------------------------------------------------------

test_that("set operation give useful error message. #903", {
  expect_snapshot({
    alfa <- tibble(
      land = c("Sverige", "Norway", "Danmark", "Island", "GB"),
      data = rnorm(length(land))
    )
    beta <- tibble(
      land = c("Norge", "Danmark", "Island", "Storbritannien"),
      data2 = rnorm(length(land))
    )
    gamma <- tibble(land = 1:2, data = 1:2)
    (expect_error(
      intersect(alfa, beta)
    ))
    (expect_error(
      intersect(alfa, 1)
    ))
    (expect_error(
      intersect(alfa, gamma)
    ))

    (expect_error(
      union(alfa, beta)
    ))
    (expect_error(
      setdiff(alfa, beta)
    ))
  })

})
