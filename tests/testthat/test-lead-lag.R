test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_s3_class(lead(x), "Date")
  expect_s3_class(lag(x), "Date")

  expect_s3_class(lead(y), "POSIXct")
  expect_s3_class(lag(y), "POSIXct")
})

test_that("#925 is fixed", {
  data <- tibble(
    name = c("Rob", "Pete", "Rob", "John", "Rob", "Pete", "John", "Pete", "John", "Pete", "Rob", "Rob"),
    time = c(3, 2, 5, 3, 2, 3, 2, 4, 1, 1, 4, 1)
  )
  res <- data %>% group_by(name) %>% mutate(lag_time = lag(time))
  expect_equal(
    res$lag_time[res$name == "Rob"],
    c(NA, head(data$time[data$name == "Rob"], -1))
  )
  expect_equal(
    res$lag_time[res$name == "Pete"],
    c(NA, head(data$time[data$name == "Pete"], -1))
  )
  expect_equal(
    res$lag_time[res$name == "John"],
    c(NA, head(data$time[data$name == "John"], -1))
  )
})

test_that("#937 is fixed", {
  df <- tibble(
    name = rep(c("Al", "Jen"), 3),
    score = rep(c(100, 80, 60), 2)
  )

  res <- df %>% group_by(name) %>% mutate(next.score = lead(score))
  expect_equal(
    res$next.score[res$name == "Al"],
    c(tail(df$score[df$name == "Al"], -1), NA)
  )
  expect_equal(
    res$next.score[res$name == "Jen"],
    c(tail(df$score[df$name == "Jen"], -1), NA)
  )
})

test_that("lead() and lag() work for matrices (#5028)", {
  m <- matrix(1:6, ncol = 2)
  expect_equal(lag(m, 1), matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol = 2))
  expect_equal(lag(m, 1, default = NA), matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol= 2))

  expect_equal(lead(m, 1), matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2))
  expect_equal(lead(m, 1, default = NA), matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2))
})

test_that("lead() and lag() checks size of default (#5641)", {
  expect_error(lead(1:10, default = integer()))
  expect_error(lag(1:10, default = integer()))
})

# Errors ------------------------------------------------------------------

test_that("lead() / lag() give meaningful errors", {
  expect_snapshot({
    "# complicance of n argument"
    (expect_error(lead(letters, -1)))
    (expect_error(lead(letters, "1")))
    (expect_error(lag(letters, -1)))
    (expect_error(lag(letters, "1")))

    "# ts"
    (expect_error(lag(ts(1:10))))

    "# incompatible default"
    (expect_error(lag(c("1", "2", "3"), default = FALSE)))
    (expect_error(lead(c("1", "2", "3"), default = FALSE)))

    (expect_error(lag(c("1", "2", "3"), default = character())))
    (expect_error(lead(c("1", "2", "3"), default = character())))
  })
})
