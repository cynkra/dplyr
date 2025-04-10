test_that("fun_list is merged with new args", {
  withr::local_options(lifecycle_verbosity = "quiet")

  funs <- funs(fn = bar)
  funs <- as_fun_list(funs, env(), baz = "baz")
  expect_identical(funs$fn, quo(bar(., baz = "baz")))
})

test_that("funs() works with namespaced calls", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(summarise_all(mtcars, funs(base::mean(.))), summarise_all(mtcars, funs(mean(.))))
  expect_identical(summarise_all(mtcars, funs(base::mean)), summarise_all(mtcars, funs(mean(.))))
})

test_that("funs() found in local environment", {
  withr::local_options(lifecycle_verbosity = "quiet")

  f <- function(x) 1
  df <- data.frame(x = c(2:10, 1000))

  out <- summarise_all(df, funs(f = f, mean = mean, median = median))
  expect_equal(out, data.frame(f = 1, mean = 105.4, median = 6.5))
})

test_that("funs() accepts quoted functions", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(funs(mean), funs("mean"))
})

test_that("funs() accepts unquoted functions", {
  withr::local_options(lifecycle_verbosity = "quiet")

  funs <- funs(fn = !!mean)
  expect_identical(funs$fn, new_quosure(call2(base::mean, quote(.))))
})

test_that("funs() accepts quoted calls", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(funs(mean), funs(mean(.)))
})

test_that("funs() can be merged with new arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")

  fns <- funs(foo(.))
  expect_identical(as_fun_list(fns, current_env(), foo = 1L), funs(foo(., foo = 1L)))
})

enfun <- function(.funs, ...) {
  as_fun_list(.funs, caller_env(), ...)
}

test_that("can enfun() literal functions", {
  res <- enfun(identity(mean))
  expect_equal(length(res), 1L)
  expect_identical(res[[1L]], mean)
})

test_that("can enfun() named functions by expression", {
  res <- enfun(mean)
  expect_equal(length(res), 1L)
  expect_identical(res[[1L]], mean)
})

test_that("local objects are not treated as symbols", {
  withr::local_options(lifecycle_verbosity = "quiet")

  mean <- funs(my_mean(.))
  expect_identical(enfun(mean), mean)
})

test_that("can enfun() character vectors", {
  res <- enfun(c("min", "max"))
  expect_equal(length(res), 2L)
  expect_equal(res[[1]], min)
  expect_equal(res[[2]], max)
})

test_that("can enfun() purrr-style lambdas", {
  my_mean <- as_function(~ mean(.x))
  res <- enfun(~ mean(.x))
  expect_equal(length(res), 1L)
  expect_true(typeof(res[[1]]) == "closure")
})

test_that("funs_ works", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    funs(mean),
    funs_(list(~ mean))
  )

  expect_equal(
    funs_(list("mean")),
    funs_(list(`environment<-`(~ mean, baseenv()))),
    ignore_formula_env = TRUE
  )

  expect_equal(
    funs(mean(.)),
    funs_(list(~ mean(.)))
  )
})

test_that("as_fun_list() auto names chr vectors (4307)", {
  expect_identical(
    data.frame(x = 1:10) %>% summarise_at("x", c("mean", "sum")),
    data.frame(x = 1:10) %>% summarise(mean = mean(x), sum = sum(x))
  )
})

test_that("funs() is deprecated", {
  expect_snapshot(funs(fn = bar))
})

# Errors ------------------------------------------------------------------

test_that("funs() give meaningful error messages", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    (expect_error(funs(function(si) { mp[si] })))
    (expect_error(funs(~ mp[.]))  )
  })

})
