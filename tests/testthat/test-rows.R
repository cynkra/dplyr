test_that("rows_insert()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_insert(data, tibble(a = 4L, b = "z"), by = "a"),
    tibble(a = 1:4, b = c("a", "b", NA, "z"), c = c(0.5, 1.5, 2.5, NA))
  )

  expect_error(
    rows_insert(data, tibble(a = 3, b = "z"), by = "a"),
    "insert duplicate"
  )

  expect_error(
    rows_insert(data, tibble()),
    "at least 1 column"
  )
})

test_that("rows_update()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_update(data, tibble(a = 2:3, b = "z"), by = "a"),
    tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
  )

  expect_error(
    rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b")),
    "update missing"
  )

  expect_silent(
    expect_identical(
      rows_update(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
    )
  )

  expect_error(
    rows_update(data, tibble()),
    "at least 1 column"
  )
})

test_that("rows_patch()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_patch(data, tibble(a = 2:3, b = "z"), by = "a"),
    tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
  )

  expect_error(
    rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b")),
    "patch missing"
  )

  expect_silent(
    expect_identical(
      rows_patch(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
    )
  )

  expect_error(
    rows_patch(data, tibble()),
    "at least 1 column"
  )
})

test_that("rows_upsert()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_upsert(data, tibble(a = 2:4, b = "z"), by = "a"),
    tibble(a = 1:4, b = c("a", "z", "z", "z"), c = c(data$c, NA))
  )

  expect_error(
    rows_upsert(data, tibble()),
    "at least 1 column"
  )
})

test_that("rows_delete()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_delete(data, tibble(a = 2:3), by = "a"),
    data[1, ]
  )

  expect_error(
    rows_delete(data, tibble(a = 2:4), by = "a"),
    "delete missing"
  )

  expect_snapshot(res <- rows_delete(data, tibble(a = 2:3, b = "b"), by = "a"))
  expect_identical(res, data[1, ])

  expect_error(
    rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")),
    "delete missing"
  )

  expect_error(
    rows_delete(data, tibble()),
    "at least 1 column"
  )
})

test_that("rows_*() errors", {
  expect_snapshot({
    data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

    # Insert
    (expect_error(
                    rows_insert(data, tibble(a = 3, b = "z"))
    ))
    (expect_error(
                    rows_insert(data[c(1, 1), ], tibble(a = 3))
    ))
    (expect_error(
                    rows_insert(data, tibble(a = 4, b = "z"), by = "e")
    ))

    (expect_error(
                    rows_insert(data, tibble(d = 4))
    ))

    # Update
    (expect_error(
                    rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
    ))

    # Variants: patch
    (expect_error(
                    rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
    ))

    # Delete and truncate
    (expect_error(
                    rows_delete(data, tibble(a = 2:4))
    ))
    (expect_error(
                    rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b"))
    ))
    rows_delete(data, tibble(a = 2:3))
    rows_delete(data, tibble(a = 2:3, b = "b"))

  })


})
