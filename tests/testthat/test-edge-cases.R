library(usearchlite)

# Helper for Windows-safe cleanup - removes objects only if they exist
safe_on_exit <- function(tmp, env = parent.frame()) {
  for (obj in c("idx", "idx2", "res", "m")) {
    if (exists(obj, envir = env, inherits = FALSE)) {
      rm(list = obj, envir = env)
    }
  }
  gc()
  unlink(tmp, recursive = TRUE, force = TRUE)
}

test_that("k larger than number of vectors pads with NA", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))
  idx <- index_add(idx, 2L, c(0, 1, 0))

  # Request k=5 but only 2 vectors exist
  res <- index_search(idx, c(1, 0, 0), k = 5L)

  expect_equal(length(res$ids), 5L)
  expect_equal(length(res$distances), 5L)

  # First 2 should be non-NA, last 3 should be NA
  expect_equal(sum(!is.na(res$ids)), 2L)
  expect_equal(sum(is.na(res$ids)), 3L)
  expect_true(all(!is.na(res$ids[1:2])))
  expect_true(all(is.na(res$ids[3:5])))
})

test_that("non-finite values rejected on add", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)

  # NA in vector
  expect_error(
    index_add(idx, 1L, c(1, NA, 0)),
    "finite|non-finite|NA|missing",
    ignore.case = TRUE
  )

  # Inf in vector
  expect_error(
    index_add(idx, 2L, c(1, Inf, 0)),
    "finite|non-finite|Inf|infinite",
    ignore.case = TRUE
  )

  # -Inf in vector
  expect_error(
    index_add(idx, 3L, c(-Inf, 0, 0)),
    "finite|non-finite|Inf|infinite",
    ignore.case = TRUE
  )

  # NaN in vector
  expect_error(
    index_add(idx, 4L, c(NaN, 0, 0)),
    "finite|non-finite|NaN|NA",
    ignore.case = TRUE
  )
})

test_that("non-finite values rejected on search", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))

  # NA in query
  expect_error(
    index_search(idx, c(1, NA, 0), k = 1L),
    "finite|non-finite|NA|missing",
    ignore.case = TRUE
  )

  # Inf in query
  expect_error(
    index_search(idx, c(1, Inf, 0), k = 1L),
    "finite|non-finite|Inf|infinite",
    ignore.case = TRUE
  )

  # NaN in query
  expect_error(
    index_search(idx, c(NaN, 0, 0), k = 1L),
    "finite|non-finite|NaN|NA",
    ignore.case = TRUE
  )
})

test_that("prefilter_k smaller than k caps filtered results", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(cat = "a"))
  idx <- index_add(idx, 2L, c(0.9, 0.1, 0), meta = list(cat = "a"))
  idx <- index_add(idx, 3L, c(0.8, 0.2, 0), meta = list(cat = "a"))
  idx <- index_add(idx, 4L, c(0.7, 0.3, 0), meta = list(cat = "a"))
  idx <- index_add(idx, 5L, c(0.6, 0.4, 0), meta = list(cat = "a"))

  # With a filter and prefilter_k=2, only 2 candidates are fetched from C++
  # so at most 2 results can pass through
  res <- index_search(idx, c(1, 0, 0), k = 5L,
                      filter = function(m) m$cat == "a",  # all pass
                      prefilter_k = 2L)

  # Should have at most 2 non-NA results (prefilter caps it)
  expect_lte(sum(!is.na(res$ids)), 2L)
})

test_that("filter returning wrong type errors clearly", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(cat = "a"))
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(cat = "b"))

  # Filter returning character instead of logical/data.frame
  expect_error(
    index_search(idx, c(1, 0, 0), k = 1L,
                 filter = function(m) "wrong type",
                 prefilter_k = 10L),
    "filter|logical|data.frame",
    ignore.case = TRUE
  )

  # Filter returning numeric
  expect_error(
    index_search(idx, c(1, 0, 0), k = 1L,
                 filter = function(m) 1:nrow(m),
                 prefilter_k = 10L),
    "filter|logical|data.frame",
    ignore.case = TRUE
  )
})

test_that("metadata schema evolution works", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)

  # First add with columns A, B
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(colA = "a1", colB = 10))

  # Second add introduces column C (colA/B absent)
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(colC = "new"))

  # Third add has all three
  idx <- index_add(idx, 3L, c(0, 0, 1), meta = list(colA = "a3", colB = 30, colC = "c3"))

  m <- index_meta(idx)

  expect_equal(nrow(m), 3L)
  expect_true(all(c("id", "colA", "colB", "colC") %in% names(m)))

  # Check values filled correctly (NA where missing)
  expect_equal(m$colA[1], "a1")
  expect_equal(m$colB[1], 10)
  expect_true(is.na(m$colC[1]))

  expect_true(is.na(m$colA[2]))
  expect_true(is.na(m$colB[2]))
  expect_equal(m$colC[2], "new")

  expect_equal(m$colA[3], "a3")
  expect_equal(m$colB[3], 30)
  expect_equal(m$colC[3], "c3")
})

test_that("duplicate id is rejected", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(name = "original"))

  # Adding same id should error (USearch doesn't allow duplicates by default)
  expect_error(
    index_add(idx, 1L, c(0, 1, 0), meta = list(name = "duplicate")),
    "Duplicate|duplicate|already exists|key",
    ignore.case = TRUE
  )

  # Original should still be there
  m <- index_meta(idx)
  expect_equal(nrow(m), 1L)
  expect_equal(m$name[1], "original")
})
