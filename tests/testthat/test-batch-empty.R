library(usearchlite)

# Helper for Windows-safe cleanup - removes objects only if they exist
safe_on_exit <- function(tmp, env = parent.frame()) {
  for (obj in c("idx", "idx2", "res", "queries")) {
    if (exists(obj, envir = env, inherits = FALSE)) {
      rm(list = obj, envir = env)
    }
  }
  gc()
  unlink(tmp, recursive = TRUE, force = TRUE)
}

test_that("batch search on empty index returns NA matrices", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)

  # Batch query with 2 queries, k=3
  queries <- rbind(c(1, 0, 0), c(0, 1, 0))
  res <- index_search(idx, queries, k = 3L)

  # Should return matrices
  expect_true(is.matrix(res$ids))
  expect_true(is.matrix(res$distances))

  # Correct dimensions: 2 queries x 3 results each
  expect_equal(dim(res$ids), c(2L, 3L))
  expect_equal(dim(res$distances), c(2L, 3L))

  # All should be NA
  expect_true(all(is.na(res$ids)))
  expect_true(all(is.na(res$distances)))
})

test_that("batch search with partial results pads correctly", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))
  idx <- index_add(idx, 2L, c(0, 1, 0))

  # 3 queries, k=5, but only 2 vectors in index
  queries <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  res <- index_search(idx, queries, k = 5L)

  expect_true(is.matrix(res$ids))
  expect_equal(dim(res$ids), c(3L, 5L))

  # Each row should have at most 2 non-NA values
  for (i in 1:3) {
    expect_lte(sum(!is.na(res$ids[i, ])), 2L)
  }
})

test_that("single query returns vector not matrix", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(safe_on_exit(tmp), add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))

  # Single query as vector
  res <- index_search(idx, c(1, 0, 0), k = 3L)

  # Should return vectors, not matrices
  expect_false(is.matrix(res$ids))
  expect_false(is.matrix(res$distances))
  expect_equal(length(res$ids), 3L)
  expect_equal(length(res$distances), 3L)
})
