library(usearchlite)

# Helper for Windows-safe cleanup
safe_cleanup <- function(tmp, ...) {

  objs <- c(...)
  for (obj in objs) {
    if (exists(obj, envir = parent.frame())) {
      rm(list = obj, envir = parent.frame())
    }
  }
  gc()
  unlink(tmp, recursive = TRUE, force = TRUE)
}

test_that("index_new creates an index", {
 tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)

  expect_s3_class(idx, "usearchlite_index")
  expect_equal(idx$dim, 3L)
  expect_equal(normalizePath(idx$path), normalizePath(tmp))
  expect_true(is.data.frame(idx$meta))
  expect_true("id" %in% names(idx$meta))
})

test_that("index_add adds vectors and metadata", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(category = "b"))
  idx <- index_add(idx, 3L, c(0, 0, 1), meta = list(category = "a"))

  expect_equal(nrow(idx$meta), 3L)
  expect_equal(idx$meta$id, c(1L, 2L, 3L))
  expect_equal(idx$meta$category, c("a", "b", "a"))
})

test_that("index_search finds nearest neighbors", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(category = "b"))
  idx <- index_add(idx, 3L, c(0, 0, 1), meta = list(category = "a"))

  # Search for vector similar to first
  res <- index_search(idx, c(1, 0, 0), k = 1L)

  expect_equal(res$ids[1], 1L)
  expect_true(!is.na(res$distances[1]))
  expect_true(is.data.frame(res$meta))
})

test_that("index_search handles empty index", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  res <- index_search(idx, c(1, 0, 0), k = 5L)

  expect_equal(length(res$ids), 5L)
  expect_true(all(is.na(res$ids)))
  expect_true(all(is.na(res$distances)))
})

test_that("index_search with filter", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
  idx <- index_add(idx, 2L, c(0.9, 0.1, 0), meta = list(category = "b"))
  idx <- index_add(idx, 3L, c(0.8, 0.2, 0), meta = list(category = "a"))

  # Filter for category "b" only
  res <- index_search(idx, c(1, 0, 0), k = 1L,
                      filter = function(m) m$category == "b",
                      prefilter_k = 10L)

  expect_equal(res$ids[1], 2L)
})

test_that("index persists to disk", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  # Create and populate index
 idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(category = "b"))

  # Release index before checking files (ensures finalizer runs)
  rm(idx)
  gc()

  # Verify files exist
  expect_true(file.exists(file.path(tmp, "index.usearch")))
  expect_true(file.exists(file.path(tmp, "meta.rds")))

  # Create new index object from same path
  idx2 <- index_new(3L, tmp)

  # Should have same metadata
  expect_equal(nrow(idx2$meta), 2L)
  expect_equal(idx2$meta$id, c(1L, 2L))
  expect_equal(idx2$meta$category, c("a", "b"))

  # Search should work
  res <- index_search(idx2, c(1, 0, 0), k = 1L)
  expect_equal(res$ids[1], 1L)

  rm(idx2)
})

test_that("dimension mismatch is detected on add", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)

  expect_error(index_add(idx, 1L, c(1, 0)),
               "vector length .* must match index dimension")
  expect_error(index_add(idx, 1L, c(1, 0, 0, 0)),
               "vector length .* must match index dimension")
})

test_that("dimension mismatch is detected on search", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))

  expect_error(index_search(idx, c(1, 0), k = 1L),
               "query length .* must match index dimension")
})

test_that("dimension mismatch is detected on reload", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  # Create with dim 3
  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))
  rm(idx)
  gc()

  # Try to reload with different dimension
  expect_error(index_new(5L, tmp), "Dimension mismatch")
})

test_that("index_meta returns metadata", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(name = "first"))
  idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(name = "second"))

  m <- index_meta(idx)

  expect_true(is.data.frame(m))
  expect_equal(nrow(m), 2L)
  expect_equal(m$id, c(1L, 2L))
  expect_equal(m$name, c("first", "second"))
})

test_that("batch search works", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))
  idx <- index_add(idx, 2L, c(0, 1, 0))
  idx <- index_add(idx, 3L, c(0, 0, 1))

  # Batch query with matrix
  queries <- matrix(c(1, 0, 0,
                      0, 1, 0), nrow = 2, byrow = TRUE)
  res <- index_search(idx, queries, k = 1L)

  expect_true(is.matrix(res$ids))
  expect_equal(dim(res$ids), c(2L, 1L))
  expect_equal(res$ids[1, 1], 1L)
  expect_equal(res$ids[2, 1], 2L)
})

test_that("print method works", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)
  idx <- index_add(idx, 1L, c(1, 0, 0))

  expect_output(print(idx), "usearchlite_index")
  expect_output(print(idx), "Dimension: 3")
})

test_that("negative id is rejected", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ rm(idx); gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  idx <- index_new(3L, tmp)

  expect_error(
    index_add(idx, -1L, c(1, 0, 0)),
    "id|negative|positive|>=\\s*0",
    ignore.case = TRUE
  )
})

test_that("invalid dim is rejected", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit({ gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

  expect_error(index_new(0L, tmp), "dim must be a single positive integer")
  expect_error(index_new(-1L, tmp), "dim must be a single positive integer")
})
