#' @useDynLib usearchlite, .registration = TRUE
NULL

# Internal helper to call C++ functions
cpp_index_new <- function(dim, path) {
  .Call(`_usearchlite_cpp_index_new`, dim, path)
}

cpp_index_add <- function(ptr, id, vector) {
  invisible(.Call(`_usearchlite_cpp_index_add`, ptr, id, vector))
}

cpp_index_search <- function(ptr, query, k) {
  .Call(`_usearchlite_cpp_index_search`, ptr, query, k)
}

cpp_index_size <- function(ptr) {
  .Call(`_usearchlite_cpp_index_size`, ptr)
}

#' Create a new vector index
#'
#' Creates or loads a vector index at the specified path.
#'
#' @param dim Integer. The dimensionality of vectors to be stored.
#' @param path Character. Directory path where the index will be stored.
#'   Will create `index.usearch` and `meta.rds` files in this directory.
#'
#' @return An object of class `usearchlite_index` containing the index state.
#'
#' @examples
#' \dontrun{
#' tmp <- tempfile()
#' dir.create(tmp)
#' idx <- index_new(3, tmp)
#' }
#'
#' @export
index_new <- function(dim, path) {
  dim <- as.integer(dim)
  path <- as.character(path)

  if (length(dim) != 1L || is.na(dim) || dim <= 0L) {
    stop("dim must be a single positive integer")
  }
  if (length(path) != 1L || is.na(path) || nchar(path) == 0L) {
    stop("path must be a non-empty character string")
  }

  # Create directory if needed
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Load or initialize metadata
  meta_path <- file.path(path, "meta.rds")
  if (file.exists(meta_path)) {
    meta <- readRDS(meta_path)
    # Validate meta structure
    if (!is.data.frame(meta) || !"id" %in% names(meta)) {
      stop("Invalid meta.rds file: expected data.frame with 'id' column")
    }
  } else {
    meta <- data.frame(id = integer(0))
  }

  # Create C++ index
  ptr <- cpp_index_new(dim, path)

  # Create index environment
  idx <- new.env(parent = emptyenv())
  idx$ptr <- ptr
  idx$dim <- dim
  idx$path <- path
  idx$meta <- meta
  class(idx) <- "usearchlite_index"

  idx
}

#' Add a vector to the index
#'
#' Adds a vector with the given ID to the index, optionally with metadata.
#'
#' @param index An `usearchlite_index` object.
#' @param id Integer. Unique identifier for the vector.
#' @param vector Numeric vector of length equal to the index dimension.
#' @param meta Optional list of metadata to associate with this vector.
#'
#' @return The index object (invisibly), for chaining.
#'
#' @examples
#' \dontrun{
#' idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
#' }
#'
#' @export
index_add <- function(index, id, vector, meta = NULL) {
  if (!inherits(index, "usearchlite_index")) {
    stop("index must be a usearchlite_index object")
  }

  id <- as.integer(id)
  vector <- as.double(vector)

  if (length(id) != 1L || is.na(id) || id < 0L) {
    stop("id must be a single non-negative integer")
  }
  if (length(vector) != index$dim) {
    stop(sprintf("vector length (%d) must match index dimension (%d)",
                 length(vector), index$dim))
  }

  # Add to C++ index
  cpp_index_add(index$ptr, id, vector)

  # Update metadata in R
  # Build metadata row as a list first, then convert
  meta_vals <- list(id = id)
  if (!is.null(meta)) {
    if (!is.list(meta)) {
      stop("meta must be a list or NULL")
    }
    for (nm in names(meta)) {
      if (nm == "id") next  # Skip if user provides id in meta
      meta_vals[[nm]] <- meta[[nm]]
    }
  }

  # Check if id already exists
  existing_idx <- which(index$meta$id == id)
  if (length(existing_idx) > 0L) {
    # Update existing row
    for (col in names(meta_vals)) {
      if (col == "id") next
      # Add column if it doesn't exist
      if (!col %in% names(index$meta)) {
        index$meta[[col]] <- NA
      }
      index$meta[[col]][existing_idx] <- meta_vals[[col]]
    }
  } else {
    # Create new row with all columns
    # First, ensure meta has all columns from new row
    for (col in names(meta_vals)) {
      if (!col %in% names(index$meta)) {
        if (nrow(index$meta) == 0L) {
          # For empty data.frame, initialize column properly
          index$meta[[col]] <- vector(typeof(meta_vals[[col]]), 0L)
        } else {
          index$meta[[col]] <- NA
        }
      }
    }
    # Now ensure new row has all columns from meta
    for (col in names(index$meta)) {
      if (!col %in% names(meta_vals)) {
        meta_vals[[col]] <- NA
      }
    }
    # Create data.frame from list and append
    meta_row <- as.data.frame(meta_vals, stringsAsFactors = FALSE)
    meta_row <- meta_row[names(index$meta)]  # Reorder to match
    index$meta <- rbind(index$meta, meta_row)
  }

  # Persist metadata
  saveRDS(index$meta, file.path(index$path, "meta.rds"))

  invisible(index)
}

#' Search for nearest neighbors
#'
#' Searches the index for the k nearest neighbors of the query vector(s).
#'
#' @param index An `usearchlite_index` object.
#' @param query Numeric vector of length `dim`, or matrix with `dim` columns
#'   for batch queries.
#' @param k Integer. Number of nearest neighbors to return.
#' @param filter Optional function that takes the metadata data.frame and
#'   returns a logical vector indicating which rows to keep, or a filtered
#'   data.frame.
#' @param prefilter_k Integer. Number of candidates to retrieve from the
#'   C++ layer before applying the filter. Should be >= k.
#'
#' @return A list with components:
#' \describe{
#'   \item{ids}{Integer vector (or matrix for batch) of neighbor IDs}
#'   \item{distances}{Numeric vector (or matrix for batch) of distances}
#'   \item{meta}{Data.frame of metadata for returned IDs}
#' }
#'
#' @examples
#' \dontrun{
#' res <- index_search(idx, c(1, 0, 0), k = 5)
#' res <- index_search(idx, c(1, 0, 0), k = 5,
#'                     filter = function(m) m$category == "a")
#' }
#'
#' @export
index_search <- function(index, query, k = 10L, filter = NULL, prefilter_k = 100L) {
  if (!inherits(index, "usearchlite_index")) {
    stop("index must be a usearchlite_index object")
  }

  k <- as.integer(k)
  prefilter_k <- as.integer(prefilter_k)

  if (length(k) != 1L || is.na(k) || k <= 0L) {
    stop("k must be a single positive integer")
  }
  if (length(prefilter_k) != 1L || is.na(prefilter_k) || prefilter_k <= 0L) {
    stop("prefilter_k must be a single positive integer")
  }

  # Validate query dimensions
  if (is.matrix(query)) {
    if (ncol(query) != index$dim) {
      stop(sprintf("query columns (%d) must match index dimension (%d)",
                   ncol(query), index$dim))
    }
    query <- matrix(as.double(query), nrow = nrow(query), ncol = ncol(query))
  } else {
    query <- as.double(query)
    if (length(query) != index$dim) {
      stop(sprintf("query length (%d) must match index dimension (%d)",
                   length(query), index$dim))
    }
  }

  # Determine how many candidates to fetch
  fetch_k <- if (is.null(filter)) k else prefilter_k

  # Call C++ search
  cpp_result <- cpp_index_search(index$ptr, query, fetch_k)

  # If no filter, just return top k with metadata
  if (is.null(filter)) {
    ids <- cpp_result$ids
    distances <- cpp_result$distances

    # Get metadata for returned IDs
    if (is.matrix(ids)) {
      # Batch case
      all_ids <- as.integer(unique(as.vector(ids)))
      all_ids <- all_ids[!is.na(all_ids)]
    } else {
      all_ids <- as.integer(ids)
      all_ids <- all_ids[!is.na(all_ids)]
    }

    if (length(all_ids) > 0L && nrow(index$meta) > 0L) {
      meta_match <- index$meta[index$meta$id %in% all_ids, , drop = FALSE]
    } else {
      meta_match <- index$meta[integer(0), , drop = FALSE]
    }

    return(list(
      ids = ids,
      distances = distances,
      meta = meta_match
    ))
  }

  # Apply filter
  if (is.matrix(cpp_result$ids)) {
    # Batch query - apply filter to each row
    n_queries <- nrow(cpp_result$ids)
    result_ids <- matrix(NA_integer_, nrow = n_queries, ncol = k)
    result_dist <- matrix(NA_real_, nrow = n_queries, ncol = k)

    for (q in seq_len(n_queries)) {
      row_ids <- cpp_result$ids[q, ]
      row_dist <- cpp_result$distances[q, ]
      valid_mask <- !is.na(row_ids)
      row_ids <- row_ids[valid_mask]
      row_dist <- row_dist[valid_mask]

      if (length(row_ids) > 0L && nrow(index$meta) > 0L) {
        # Get metadata for candidates
        cand_meta <- index$meta[match(row_ids, index$meta$id), , drop = FALSE]
        # Remove NAs from matching
        valid_idx <- !is.na(cand_meta$id)
        cand_meta <- cand_meta[valid_idx, , drop = FALSE]
        row_ids <- row_ids[valid_idx]
        row_dist <- row_dist[valid_idx]

        if (nrow(cand_meta) > 0L) {
          # Apply filter
          filter_result <- filter(cand_meta)
          if (is.logical(filter_result)) {
            keep <- filter_result
          } else if (is.data.frame(filter_result)) {
            keep <- cand_meta$id %in% filter_result$id
          } else {
            stop("filter must return a logical vector or data.frame")
          }

          row_ids <- row_ids[keep]
          row_dist <- row_dist[keep]
        }
      }

      # Take top k
      n_take <- min(k, length(row_ids))
      if (n_take > 0L) {
        result_ids[q, seq_len(n_take)] <- row_ids[seq_len(n_take)]
        result_dist[q, seq_len(n_take)] <- row_dist[seq_len(n_take)]
      }
    }

    all_ids <- as.integer(unique(as.vector(result_ids)))
    all_ids <- all_ids[!is.na(all_ids)]
    if (length(all_ids) > 0L && nrow(index$meta) > 0L) {
      meta_match <- index$meta[index$meta$id %in% all_ids, , drop = FALSE]
    } else {
      meta_match <- index$meta[integer(0), , drop = FALSE]
    }

    return(list(
      ids = result_ids,
      distances = result_dist,
      meta = meta_match
    ))
  } else {
    # Single query
    ids <- cpp_result$ids
    distances <- cpp_result$distances
    valid_mask <- !is.na(ids)
    ids <- ids[valid_mask]
    distances <- distances[valid_mask]

    if (length(ids) > 0L && nrow(index$meta) > 0L) {
      # Get metadata for candidates
      cand_meta <- index$meta[match(ids, index$meta$id), , drop = FALSE]
      # Remove NAs from matching
      valid_idx <- !is.na(cand_meta$id)
      cand_meta <- cand_meta[valid_idx, , drop = FALSE]
      ids <- ids[valid_idx]
      distances <- distances[valid_idx]

      if (nrow(cand_meta) > 0L) {
        # Apply filter
        filter_result <- filter(cand_meta)
        if (is.logical(filter_result)) {
          keep <- filter_result
        } else if (is.data.frame(filter_result)) {
          keep <- cand_meta$id %in% filter_result$id
        } else {
          stop("filter must return a logical vector or data.frame")
        }

        ids <- ids[keep]
        distances <- distances[keep]
      }
    }

    # Take top k and pad with NA if needed
    n_take <- min(k, length(ids))
    result_ids <- rep(NA_integer_, k)
    result_dist <- rep(NA_real_, k)
    if (n_take > 0L) {
      result_ids[seq_len(n_take)] <- ids[seq_len(n_take)]
      result_dist[seq_len(n_take)] <- distances[seq_len(n_take)]
    }

    all_ids <- as.integer(result_ids)
    all_ids <- all_ids[!is.na(all_ids)]
    if (length(all_ids) > 0L && nrow(index$meta) > 0L) {
      meta_match <- index$meta[index$meta$id %in% all_ids, , drop = FALSE]
    } else {
      meta_match <- index$meta[integer(0), , drop = FALSE]
    }

    return(list(
      ids = result_ids,
      distances = result_dist,
      meta = meta_match
    ))
  }
}

#' Get index metadata
#'
#' Returns the metadata data.frame for all vectors in the index.
#'
#' @param index An `usearchlite_index` object.
#'
#' @return A data.frame with at least an 'id' column.
#'
#' @export
index_meta <- function(index) {
  if (!inherits(index, "usearchlite_index")) {
    stop("index must be a usearchlite_index object")
  }
  index$meta
}

#' Print method for usearchlite_index
#'
#' @param x An `usearchlite_index` object.
#' @param ... Ignored.
#'
#' @return The index (invisibly).
#'
#' @export
print.usearchlite_index <- function(x, ...) {
  size <- tryCatch(cpp_index_size(x$ptr), error = function(e) NA_integer_)
  cat("<usearchlite_index>\n")
  cat("  Dimension:", x$dim, "\n")
  cat("  Path:", x$path, "\n")
  cat("  Size:", if (is.na(size)) "unknown" else size, "vectors\n")
  cat("  Metadata rows:", nrow(x$meta), "\n")
  invisible(x)
}
