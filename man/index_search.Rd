\name{index_search}
\alias{index_search}
\title{Search for Nearest Neighbors}
\description{
Searches the index for the k nearest neighbors of the query vector(s).
}
\usage{
index_search(index, query, k = 10L, filter = NULL, prefilter_k = 100L)
}
\arguments{
  \item{index}{An \code{usearchlite_index} object.}
  \item{query}{Numeric vector of length \code{dim}, or matrix with \code{dim} columns
    for batch queries.}
  \item{k}{Integer. Number of nearest neighbors to return.}
  \item{filter}{Optional function that takes the metadata data.frame and
    returns a logical vector indicating which rows to keep, or a filtered
    data.frame.}
  \item{prefilter_k}{Integer. Number of candidates to retrieve from the
    C++ layer before applying the filter. Should be >= k.}
}
\value{
A list with components:
\describe{
  \item{ids}{Integer vector (or matrix for batch) of neighbor IDs}
  \item{distances}{Numeric vector (or matrix for batch) of distances}
  \item{meta}{Data.frame of metadata for returned IDs}
}
}
\examples{
tmp <- tempfile()
dir.create(tmp)
idx <- index_new(3L, tmp)
idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
idx <- index_add(idx, 2L, c(0, 1, 0), meta = list(category = "b"))
res <- index_search(idx, c(1, 0, 0), k = 2L)
res <- index_search(idx, c(1, 0, 0), k = 2L,
                    filter = function(m) m$category == "a")
unlink(tmp, recursive = TRUE)
}
