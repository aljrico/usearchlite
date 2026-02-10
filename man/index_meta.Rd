\name{index_meta}
\alias{index_meta}
\title{Get Index Metadata}
\description{
Returns the metadata data.frame for all vectors in the index.
}
\usage{
index_meta(index)
}
\arguments{
  \item{index}{An \code{usearchlite_index} object.}
}
\value{
A data.frame with at least an 'id' column.
}
\examples{
tmp <- tempfile()
dir.create(tmp)
idx <- index_new(3L, tmp)
idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(name = "first"))
m <- index_meta(idx)
unlink(tmp, recursive = TRUE)
}
