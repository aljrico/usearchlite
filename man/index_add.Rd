\name{index_add}
\alias{index_add}
\title{Add a Vector to the Index}
\description{
Adds a vector with the given ID to the index, optionally with metadata.
}
\usage{
index_add(index, id, vector, meta = NULL)
}
\arguments{
  \item{index}{An \code{usearchlite_index} object.}
  \item{id}{Integer. Unique identifier for the vector.}
  \item{vector}{Numeric vector of length equal to the index dimension.}
  \item{meta}{Optional list of metadata to associate with this vector.}
}
\value{
The index object (invisibly), for chaining.
}
\examples{
\dontrun{
idx <- index_add(idx, 1L, c(1, 0, 0), meta = list(category = "a"))
}
}
