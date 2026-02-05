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
\dontrun{
m <- index_meta(idx)
}
}
