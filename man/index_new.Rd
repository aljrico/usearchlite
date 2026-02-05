\name{index_new}
\alias{index_new}
\title{Create a New Vector Index}
\description{
Creates or loads a vector index at the specified path.
}
\usage{
index_new(dim, path)
}
\arguments{
  \item{dim}{Integer. The dimensionality of vectors to be stored.}
  \item{path}{Character. Directory path where the index will be stored.
    Will create \code{index.usearch} and \code{meta.rds} files in this directory.}
}
\value{
An object of class \code{usearchlite_index} containing the index state.
}
\examples{
\dontrun{
tmp <- tempfile()
dir.create(tmp)
idx <- index_new(3, tmp)
}
}
