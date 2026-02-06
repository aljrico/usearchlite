# usearchlite

<!-- badges: start -->
[![R-CMD-check](https://github.com/aljrico/usearchlite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aljrico/usearchlite/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A lightweight local vector index for R. Store embeddings, search by similarity, filter by metadata. All without leaving your filesystem.

Built on [USearch](https://github.com/unum-cloud/usearch), one of the fastest approximate nearest neighbor libraries available.

## Installation

``` r
# Install from CRAN
install.packages("usearchlite")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("aljrico/usearchlite")
```

## Usage

```r
library(usearchlite)

# Create an index (vectors stored locally in a directory)
idx <- index_new(dim = 384L, path = "my_index")

# Add vectors with metadata (each vector is a numeric vector of length 384)
idx <- index_add(idx, id = 1L, vector = embedding1, meta = list(text = "First document", category = "A"))
idx <- index_add(idx, id = 2L, vector = embedding2, meta = list(text = "Second document", category = "B"))
idx <- index_add(idx, id = 3L, vector = embedding3, meta = list(text = "Third document", category = "A"))

# Search for nearest neighbors
results <- index_search(idx, query = query_embedding, k = 10L)
results$ids
#> [1] 3 1 2

# Filter by metadata (filtering happens in R, on the candidate set)
# prefilter_k controls how many candidates to fetch before filtering --
# set it higher than k to ensure enough results survive the filter
results <- index_search(
  idx,
  query = query_embedding,
  k = 5L,
  filter = function(m) m$category == "A",
  prefilter_k = 50L
)
```

Close R, come back later. Your index is still there:

```r
idx <- index_new(dim = 384L, path = "my_index")
# Picks up where you left off
```

## Why usearchlite?

Sometimes you just need vector search without the infrastructure. No databases to configure, no services to run, no cloud dependencies. Just a directory on disk.

The API is intentionally minimal:

| Function | Purpose |
|----------|---------|
| `index_new()` | Create or load an index |
| `index_add()` | Add a vector with metadata |
| `index_search()` | Find nearest neighbors |
| `index_meta()` | Retrieve all metadata |

Filtering is handled in R rather than C++. This keeps the core simple while giving you full flexibility: use any R expression to filter your results.

## License

MIT. Vendored USearch code is Apache-2.0 licensed (see `inst/COPYRIGHTS`).
