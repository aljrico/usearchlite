#include <cpp11.hpp>
#include <string>
#include <vector>
#include <cstdint>

// USearch C API header
extern "C" {
#include "usearch.h"
}

// Wrapper struct to hold index state
struct IndexWrapper {
    usearch_index_t index;
    size_t dim;
    std::string path;
    bool dirty;

    IndexWrapper() : index(nullptr), dim(0), dirty(false) {}

    ~IndexWrapper() {
        if (index) {
            // Save if dirty before freeing
            if (dirty && !path.empty()) {
                usearch_error_t error = nullptr;
                usearch_save(index, path.c_str(), &error);
                // Ignore errors in destructor
            }
            usearch_error_t error = nullptr;
            usearch_free(index, &error);
            index = nullptr;
        }
    }
};

// Helper to get wrapper from external pointer with validation
inline IndexWrapper* get_wrapper(SEXP ptr) {
    if (ptr == R_NilValue) {
        cpp11::stop("Index pointer is NULL");
    }
    if (TYPEOF(ptr) != EXTPTRSXP) {
        cpp11::stop("Expected external pointer");
    }
    IndexWrapper* wrapper = static_cast<IndexWrapper*>(R_ExternalPtrAddr(ptr));
    if (!wrapper) {
        cpp11::stop("Index has been freed or is invalid");
    }
    if (!wrapper->index) {
        cpp11::stop("USearch index is not initialized");
    }
    return wrapper;
}

[[cpp11::register]]
SEXP cpp_index_new(int dim, std::string path) {
    if (dim <= 0) {
        cpp11::stop("Dimension must be positive");
    }

    // Create wrapper
    IndexWrapper* wrapper = new IndexWrapper();
    wrapper->dim = static_cast<size_t>(dim);
    wrapper->path = path + "/index.usearch";
    wrapper->dirty = false;

    usearch_error_t error = nullptr;

    // Check if index file exists
    FILE* f = fopen(wrapper->path.c_str(), "r");
    bool exists = (f != nullptr);
    if (f) fclose(f);

    if (exists) {
        // Load existing index
        // First load metadata to get options
        usearch_init_options_t opts = {};
        usearch_metadata(wrapper->path.c_str(), &opts, &error);
        if (error) {
            std::string err_msg = error;
            delete wrapper;
            cpp11::stop("Failed to load index metadata: %s", err_msg.c_str());
        }

        // Verify dimension matches
        if (opts.dimensions != static_cast<size_t>(dim)) {
            delete wrapper;
            cpp11::stop("Dimension mismatch: requested %d but index has %zu", dim, opts.dimensions);
        }

        // Initialize index with loaded options
        wrapper->index = usearch_init(&opts, &error);
        if (error || !wrapper->index) {
            std::string err_msg = error ? error : "Unknown error";
            delete wrapper;
            cpp11::stop("Failed to initialize index from metadata: %s", err_msg.c_str());
        }

        // Load the actual index data
        usearch_load(wrapper->index, wrapper->path.c_str(), &error);
        if (error) {
            std::string err_msg = error;
            usearch_error_t free_err = nullptr;
            usearch_free(wrapper->index, &free_err);
            delete wrapper;
            cpp11::stop("Failed to load index: %s", err_msg.c_str());
        }
    } else {
        // Create new index with cosine metric
        usearch_init_options_t opts = {};
        opts.dimensions = static_cast<size_t>(dim);
        opts.metric_kind = usearch_metric_cos_k;  // Cosine similarity (good for embeddings)
        opts.quantization = usearch_scalar_f32_k; // Use float32 internally
        opts.connectivity = 16;                   // Default M parameter for HNSW
        opts.expansion_add = 128;                 // Default ef_construction
        opts.expansion_search = 64;               // Default ef
        opts.multi = false;
        opts.metric = nullptr;

        wrapper->index = usearch_init(&opts, &error);
        if (error || !wrapper->index) {
            std::string err_msg = error ? error : "Unknown error";
            delete wrapper;
            cpp11::stop("Failed to create index: %s", err_msg.c_str());
        }

        // Reserve initial capacity - important for thread-local structures
        usearch_reserve(wrapper->index, 1024, &error);
        if (error) {
            std::string err_msg = error;
            usearch_error_t free_err = nullptr;
            usearch_free(wrapper->index, &free_err);
            delete wrapper;
            cpp11::stop("Failed to reserve index capacity: %s", err_msg.c_str());
        }
    }

    // Create external pointer with custom deleter
    SEXP ptr = PROTECT(R_MakeExternalPtr(wrapper, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, [](SEXP x) {
        IndexWrapper* w = static_cast<IndexWrapper*>(R_ExternalPtrAddr(x));
        if (w) {
            delete w;
            R_ClearExternalPtr(x);
        }
    }, TRUE);
    UNPROTECT(1);

    return ptr;
}

[[cpp11::register]]
void cpp_index_add(SEXP ptr, int id, cpp11::doubles vector) {
    IndexWrapper* wrapper = get_wrapper(ptr);

    if (static_cast<size_t>(vector.size()) != wrapper->dim) {
        cpp11::stop("Vector length (%d) does not match index dimension (%zu)",
                    static_cast<int>(vector.size()), wrapper->dim);
    }

    if (id < 0) {
        cpp11::stop("ID must be non-negative");
    }

    // Convert R doubles to float32
    std::vector<float> vec(wrapper->dim);
    for (size_t i = 0; i < wrapper->dim; ++i) {
        vec[i] = static_cast<float>(vector[static_cast<R_xlen_t>(i)]);
    }

    usearch_error_t error = nullptr;

    // Ensure there's capacity for this vector
    size_t current_size = usearch_size(wrapper->index, &error);
    size_t current_cap = usearch_capacity(wrapper->index, &error);
    if (current_size >= current_cap) {
        // Reserve more space
        size_t new_cap = current_cap == 0 ? 1024 : current_cap * 2;
        usearch_reserve(wrapper->index, new_cap, &error);
        if (error) {
            cpp11::stop("Failed to reserve capacity: %s", error);
        }
    }

    usearch_add(wrapper->index, static_cast<usearch_key_t>(id), vec.data(),
                usearch_scalar_f32_k, &error);

    if (error) {
        cpp11::stop("Failed to add vector: %s", error);
    }

    wrapper->dirty = true;

    // Save after each add for MVP safety
    usearch_save(wrapper->index, wrapper->path.c_str(), &error);
    if (error) {
        cpp11::stop("Failed to save index: %s", error);
    }
    wrapper->dirty = false;
}

[[cpp11::register]]
cpp11::writable::list cpp_index_search(SEXP ptr, SEXP query, int k) {
    IndexWrapper* wrapper = get_wrapper(ptr);

    if (k <= 0) {
        cpp11::stop("k must be positive");
    }

    usearch_error_t error = nullptr;
    size_t index_size = usearch_size(wrapper->index, &error);

    // Handle matrix or vector input
    bool is_matrix = Rf_isMatrix(query);
    int n_queries = 1;
    int query_dim = 0;

    if (is_matrix) {
        n_queries = Rf_nrows(query);
        query_dim = Rf_ncols(query);
    } else {
        query_dim = Rf_length(query);
    }

    if (static_cast<size_t>(query_dim) != wrapper->dim) {
        cpp11::stop("Query dimension (%d) does not match index dimension (%zu)",
                    query_dim, wrapper->dim);
    }

    // Get pointer to query data
    double* query_data = REAL(query);

    // Allocate result buffers
    std::vector<usearch_key_t> keys(static_cast<size_t>(k));
    std::vector<usearch_distance_t> distances(static_cast<size_t>(k));

    // Prepare output
    cpp11::writable::list result;

    if (is_matrix) {
        // Batch query - return matrices
        cpp11::writable::integers_matrix<> ids_mat(n_queries, k);
        cpp11::writable::doubles_matrix<> dist_mat(n_queries, k);

        // Initialize with NA
        for (int i = 0; i < n_queries; ++i) {
            for (int j = 0; j < k; ++j) {
                ids_mat(i, j) = NA_INTEGER;
                dist_mat(i, j) = NA_REAL;
            }
        }

        // Process each query
        std::vector<float> query_vec(wrapper->dim);
        for (int q = 0; q < n_queries; ++q) {
            // Convert query to float32 (matrix is column-major in R)
            for (size_t i = 0; i < wrapper->dim; ++i) {
                query_vec[i] = static_cast<float>(query_data[q + i * n_queries]);
            }

            // Search
            size_t found = 0;
            if (index_size > 0) {
                found = usearch_search(wrapper->index, query_vec.data(),
                                       usearch_scalar_f32_k,
                                       static_cast<size_t>(k),
                                       keys.data(), distances.data(), &error);
                if (error) {
                    cpp11::stop("Search failed: %s", error);
                }
            }

            // Fill results
            for (size_t i = 0; i < found && i < static_cast<size_t>(k); ++i) {
                ids_mat(q, static_cast<int>(i)) = static_cast<int>(keys[i]);
                dist_mat(q, static_cast<int>(i)) = static_cast<double>(distances[i]);
            }
        }

        result.push_back(cpp11::named_arg("ids") = ids_mat);
        result.push_back(cpp11::named_arg("distances") = dist_mat);
    } else {
        // Single query - return vectors
        cpp11::writable::integers ids_vec(k);
        cpp11::writable::doubles dist_vec(k);

        // Initialize with NA
        for (int i = 0; i < k; ++i) {
            ids_vec[i] = NA_INTEGER;
            dist_vec[i] = NA_REAL;
        }

        // Convert query to float32
        std::vector<float> query_vec(wrapper->dim);
        for (size_t i = 0; i < wrapper->dim; ++i) {
            query_vec[i] = static_cast<float>(query_data[i]);
        }

        // Search
        size_t found = 0;
        if (index_size > 0) {
            found = usearch_search(wrapper->index, query_vec.data(),
                                   usearch_scalar_f32_k,
                                   static_cast<size_t>(k),
                                   keys.data(), distances.data(), &error);
            if (error) {
                cpp11::stop("Search failed: %s", error);
            }
        }

        // Fill results
        for (size_t i = 0; i < found && i < static_cast<size_t>(k); ++i) {
            ids_vec[static_cast<int>(i)] = static_cast<int>(keys[i]);
            dist_vec[static_cast<int>(i)] = static_cast<double>(distances[i]);
        }

        result.push_back(cpp11::named_arg("ids") = ids_vec);
        result.push_back(cpp11::named_arg("distances") = dist_vec);
    }

    return result;
}

[[cpp11::register]]
int cpp_index_size(SEXP ptr) {
    IndexWrapper* wrapper = get_wrapper(ptr);
    usearch_error_t error = nullptr;
    size_t size = usearch_size(wrapper->index, &error);
    return static_cast<int>(size);
}
