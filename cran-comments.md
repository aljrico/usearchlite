## Test environments
- local macOS (aarch64-apple-darwin24.2.0), R 4.5.0

## R CMD check results
0 errors | 0 warnings | 1 note

* New submission

## Notes for CRAN
This package vendors a portion of the USearch library (Apache-2.0 licensed).
To comply with CRAN checks for "pragmas in C/C++ headers and code", the vendored
headers were modified to:
- replace `#pragma once` with standard include guards, and
- remove compiler-specific diagnostic-suppression pragmas (e.g., `#pragma GCC diagnostic ignored`).

These changes are non-functional (portability/compliance only).
