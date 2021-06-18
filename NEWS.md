# SUtools 0.5.2

- Better detection of `gfortran` on Windows via `callr` package
- Allowed for optionally processing `allocate` statements in mortran.

# SUtools 0.5.2

- Added an argument `callEntries` to `gen_registration()` to allow for
  inclusion of `.Call()` entries in registration as needed for Fortran
  calling R

# SUtools 0.5.1

- Windows fixes

# SUtools 0.5

- Bug fixes

# SUtools 0.4

- Bug in `fix_jhf_allocate` where I was not accounting for zero
  `jerr_indices`

- Bug in `process_mortran` where a `NULL` was being returned as the
  third value contradicting vignette example!
  
# SUtools 0.3

- Improved a lot of things
- Automatic fix of label warnings from compiler

