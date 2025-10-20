# The path comes from your error message
corrupt_path <- "~/R/x86_64-pc-linux-gnu-library/4.5/HanPolNet"
unlink(corrupt_path, recursive = TRUE)

# Double-check it's gone
file.exists(corrupt_path)
#> [1] FALSE

# Regenerate documentation and the NAMESPACE file
devtools::document()

# Fully re-install the package
devtools::install()
