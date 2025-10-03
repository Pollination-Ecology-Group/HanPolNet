# Filename: _build.R
# Purpose:  A reproducible script to document and reload the package for development.

# --- 1. Clean the Environment ---
# Clear all objects from the current session to ensure a fresh start.
# This prevents leftover variables from interfering with package functions.
rm(list = ls())
message("✔ Environment cleared.")

# --- 2. Document the Package ---
# Generate .Rd files and update NAMESPACE from roxygen2 comments.
devtools::document()
message("✔ Documentation updated.")

# --- 3. Load the Package ---
# Reload the package for interactive testing.
devtools::load_all()
message("✔ Package reloaded. Ready to test!")
