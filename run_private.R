# =============================================================================
# run_private.R  --  Run the dashboard locally with REAL team names.
# -----------------------------------------------------------------------------
# The app defaults to the PUBLIC (anonymized) data, so a plain
# `shiny::runApp()` shows "Team A–H". This helper opts into the real names for
# your own local viewing. Source it (RStudio: click "Source", or run the lines).
#
# Real names come from data/ (git-ignored, local-only); if it's missing, rebuild
# it with data-raw/03_clean.R.
# =============================================================================
Sys.setenv(FF_DATA_DIR = "data")
shiny::runApp(".", launch.browser = TRUE)
