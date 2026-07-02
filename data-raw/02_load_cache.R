# =============================================================================
# 02_load_cache.R  --  Offline data source (fallback / canonical)
# -----------------------------------------------------------------------------
# Loads the cached ESPN snapshot (.RData pulled via ffscrapr on 2025-11-26),
# strips the embedded credentials, and writes a credential-free raw snapshot
# (data-raw/ff_snapshot_raw.rds) in the SAME shape that 01_pull_espn.R produces.
#
# This is the canonical data source for the project: the live ESPN API is
# currently unavailable (expired cookie / ESPN API host change), so the app is
# built from this snapshot. Run 03_clean.R next to produce the tidy data/*.rds.
#
# Usage:  Rscript data-raw/02_load_cache.R   (run from the project root)
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

# --- Paths -------------------------------------------------------------------
snapshot_rdata <- "ff_2023-2025_asof_2025-11-26.RData"
out_path       <- file.path("data-raw", "ff_snapshot_raw.rds")

if (!file.exists(snapshot_rdata)) {
  stop("Cannot find '", snapshot_rdata, "'. Run this script from the project root.")
}

# --- Load snapshot into an isolated environment ------------------------------
snap <- new.env()
load(snapshot_rdata, envir = snap)

# The 2025 objects use the prefix "bqgg_"; prior seasons use "bqgg2024_"/"bqgg2023_".
season_prefix <- c("2025" = "bqgg_", "2024" = "bqgg2024_", "2023" = "bqgg2023_")

# ffscrapr tables we carry forward (NULL-safe: some seasons lack transactions).
tables <- c("league", "franchises", "schedule", "standings",
            "starters", "draft", "playerscores", "rosters",
            "scoringhistory", "transactions")

#' Pull one ffscrapr table for one season out of the snapshot environment.
#' Returns NULL if the object is missing or empty.
get_table <- function(prefix, table) {
  obj_name <- paste0(prefix, table)
  if (!exists(obj_name, envir = snap, inherits = FALSE)) return(NULL)
  obj <- get(obj_name, envir = snap)
  if (is.null(obj) || (is.data.frame(obj) && nrow(obj) == 0)) return(NULL)
  tibble::as_tibble(obj)
}

# --- Assemble nested list: raw[[season]][[table]] ----------------------------
raw <- map(season_prefix, function(prefix) {
  set_names(map(tables, ~ get_table(prefix, .x)), tables)
})

# Scrub owner identifiers (ESPN user GUIDs incl. the auth SWID, real manager
# names) from franchises — unused by the app and must not be committed.
raw <- modify(raw, function(s) {
  if (is.data.frame(s$franchises)) {
    s$franchises <- dplyr::select(
      s$franchises, -dplyr::any_of(c("user_id", "user_nickname", "user_name")))
  }
  s
})

# --- Report what we loaded ---------------------------------------------------
cat("Loaded snapshot from", snapshot_rdata, "\n")
for (s in names(raw)) {
  present <- names(keep(raw[[s]], ~ !is.null(.x)))
  cat(sprintf("  %s: %s\n", s, paste(present, collapse = ", ")))
}

# --- Save credential-free raw snapshot (safe to commit) ----------------------
# NOTE: espn_s2_val / espn_swid_val from the .RData are intentionally NOT carried
# into `raw`, so this .rds contains no secrets.
saveRDS(raw, out_path)
cat("\nWrote credential-free raw snapshot ->", out_path, "\n")
