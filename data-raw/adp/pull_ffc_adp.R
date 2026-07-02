# =============================================================================
# data-raw/adp/pull_ffc_adp.R  --  Pull 2QB ADP from Fantasy Football Calculator
# -----------------------------------------------------------------------------
# Writes data-raw/adp/<year>/ffc_2qb_adp.csv for each season. FFC's ADP REST API
# is free and exposes genuine *preseason* 2QB ADP by year, which ESPN's API does
# not (ESPN only serves current ADP, and in standard/PPR not 2QB).
#
#   https://fantasyfootballcalculator.com/api/v1/adp/2qb?teams=12&year=<year>
#
# 12-team is FFC's well-populated 2QB dataset; relative ADP (for reach/value) is
# what matters, not the exact team count. Runs BEFORE 03_clean.R (which joins
# these CSVs to the draft picks). Re-run when refreshing the data.
#
# Usage:  Rscript data-raw/adp/pull_ffc_adp.R   (from the project root)
# =============================================================================

suppressPackageStartupMessages({
  library(httr2); library(readr); library(dplyr)
})

seasons <- c(2023L, 2024L, 2025L)
base_url <- "https://fantasyfootballcalculator.com/api/v1/adp/2qb"

pull_year <- function(yr) {
  message("Fetching FFC 2QB ADP for ", yr, " ...")
  resp <- request(base_url) |>
    req_url_query(teams = 12, year = yr) |>
    req_user_agent("FF_Dashboard portfolio project (personal use)") |>
    req_retry(max_tries = 3) |>
    req_perform()

  # FFC serves JSON but mislabels the Content-Type as text/html, so skip the check.
  dat <- resp_body_json(resp, check_type = FALSE, simplifyVector = TRUE)
  players <- dat$players
  if (is.null(players) || !nrow(players)) {
    warning("  -> no players returned for ", yr); return(invisible(NULL))
  }
  meta <- dat$meta
  message(sprintf("  -> %d players (%s, %d-team, from %s drafts)",
                  nrow(players), meta$format, meta$teams,
                  if (!is.null(meta$total_drafts)) meta$total_drafts else "?"))

  out <- players %>%
    transmute(player_id, name, position, team,
              adp, times_drafted, high, low, stdev, bye)

  dir <- file.path("data-raw", "adp", as.character(yr))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "ffc_2qb_adp.csv")
  write_csv(out, path)
  message("  -> wrote ", path)
  Sys.sleep(1)  # FFC updates once/day; be polite between calls
}

invisible(lapply(seasons, pull_year))
message("\nDone. Next: Rscript data-raw/03_clean.R")
