# =============================================================================
# utils_data.R  --  Load the cleaned snapshot + small data helpers
# -----------------------------------------------------------------------------
# The app reads ONLY data/*.rds (produced by data-raw/03_clean.R). Loaded once
# at startup and shared across sessions.
# =============================================================================

#' Load all cleaned tables into a single named list.
#' @param data_dir Directory holding the cleaned .rds files.
load_ff_data <- function(data_dir = "data") {
  files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No .rds files in '", data_dir, "'. Run data-raw/03_clean.R first.")
  }
  dat <- stats::setNames(lapply(files, readRDS), tools::file_path_sans_ext(basename(files)))
  dat
}

#' Seasons present in the data, descending (most recent first).
seasons_available <- function(ff) {
  sort(unique(ff$schedule$season), decreasing = TRUE)
}

#' Played (completed) weeks for a season, ascending.
weeks_played <- function(ff, season) {
  ff$schedule %>%
    dplyr::filter(season == !!season, is_played) %>%
    dplyr::pull(week) %>%
    unique() %>%
    sort()
}

#' Most recent played week for a season (includes playoff weeks).
current_week <- function(ff, season) {
  max(weeks_played(ff, season), na.rm = TRUE)
}

#' Last regular-season week for a season (from the `is_regular` tag).
#' League-performance tabs default to this rather than `current_week`, so
#' playoff weeks (incl. the merged championship) don't distort the metrics.
reg_season_end <- function(ff, season) {
  max(ff$schedule$week[ff$schedule$season == season & ff$schedule$is_regular],
      na.rm = TRUE)
}

#' Current week *within the regular season*: the latest played week, capped at the
#' final regular-season week. Mid-season this is the latest played week; once the
#' playoffs/championship land in the data it stays pinned at the regular-season end
#' (so it reads as a regular-season progress counter, not a playoff week).
current_reg_week <- function(ff, season) {
  min(current_week(ff, season), reg_season_end(ff, season))
}

#' Franchises for one season (id, name, abbrev), ordered by id.
season_franchises <- function(ff, season) {
  ff$franchises %>%
    dplyr::filter(season == !!season) %>%
    dplyr::arrange(franchise_id)
}
