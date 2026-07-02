# =============================================================================
# 01_pull_espn.R  --  Live data source (refresh path)
# -----------------------------------------------------------------------------
# Pulls the league's data straight from the ESPN Fantasy API via ffscrapr and
# writes data-raw/ff_snapshot_raw.rds in the shape 03_clean.R expects.
#
# Credentials are read from the environment (.Renviron): ESPN_S2, ESPN_SWID,
# FF_LEAGUE_ID. They are NEVER hard-coded here.
#
# ESPN compatibility (see data-raw/espn_compat.R): ffscrapr 1.4.8 targets ESPN's
# old API host and can't read the new single-field team names. We patch the host
# and restore franchise names from the mTeam endpoint. Verified working 2026-06.
#
# Usage:  Rscript data-raw/01_pull_espn.R   (run from the project root)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ffscrapr)
  library(httr2)
})

source("data-raw/espn_compat.R")
patch_ffscrapr_host()

espn_s2   <- Sys.getenv("ESPN_S2")
espn_swid <- Sys.getenv("ESPN_SWID")
league_id <- Sys.getenv("FF_LEAGUE_ID")

if (espn_s2 == "" || espn_swid == "" || league_id == "") {
  stop("Missing ESPN credentials. Set ESPN_S2, ESPN_SWID, FF_LEAGUE_ID in .Renviron ",
       "(see .Renviron.example) and restart R.")
}

seasons <- c(2025, 2024, 2023)

#' Pull every table we use for a single season. Each ff_* call is wrapped so one
#' failure (e.g. transactions unavailable for older seasons) doesn't abort the
#' whole pull. Franchise names are restored from the mTeam endpoint and applied
#' to every table carrying a franchise_id (ffscrapr returns them as NA).
pull_season <- function(season) {
  message("Connecting to ESPN for season ", season, " ...")
  conn <- espn_connect(season = as.integer(season),
                       league_id = as.integer(league_id),
                       swid = espn_swid, espn_s2 = espn_s2)

  safe <- function(expr) tryCatch(expr, error = function(e) {
    warning("  -> failed: ", conditionMessage(e)); NULL
  })

  tables <- list(
    league         = safe(ff_league(conn)),
    franchises     = safe(ff_franchises(conn)),
    schedule       = safe(ff_schedule(conn)),
    standings      = safe(ff_standings(conn)),
    starters       = safe(ff_starters(conn)),
    draft          = safe(ff_draft(conn)),
    playerscores   = safe(ff_playerscores(conn)),
    rosters        = safe(ff_rosters(conn)),
    # BUGFIX vs. the old reference code, which passed 2025 for every season:
    # request scoring history for THIS season.
    scoringhistory = safe(ff_scoringhistory(conn, season = as.integer(season))),
    transactions   = safe(ff_transactions(conn))
  )

  # Fix projected_score: ffscrapr reads the projection by list position, but ESPN
  # flips the actual/projected order week to week, so it's wrong for ~half the
  # weeks. Re-pull projections selected by statSourceId and overwrite.
  if (is.data.frame(tables$starters)) {
    wks <- sort(unique(tables$starters$week))
    proj <- safe(espn_projections(season, league_id, espn_s2, espn_swid, wks))
    if (!is.null(proj)) {
      tables$starters <- tables$starters %>%
        dplyr::select(-"projected_score") %>%
        dplyr::left_join(proj, by = c("week", "player_id"))
    }
  }

  # Restore franchise names (ESPN's new API + ffscrapr return them as NA).
  names_tbl <- safe(espn_team_names(season, league_id, espn_s2, espn_swid))
  if (!is.null(names_tbl)) {
    lookup <- stats::setNames(names_tbl$franchise_name, names_tbl$franchise_id)
    tables <- purrr::map(tables, function(df) {
      if (is.data.frame(df) && "franchise_id" %in% names(df)) {
        df$franchise_name <- unname(lookup[as.character(df$franchise_id)])
      }
      df
    })
    # ff_franchises keeps the correct abbrev; make sure it carries real names too.
    if (is.data.frame(tables$franchises)) {
      tables$franchises <- tables$franchises %>%
        dplyr::rows_update(names_tbl, by = "franchise_id", unmatched = "ignore")
    }
  }

  # Scrub owner identifiers (ESPN user GUIDs incl. the auth SWID, and real
  # manager names) before saving — the app never uses them and they must not be
  # committed to the repo.
  if (is.data.frame(tables$franchises)) {
    tables$franchises <- dplyr::select(
      tables$franchises, -dplyr::any_of(c("user_id", "user_nickname", "user_name")))
  }
  tables
}

raw <- set_names(map(seasons, pull_season), as.character(seasons))

out_path <- file.path("data-raw", "ff_snapshot_raw.rds")
saveRDS(raw, out_path)
message("Wrote live pull -> ", out_path)
message("Next: Rscript data-raw/03_clean.R")
