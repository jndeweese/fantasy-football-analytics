# =============================================================================
# 05_make_public_data.R  --  Build the PUBLIC (anonymized) copy of the data
# -----------------------------------------------------------------------------
# Reads the canonical data/*.rds and writes an anonymized mirror to
# data-public/*.rds, replacing every real team name/abbrev with a stable alias
# ("Team A" .. "Team H", plus "Team I" for the one 2023-only owner).
#
# The alias is keyed on (season, franchise_id) via data-raw/franchise_alias.csv,
# so person identity is encoded in that crosswalk: the SAME person keeps the SAME
# alias across seasons (edit the CSV so their (season, franchise_id) rows share a
# label), and a seat that changed owner gets a DISTINCT alias for the odd season.
#
# franchise_id itself is NEVER changed, so all joins, colors (the palette is keyed
# on franchise_id), and metrics are identical between the two datasets -- only the
# display strings differ.
#
# Run AFTER the data is fully refreshed (01 -> adp -> 03 -> 04), so playoff_sims
# is included:
#   Rscript data-raw/05_make_public_data.R   (from the project root)
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

in_dir  <- "data"
out_dir <- "data-public"
alias_path <- file.path("data-raw", "franchise_alias.csv")

if (!dir.exists(in_dir)) stop("Missing ", in_dir, "/ -- run data-raw/03_clean.R first.")
if (!file.exists(alias_path)) stop("Missing ", alias_path, ".")

alias <- readr::read_csv(alias_path, show_col_types = FALSE) %>%
  mutate(season = as.integer(season), franchise_id = as.integer(franchise_id))

stopifnot(all(c("season", "franchise_id", "alias", "alias_abbrev") %in% names(alias)))

# A seat may appear at most once per season. A duplicate (season, franchise_id)
# row would multiply every joined table's rows in apply_alias() below (silent data
# corruption), and it can slip past the alias-uniqueness check when the duplicate
# rows carry *different* aliases -- so guard the join key directly.
id_dupe <- alias %>% count(season, franchise_id) %>% filter(n > 1)
if (nrow(id_dupe) > 0) {
  stop("Duplicate (season, franchise_id) in ", alias_path, ":\n",
       paste(sprintf("  season %d, franchise_id %d", id_dupe$season, id_dupe$franchise_id),
             collapse = "\n"))
}

# --- Validate the crosswalk against the real franchises -----------------------
fr <- readRDS(file.path(in_dir, "franchises.rds"))

missing <- dplyr::anti_join(fr, alias, by = c("season", "franchise_id"))
if (nrow(missing) > 0) {
  stop("franchise_alias.csv is missing rows for:\n",
       paste(sprintf("  season %d, franchise_id %d", missing$season, missing$franchise_id),
             collapse = "\n"))
}
# Aliases must be unique *within* a season (two teams can't share a label in the
# same year); they SHOULD repeat across seasons (same person, same alias).
dupe <- alias %>% count(season, alias) %>% filter(n > 1)
if (nrow(dupe) > 0) {
  stop("Duplicate alias within a season (each label must be unique per season):\n",
       paste(sprintf("  season %d: %s", dupe$season, dupe$alias), collapse = "\n"))
}

# --- The anonymizer: swap display strings, keep the ids -----------------------
#' Replace franchise_name/abbrev (and opponent_name/abbrev) with aliases, keyed on
#' the id columns. Any table lacking those columns is returned unchanged.
apply_alias <- function(df) {
  if (!all(c("season", "franchise_id") %in% names(df))) return(df)

  if (any(c("franchise_name", "franchise_abbrev") %in% names(df))) {
    df <- dplyr::left_join(df, alias, by = c("season", "franchise_id"))
    if ("franchise_name"   %in% names(df)) df$franchise_name   <- df$alias
    if ("franchise_abbrev" %in% names(df)) df$franchise_abbrev <- df$alias_abbrev
    df <- dplyr::select(df, -alias, -alias_abbrev)
  }

  if ("opponent_id" %in% names(df) &&
      any(c("opponent_name", "opponent_abbrev") %in% names(df))) {
    opp <- dplyr::rename(alias, opponent_id = franchise_id,
                         .opp_name = alias, .opp_abbrev = alias_abbrev)
    df <- dplyr::left_join(df, opp, by = c("season", "opponent_id"))
    if ("opponent_name"   %in% names(df)) df$opponent_name   <- df$.opp_name
    if ("opponent_abbrev" %in% names(df)) df$opponent_abbrev <- df$.opp_abbrev
    df <- dplyr::select(df, -.opp_name, -.opp_abbrev)
  }
  df
}

# Collect the real names up front so we can prove none leak into the output.
real_names <- unique(c(fr$franchise_name, fr$franchise_abbrev))

# --- Transform every table ----------------------------------------------------
dir.create(out_dir, showWarnings = FALSE)
files <- list.files(in_dir, pattern = "[.]rds$", full.names = FALSE)

leaks <- character(0)
for (f in files) {
  d <- readRDS(file.path(in_dir, f))

  if (is.data.frame(d)) {
    d <- apply_alias(d)

    # league_info carries the real league name/id -> neutralize (not team data,
    # not displayed by the app, but it's in the bundle, so don't ship it).
    if (identical(f, "league_info.rds")) {
      if ("league_name" %in% names(d)) d$league_name <- "Fantasy Football League"
      if ("league_id"   %in% names(d)) d$league_id   <- NA_character_
    }

    # Leak check: no real franchise string may survive in the *franchise/opponent*
    # display columns. (We scope to these columns on purpose -- the NFL `team`
    # column legitimately holds abbrevs like "CIN" that collide with a fantasy
    # abbrev, and player names are real NFL players, neither of which is a leak.)
    name_cols <- intersect(c("franchise_name", "franchise_abbrev",
                             "opponent_name", "opponent_abbrev"), names(d))
    chr_vals <- unlist(lapply(d[name_cols], unique), use.names = FALSE)
    hit <- intersect(chr_vals, real_names)
    if (length(hit) > 0) leaks <- c(leaks, sprintf("%s: %s", f, paste(hit, collapse = ", ")))
  }

  saveRDS(d, file.path(out_dir, f))
}

if (length(leaks) > 0) {
  stop("Real team names leaked into the public build:\n  ", paste(leaks, collapse = "\n  "))
}

# --- Report -------------------------------------------------------------------
cat("\n=== PUBLIC DATA BUILD (", out_dir, ") ===\n", sep = "")
pub_fr <- readRDS(file.path(out_dir, "franchises.rds"))
pub_fr %>%
  transmute(season, franchise_id, real = fr$franchise_name[match(
    paste(season, franchise_id), paste(fr$season, fr$franchise_id))],
    alias = franchise_name, abbrev = franchise_abbrev) %>%
  arrange(season, franchise_id) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nWrote", length(files), "anonymized tables to", out_dir, "/\n")
cat("Leak check passed: no real team name or abbrev appears in the public data.\n")
