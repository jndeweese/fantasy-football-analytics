# =============================================================================
# 03_clean.R  --  Transform raw snapshot -> tidy, app-ready data/*.rds
# -----------------------------------------------------------------------------
# Reads data-raw/ff_snapshot_raw.rds (produced by 01_pull_espn.R OR
# 02_load_cache.R) and writes clean, documented tables to data/. The Shiny app
# reads ONLY these files — it never touches the API or the raw snapshot.
#
# Outputs (all combined across seasons 2023/2024/2025 with a `season` column):
#   data/franchises.rds   one row per team-season
#   data/schedule.rds     one row per team-week (with opponent + result)
#   data/standings.rds    one row per team-season (ESPN end/asof standings)
#   data/starters.rds     one row per rostered player-week (lineup slots)
#   data/draft.rds        one row per draft pick
#   data/draft_adp.rds    draft picks joined to FFC 2QB ADP (all seasons)
#   data/playerscores.rds season point totals per player (Draft Value production)
#   data/league_info.rds  one row per season (league settings)
#
# Usage:  Rscript data-raw/03_clean.R   (run from the project root)
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

raw_path <- file.path("data-raw", "ff_snapshot_raw.rds")
if (!file.exists(raw_path)) {
  stop("Missing ", raw_path, ". Run data-raw/02_load_cache.R (or 01_pull_espn.R) first.")
}
raw <- readRDS(raw_path)
seasons <- names(raw)

# Per-season regular-season / playoff structure (used to tag weeks below).
season_structure <- readr::read_csv(file.path("data-raw", "season_structure.csv"),
                                    show_col_types = FALSE)

dir.create("data", showWarnings = FALSE)

# Starting lineup slots (everything else is bench/IR). Superflex/2QB league, so
# OP / QB-flex slots count as starters.
bench_slots <- c("BE", "IR")

#' Normalize a player name for cross-source joins (lowercase, strip suffixes,
#' punctuation, and extra whitespace). Used to join ESPN names to ADP sources.
normalize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\s+(jr|sr|ii|iii|iv|v)\\.?$", "") %>%
    str_remove_all("[.'`,]") %>%
    str_squish()
}

# --- franchises (team-season identity) ---------------------------------------
franchises <- imap_dfr(raw, function(s, season) {
  s$franchises %>%
    transmute(season = as.integer(season),
              franchise_id,
              franchise_name,
              franchise_abbrev)
})

# --- league_info (settings per season) ---------------------------------------
league_info <- imap_dfr(raw, function(s, season) {
  s$league %>%
    transmute(season = as.integer(season),
              league_id, league_name, league_type, qb_type,
              franchise_count, roster_size, keeper_count)
})

# --- schedule (team-week, with opponent names + outcome flags) ---------------
schedule <- imap_dfr(raw, function(s, season) {
  fr <- s$franchises %>% select(franchise_id, franchise_name, franchise_abbrev)
  s$schedule %>%
    mutate(season = as.integer(season)) %>%
    # Drop any names the source already carried; we re-attach them from `fr`
    # so this works for both the cached snapshot and the live pull.
    select(-any_of(c("franchise_name", "franchise_abbrev",
                     "opponent_name", "opponent_abbrev"))) %>%
    left_join(fr, by = "franchise_id") %>%
    left_join(fr %>% rename(opponent_id = franchise_id,
                            opponent_name = franchise_name,
                            opponent_abbrev = franchise_abbrev),
              by = "opponent_id") %>%
    transmute(season, week,
              franchise_id, franchise_name, franchise_abbrev, franchise_score,
              opponent_id, opponent_name, opponent_abbrev, opponent_score,
              result,
              is_played = !is.na(result),
              margin = franchise_score - opponent_score)
})

# Tag regular-season vs playoff weeks (per data-raw/season_structure.csv).
# 2024/2025 merge the championship (NFL wks 16+17) into one "week" -> labeled.
schedule <- schedule %>%
  dplyr::left_join(season_structure, by = "season") %>%
  dplyr::mutate(
    is_regular = week <= regular_end,
    week_type = dplyr::case_when(
      week <= regular_end ~ "Regular",
      week == r1_week     ~ "Playoff R1",
      week == champ_week  ~ "Championship",
      TRUE                ~ "Playoff"
    ),
    week_label = dplyr::if_else(
      champ_combined & week == champ_week,
      paste0(champ_week, "–", champ_week + 1L),
      as.character(week)
    )
  ) %>%
  dplyr::select(-regular_end, -r1_week, -champ_week, -champ_combined)

# --- standings (as-of / end-of-season, straight from ESPN) -------------------
standings <- imap_dfr(raw, function(s, season) {
  s$standings %>% mutate(season = as.integer(season), .before = 1)
})

# --- starters (player-week lineup rows) --------------------------------------
starters <- imap_dfr(raw, function(s, season) {
  s$starters %>%
    mutate(season = as.integer(season),
           is_starter = !lineup_slot %in% bench_slots) %>%
    select(season, week, franchise_id, franchise_name, franchise_score,
           lineup_slot, is_starter, player_id, player_name, pos, team,
           player_score, projected_score)
})

# Tag regular-season player-weeks (for lineup-efficiency / POP / draft-value).
starters <- starters %>%
  dplyr::left_join(dplyr::select(season_structure, season, regular_end), by = "season") %>%
  dplyr::mutate(is_regular = week <= regular_end) %>%
  dplyr::select(-regular_end)

# --- draft (one row per pick, all seasons) -----------------------------------
draft <- imap_dfr(raw, function(s, season) {
  s$draft %>%
    mutate(season = as.integer(season)) %>%
    select(season, round, pick, overall,
           franchise_id, franchise_name,
           player_id, player_name, pos, team)
})

# --- draft_adp (picks joined to Fantasy Football Calculator 2QB ADP) ---------
# ADP from FFC (2QB, 12-team) via data-raw/adp/pull_ffc_adp.R; standardized across
# seasons (replaces the old 2023-only manual consensus file). We glob whatever FFC
# CSVs exist, so seasons FFC lacks (currently 2025) just carry NA adp.
# Currently unused by the app (the Draft Analysis tab was removed); retained for a
# planned ADP fold-in into the Draft Value tab.
ffc <- list.files(file.path("data-raw", "adp"), pattern = "^ffc_2qb_adp\\.csv$",
                  recursive = TRUE, full.names = TRUE) %>%
  set_names(basename(dirname(.))) %>%                 # name = the year folder
  imap_dfr(function(f, yr) {
    read_csv(f, show_col_types = FALSE) %>%
      transmute(season    = as.integer(yr),
                join_name = normalize_name(name),
                pos       = recode(position, PK = "K", DEF = "DST"),
                adp, times_drafted, stdev)
  }) %>%
  distinct(season, join_name, pos, .keep_all = TRUE)

draft_adp <- draft %>%
  mutate(join_name = normalize_name(player_name)) %>%
  left_join(ffc, by = c("season", "join_name", "pos")) %>%
  transmute(
    season, overall_pick = overall, round, player = player_name, player_id, pos,
    nfl_team = team, franchise_id, franchise_name,
    adp, times_drafted, stdev,
    pick_vs_adp = overall - adp)            # + = value (fell past ADP); - = reach (taken before ADP)

# --- playerscores (each player's season fantasy-point total) -----------------
# ESPN's per-player season totals/averages (the whole player pool, not just
# rostered). The Draft Value tab uses score_total as the "production" of a pick.
playerscores <- imap_dfr(raw, function(s, season) {
  if (is.null(s$playerscores)) return(NULL)
  s$playerscores %>%
    mutate(season = as.integer(season)) %>%
    transmute(season, player_id, player_name, pos, score_total, score_average)
})

# --- Write outputs -----------------------------------------------------------
saveRDS(franchises,  "data/franchises.rds")
saveRDS(league_info, "data/league_info.rds")
saveRDS(schedule,    "data/schedule.rds")
saveRDS(standings,   "data/standings.rds")
saveRDS(starters,    "data/starters.rds")
saveRDS(draft,       "data/draft.rds")
saveRDS(draft_adp,   "data/draft_adp.rds")
saveRDS(playerscores,"data/playerscores.rds")

# --- Validation report -------------------------------------------------------
cat("\n=== CLEAN OUTPUTS ===\n")
report <- function(name, df) cat(sprintf("%-14s %5d rows x %2d cols\n", name, nrow(df), ncol(df)))
report("franchises",  franchises)
report("league_info", league_info)
report("schedule",    schedule)
report("standings",   standings)
report("starters",    starters)
report("draft",       draft)
report("draft_adp",   draft_adp)
report("playerscores",playerscores)

cat("\nPlayed weeks per season (schedule):\n")
schedule %>% filter(is_played) %>% count(season, week) %>%
  group_by(season) %>% summarise(weeks = paste0(min(week), "-", max(week)),
                                 team_weeks = sum(n), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nDistinct starter lineup slots:\n")
print(starters %>% filter(is_starter) %>% count(lineup_slot, sort = TRUE) %>% as.data.frame(), row.names = FALSE)

cat("\nFranchise names by season:\n")
franchises %>% arrange(season, franchise_id) %>%
  group_by(season) %>% summarise(teams = paste(franchise_name, collapse = " | "), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nDone.\n")
