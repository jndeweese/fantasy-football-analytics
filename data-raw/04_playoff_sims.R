# =============================================================================
# data-raw/04_playoff_sims.R  --  Precompute playoff-odds simulations.
# -----------------------------------------------------------------------------
# Runs the player-level Monte Carlo (R/utils_sim.R) for the end of every week of
# every season and writes data/playoff_sims.rds. The Playoff Projections tab READS
# this -- it no longer simulates live. simulate_playoffs() is deterministic
# (set.seed), so the saved odds are stable.
#
# Re-run after refreshing the cleaned data (i.e. after data-raw/03_clean.R):
#   Rscript data-raw/04_playoff_sims.R     (from the project root)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble)
})
source("R/utils_data.R")
source("R/utils_sim.R")

ff <- load_ff_data("data")

N_SIMS <- 10000L  # offline, so no load-time cap; 10k -> playoff-% SE ~0.5%

sims <- purrr::map_dfr(seasons_available(ff), function(s) {
  re <- reg_season_end(ff, s)
  fr <- season_franchises(ff, s) %>% dplyr::select(franchise_id, franchise_abbrev)
  purrr::map_dfr(seq_len(re - 1), function(w) {
    simulate_playoffs(ff, s, from_week = w, reg_end = re, n_sims = N_SIMS) %>%
      dplyr::mutate(from_week = w)
  }) %>%
    dplyr::left_join(fr, by = "franchise_id") %>%
    dplyr::mutate(season = s)
})

saveRDS(sims, "data/playoff_sims.rds")
cat(sprintf("Wrote data/playoff_sims.rds: %d rows | seasons %s | %d sims/wk\n",
            nrow(sims), paste(sort(unique(sims$season)), collapse = ", "), N_SIMS))
