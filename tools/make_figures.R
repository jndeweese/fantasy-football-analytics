# =============================================================================
# tools/make_figures.R  --  Regenerate README figures from the cleaned data.
# Renders each tab's key plot to img/ for the README. Reproducible:
#   Rscript tools/make_figures.R   (run from the project root)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(forcats); library(tibble); library(ggplot2); library(ggtext)
  library(ggrepel); library(scales); library(TTR)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Default to the PUBLIC (anonymized) data so committed README figures show
# "Team A–H", never real names. Override with FF_DATA_DIR=data for a private set.
ff <- load_ff_data(Sys.getenv("FF_DATA_DIR", "data-public"))
dir.create("img", showWarnings = FALSE)
S <- 2025L
save <- function(name, plot, w = 9, h = 5) {
  ggsave(file.path("img", name), plot, width = w, height = h, dpi = 110, bg = "white")
  cat("wrote img/", name, "\n", sep = "")
}

# Overview
save("overview_weekly.png", plot_weekly_scores_ranks(ff, S), w = 11, h = 6)

# Standings & luck
save("standings_luck_trend.png", plot_luck_trend(luck_trajectory(ff, S)), w = 9, h = 5)
save("schedule_swap.png", plot_schedule_swap(schedule_swap(ff, S)), w = 8, h = 6)

# Power rankings
pr <- power_rankings(ff, S, half_life = 3, score_weight = 0.5)
save("power_trend.png", plot_trend(pr, "power"), w = 9, h = 5)

# Player performance (faceted by position -> show all positions)
pp <- player_pop_range(ff, S, min_games = PLAYER_MIN_GAMES)
save("player_scatter.png", plot_player_scatter(pp), w = 9, h = 10)

# Draft value
o <- draft_outcomes(ff, S)
save("draft_value_scatter.png",
     plot_draft_value_scatter(o, draft_value_curve(ff), ymax = max(o$actual_points)), w = 9, h = 8)

# Playoff projections — odds trajectory over the season
save("playoff_trajectory.png",
     plot_playoff_trajectory(dplyr::filter(ff$playoff_sims, season == S)), w = 9, h = 5)

cat("\nAll figures written to img/\n")
