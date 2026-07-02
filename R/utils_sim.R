# =============================================================================
# utils_sim.R  --  Player-level Monte Carlo playoff simulation
# -----------------------------------------------------------------------------
# Simulates the remaining regular-season games from a chosen week and estimates
# each team's playoff odds and seed probabilities.
#
# Method (per the approved methodology):
#   1. Build each player's weekly scoring distribution from games started up to
#      the chosen week. Players with few games (< min_games) fall back to a
#      pooled distribution for their position.
#   2. Each team's remaining-week score is the sum of bootstrap draws from its
#      current (chosen-week) starting lineup's player distributions -- a genuine
#      player-level, non-parametric draw that captures skew and roster strength.
#   2b. Estimation uncertainty: a few games barely pin down how good a team is,
#       so each simulated season also draws one strength offset per team (SD from
#       the players' posterior-mean variances; see lineup_team_sd). This keeps the
#       model from being falsely certain early and lets odds firm up over time.
#   3. Replay the remaining actual matchups, adding wins/points to the standings
#      as of the chosen week.
#   4. Rank by league tiebreak (wins, then points-for); top `playoff_spots`
#      make the playoffs. Repeat n_sims times and aggregate.
# =============================================================================

#' One bootstrap draw of a team's weekly total: sum of one sampled score per
#' starter. `pools` is a list of numeric score vectors (one per starter).
draw_team_week <- function(pools, n_sims) {
  draws <- vapply(pools, function(s) sample(s, n_sims, replace = TRUE),
                  numeric(n_sims))
  if (is.null(dim(draws))) draws <- matrix(draws, nrow = n_sims)
  rowSums(draws)
}

#' Build per-team bootstrap pools from the lineups. Each player's pool is their own
#' game log (or the positional pool if they have < `min_games`). If `targets` (a
#' named vector player_id -> shrunk mean) is supplied, each pool is recentered to
#' that mean while keeping its shape/spread -- this is the projection shrinkage.
#' @return Named list keyed by franchise_id; each element a list of score vectors.
build_lineup_pools <- function(starters_hist, lineup, min_games = 4, targets = NULL) {
  pos_pool <- split(starters_hist$player_score, starters_hist$pos)
  by_player <- split(starters_hist$player_score, starters_hist$player_id)

  split(lineup, lineup$franchise_id) |>
    lapply(function(team) {
      Map(function(pid, pos) {
        s <- by_player[[as.character(pid)]]
        if (is.null(s) || length(s) < min_games) s <- pos_pool[[pos]]
        if (is.null(s) || length(s) == 0) s <- 0
        if (!is.null(targets)) {
          tg <- targets[[as.character(pid)]]
          if (!is.null(tg) && !is.na(tg)) s <- s - mean(s) + tg   # recenter to shrunk mean
        }
        s
      }, team$player_id, team$pos)
    })
}

#' Each team's representative go-forward lineup, respecting the league's starting
#' slots (verified: QB x2, RB x2, WR x3, TE x1, FLEX[RB/WR/TE] x1, DST x1, K x1).
#'
#' Start frequency over the last 3 weeks is counted per *player* (slot-agnostic),
#' so a player who moves between, e.g., the RB slot and the FLEX still gets full
#' credit. Each position's slots are filled by the most-frequent starters of that
#' position, then the FLEX takes the most-frequent leftover RB/WR/TE. Ties in
#' start count are broken at random (a random jitter < 1 added to the count).
recent_lineup <- function(st, from_week) {
  base     <- c(QB = 2, RB = 2, WR = 3, TE = 1, DST = 1, K = 1)
  flex_pos <- c("RB", "WR", "TE")

  freq <- st %>%
    dplyr::filter(week > from_week - 3, week <= from_week, is_starter) %>%
    dplyr::group_by(franchise_id, player_id, pos) %>%
    dplyr::summarise(starts = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(key = starts + stats::runif(dplyr::n()))   # +random tiebreak (<1)

  pick_team <- function(d) {
    taken <- character(0); out <- list()
    for (p in names(base)) {                       # fill each position's base slots
      sel <- d %>% dplyr::filter(pos == p, !player_id %in% taken) %>%
        dplyr::slice_max(key, n = base[[p]], with_ties = FALSE)
      taken <- c(taken, sel$player_id); out[[p]] <- sel
    }
    out[["FLEX"]] <- d %>%                          # best leftover RB/WR/TE
      dplyr::filter(pos %in% flex_pos, !player_id %in% taken) %>%
      dplyr::slice_max(key, n = 1, with_ties = FALSE)
    dplyr::bind_rows(out)
  }

  freq %>%
    dplyr::group_split(franchise_id) %>%
    purrr::map_dfr(pick_team) %>%
    dplyr::select(franchise_id, player_id, pos)
}

#' Per-team SD of the *season-mean* score, built from player posteriors.
#'
#' Parameter (estimation) uncertainty: we don't know each player's true scoring
#' level, only estimate it. A player's posterior-mean variance is
#' `sigma^2 / (n + shrink_k)` — the aleatory game-to-game variance over the
#' effective sample size (games played + the projection prior's strength). A
#' team's weekly total is the sum of its starters, so these variances add, and
#' the team's season-mean SD is `unc_scale * sqrt(sum of starter posterior-mean
#' variances)`. It shrinks as games accumulate, so odds firm up on their own.
#' @return Named (by franchise_id) numeric vector of team season-mean SDs.
lineup_team_sd <- function(lineup, pvar_by_player, pos_var, shrink_k, unc_scale) {
  split(lineup, lineup$franchise_id) |>
    vapply(function(team) {
      v <- vapply(seq_len(nrow(team)), function(i) {
        pv <- pvar_by_player[[as.character(team$player_id[i])]]
        if (is.null(pv) || is.na(pv)) {            # no own history: positional prior only
          pp <- pos_var[[team$pos[i]]]
          pv <- if (is.null(pp) || is.na(pp)) 0 else pp / shrink_k
        }
        pv
      }, numeric(1))
      unc_scale * sqrt(sum(v))
    }, numeric(1))
}

#' Simulate playoff odds from `from_week` to `reg_end`.
#'
#' Player-level Monte Carlo with a posterior-predictive treatment of team
#' strength. Each team's go-forward lineup (`recent_lineup()`) is bootstrapped
#' from player game logs, with each player's mean **shrunk toward their average
#' weekly projection** through `from_week` (weight `n/(n+shrink_k)`; `shrink_k`
#' is the prior strength in "games"; when a projection is missing the prior falls
#' back to the positional average). On top of that point estimate we add the
#' **uncertainty about the mean itself** (`lineup_team_sd()`): each simulated
#' season draws one strength offset per team, so the model isn't falsely certain
#' how good a team is after only a few games. `unc_scale` scales this estimation
#' uncertainty: 0 reproduces the old "means are known" behaviour, and 1 is the
#' nominal within-player posterior. The default (3) deliberately inflates it,
#' because that nominal posterior captures only within-player sampling error and
#' ignores the real, unmodelled uncertainty in a team's go-forward strength over
#' a 14-game season — roster churn (waivers/trades), injuries, players' shifting
#' roles, and imperfect lineup-setting. The inflation matters mainly early (it
#' shrinks automatically as games accumulate) and preserves team ordering
#' (Spearman ~0.95 vs the un-inflated odds) — humbler magnitudes, same ranking.
#' This is an analytic modelling choice, not exposed in the dashboard UI.
#' @return tibble: franchise_id/name, playoff_pct, seed1..seed`playoff_spots` pct,
#'   plus mean projected final wins/points-for.
simulate_playoffs <- function(ff, season, from_week, reg_end = 14,
                              n_sims = 10000, playoff_spots = 4,
                              min_games = 4, seed = 1409, shrink_k = 5,
                              unc_scale = 3) {
  set.seed(seed)

  fr <- season_franchises(ff, season)
  team_ids <- sort(fr$franchise_id)
  n_teams <- length(team_ids)
  id_name <- stats::setNames(fr$franchise_name, fr$franchise_id)

  st <- ff$starters %>% dplyr::filter(season == !!season)
  sched <- ff$schedule %>% dplyr::filter(season == !!season)

  # Standings as of from_week ------------------------------------------------
  cur <- sched %>%
    dplyr::filter(week <= from_week, is_played) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(wins = sum(result == "W"), pf = sum(franchise_score), .groups = "drop")
  cur <- tibble::tibble(franchise_id = team_ids) %>%
    dplyr::left_join(cur, by = "franchise_id") %>%
    dplyr::mutate(wins = dplyr::coalesce(wins, 0), pf = dplyr::coalesce(pf, 0))
  current_wins <- cur$wins
  current_pf   <- cur$pf

  # Player history up to from_week (source for both the mean and its variance).
  starters_hist <- st %>%
    dplyr::filter(week <= from_week, is_starter, !is.na(player_score)) %>%
    dplyr::select(player_id, pos, player_score)

  # Per-player games / mean / variance; positional pools as the low-sample fallback.
  own <- starters_hist %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(n = dplyr::n(), xbar = mean(player_score),
                     v = stats::var(player_score),
                     pos = dplyr::first(pos), .groups = "drop")
  pos_var  <- tapply(starters_hist$player_score, starters_hist$pos, stats::var)
  pos_mean <- tapply(starters_hist$player_score, starters_hist$pos, mean)
  proj <- st %>%
    dplyr::filter(week <= from_week, is_starter, projected_score > 0) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(proj = mean(projected_score), .groups = "drop")

  shrunk <- own %>%
    dplyr::left_join(proj, by = "player_id") %>%
    dplyr::mutate(
      # Prior each player's mean shrinks toward: their own projection, or — when it's
      # missing (e.g. 2023 wk1, where ESPN returns no projections for ~90% of
      # starters) — the positional average. Falling back to the raw single-game mean
      # (the old behaviour) silently switched OFF the early-season shrinkage for that
      # week, making the odds overconfident; the positional prior keeps the humility.
      prior  = dplyr::coalesce(proj, as.numeric(pos_mean[pos]), xbar),
      w = n / (n + shrink_k),
      target = w * xbar + (1 - w) * prior,
      # aleatory variance: own once we have enough games, else the positional pool
      sigma2 = ifelse(n >= min_games & !is.na(v), v, pos_var[pos]),
      sigma2 = ifelse(is.na(sigma2), 0, sigma2),
      pvar   = sigma2 / (n + shrink_k))          # variance of the player's mean
  targets        <- stats::setNames(shrunk$target, as.character(shrunk$player_id))
  pvar_by_player <- stats::setNames(shrunk$pvar,   as.character(shrunk$player_id))

  lu    <- recent_lineup(st, from_week)
  pools <- build_lineup_pools(starters_hist, lu, min_games = min_games, targets = targets)

  # One season-strength offset per team per simulated season (parameter uncertainty).
  team_sd <- lineup_team_sd(lu, pvar_by_player, pos_var, shrink_k, unc_scale)
  team_off <- if (any(team_sd > 0)) {
    stats::setNames(lapply(team_ids, function(tid)
      stats::rnorm(n_sims, 0, team_sd[[as.character(tid)]])), as.character(team_ids))
  } else NULL

  sampler <- function(tid) {
    base <- draw_team_week(pools[[as.character(tid)]], n_sims)
    if (is.null(team_off)) base else base + team_off[[as.character(tid)]]
  }

  # Remaining matchups (one row per matchup) ---------------------------------
  rem <- sched %>%
    dplyr::filter(week > from_week, week <= reg_end, franchise_id < opponent_id) %>%
    dplyr::select(week, a = franchise_id, b = opponent_id)

  wins_add <- matrix(0, nrow = n_sims, ncol = n_teams)
  pf_add   <- matrix(0, nrow = n_sims, ncol = n_teams)
  colpos <- stats::setNames(seq_len(n_teams), team_ids)

  for (w in sort(unique(rem$week))) {
    week_scores <- vapply(team_ids, sampler, numeric(n_sims))
    pf_add <- pf_add + week_scores
    wk <- rem %>% dplyr::filter(week == w)
    for (i in seq_len(nrow(wk))) {
      ai <- colpos[[as.character(wk$a[i])]]
      bi <- colpos[[as.character(wk$b[i])]]
      a_win <- week_scores[, ai] > week_scores[, bi]
      wins_add[, ai] <- wins_add[, ai] + a_win
      wins_add[, bi] <- wins_add[, bi] + !a_win
    }
  }

  final_wins <- sweep(wins_add, 2, current_wins, "+")
  final_pf   <- sweep(pf_add, 2, current_pf, "+")

  # Seed each simulated season (wins, then PF) -------------------------------
  combined <- final_wins * 1e6 + final_pf
  seeds <- t(apply(combined, 1, function(x) rank(-x, ties.method = "first")))

  res <- tibble::tibble(
    franchise_id = team_ids,
    franchise_name = id_name[as.character(team_ids)],
    playoff_pct = colMeans(seeds <= playoff_spots),
    proj_wins = colMeans(final_wins),
    proj_pf = colMeans(final_pf)
  )
  seed_cols <- vapply(seq_len(playoff_spots),
                      function(k) colMeans(seeds == k), numeric(n_teams))
  colnames(seed_cols) <- paste0("seed", seq_len(playoff_spots))
  dplyr::bind_cols(res, tibble::as_tibble(seed_cols)) %>%
    dplyr::arrange(dplyr::desc(playoff_pct), dplyr::desc(proj_wins))
}
