# =============================================================================
# utils_metrics.R  --  Reusable analytics (scoring trends, all-play, POP)
# -----------------------------------------------------------------------------
# Pure functions, no Shiny. Each tab composes these. Extends the rolling-mean
# and lambda-weighting work prototyped in explore_ffscrapr_2025.Rmd.
# =============================================================================

# ---- Scoring trend smoothers ------------------------------------------------

#' Causal time-decayed weighted mean: at week k, weight observation j (<=k) by
#' lambda^(k-j). lambda near 1 -> long memory; near 0 -> only recent weeks.
#' Defined from week 1 (uses all history to date). This is the smoother
#' prototyped by hand in the reference .Rmd.
ewma_decay <- function(x, lambda = 0.7) {
  vapply(seq_along(x), function(k) {
    w <- lambda^((k:1) - 1)
    sum(x[1:k] * w) / sum(w)
  }, numeric(1))
}

#' Standardize a numeric vector to z-scores (mean 0, SD 1). Returns zeros when the
#' values have no spread, so a degenerate week never yields NaNs.
zscore <- function(x) {
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x)) / s
}

# ---- League-level helpers ---------------------------------------------------

#' All-play record + record vs the weekly median, per team for a season.
#'
#' For each team-week we compare that team's score to every other team's score
#' (all-play: a W for each team you outscored) and to the weekly league median.
#' Season totals give an "all-play" record that strips out schedule luck.
#' Expected wins = all-play win% x games; luck = actual wins - expected wins.
#'
#' @param ff Loaded data list.
#' @param season Season to summarise.
#' @param weeks Optional vector of weeks to include (defaults to all played weeks).
#' @return One row per team with actual / all-play / vs-median records + luck.
all_play_record <- function(ff, season, weeks = NULL) {
  d <- team_weekly_scores(ff, season)
  if (!is.null(weeks)) d <- dplyr::filter(d, week %in% weeks)

  d <- d %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      ap_wins   = purrr::map_dbl(franchise_score, ~ sum(franchise_score < .x)),
      ap_losses = purrr::map_dbl(franchise_score, ~ sum(franchise_score > .x)),
      ap_ties   = purrr::map_dbl(franchise_score, ~ sum(franchise_score == .x) - 1),
      med       = stats::median(franchise_score),
      vs_med    = dplyr::case_when(franchise_score > med ~ "W",
                                   franchise_score < med ~ "L", TRUE ~ "T")
    ) %>%
    dplyr::ungroup()

  d %>%
    dplyr::group_by(franchise_id, franchise_name, franchise_abbrev) %>%
    dplyr::summarise(
      games       = dplyr::n(),
      wins        = sum(result == "W"),
      losses      = sum(result == "L"),
      ties        = sum(result == "T"),
      points_for  = sum(franchise_score),
      ap_wins     = sum(ap_wins),
      ap_losses   = sum(ap_losses),
      ap_ties     = sum(ap_ties),
      med_wins    = sum(vs_med == "W"),
      med_losses  = sum(vs_med == "L"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      win_pct      = wins / games,
      allplay_pct  = ap_wins / (ap_wins + ap_losses + ap_ties),
      expected_wins = allplay_pct * games,
      luck_wins     = wins - expected_wins
    ) %>%
    dplyr::arrange(dplyr::desc(allplay_pct))
}

#' Decompose luck into schedule luck, close-game record, and schedule difficulty.
#'
#' Builds on all_play_record() (schedule luck = actual - expected wins) and adds:
#'   - close-game record (games decided by <= close_margin points)
#'   - points-against luck = expected PA (average opponent) - actual PA;
#'     positive means you faced weaker-than-average opponents (lucky).
#'
#' @param close_margin Points threshold defining a "close" game.
luck_summary <- function(ff, season, weeks = NULL, close_margin = 10) {
  d <- team_weekly_scores(ff, season)
  if (!is.null(weeks)) d <- dplyr::filter(d, week %in% weeks)
  n_teams <- dplyr::n_distinct(d$franchise_id)

  extra <- d %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(week_total = sum(franchise_score),
                  exp_opp = (week_total - franchise_score) / (n_teams - 1),
                  margin = franchise_score - opponent_score) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(
      close_w   = sum(result == "W" & abs(margin) <= close_margin),
      close_l   = sum(result == "L" & abs(margin) <= close_margin),
      pa_actual = sum(opponent_score),
      pa_expected = sum(exp_opp),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pa_luck = pa_expected - pa_actual)

  all_play_record(ff, season, weeks) %>%
    dplyr::left_join(extra, by = "franchise_id") %>%
    dplyr::arrange(dplyr::desc(luck_wins))
}

#' Per-team cumulative luck trajectory over the season: running sum of
#' (actual win − expected win), where the expected win each week is the team's
#' all-play share that week (fraction of the league it outscored). Above 0 means
#' a team has banked more wins than its weekly scores deserved (lucky). The final
#' week's value equals `luck_wins` from `luck_summary()`.
#'
#' @param weeks Optional vector of weeks to include (defaults to all played weeks).
#' @return One row per team-week with `cum_luck` (and the weekly pieces).
luck_trajectory <- function(ff, season, weeks = NULL) {
  d <- team_weekly_scores(ff, season)
  if (!is.null(weeks)) d <- dplyr::filter(d, week %in% weeks)

  d %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      wk_exp = (purrr::map_dbl(franchise_score, ~ sum(franchise_score < .x)) +
                0.5 * purrr::map_dbl(franchise_score, ~ sum(franchise_score == .x) - 1)) /
               (dplyr::n() - 1),
      wk_act = dplyr::case_when(result == "W" ~ 1, result == "T" ~ 0.5, TRUE ~ 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(franchise_id, week) %>%
    dplyr::group_by(franchise_id, franchise_name, franchise_abbrev) %>%
    dplyr::mutate(wk_luck = wk_act - wk_exp, cum_luck = cumsum(wk_luck)) %>%
    dplyr::ungroup()
}

#' Trailing W/L streak from weekly results ordered by week (e.g. "W3", "L2").
streak_str <- function(result) {
  n <- length(result)
  if (n == 0) return("—")
  last <- result[n]
  k <- 0L
  for (r in rev(result)) if (identical(r, last)) k <- k + 1L else break
  paste0(last, k)
}

#' Full-season standings table: actual record + the headline metrics computed
#' elsewhere — points for/against (and differential), current Power index, season
#' lineup efficiency, schedule luck, and current W/L streak. Ordered by the league
#' tiebreak (wins, then points-for) with the top 4 flagged as playoff teams.
#' One row per team.
team_standings <- function(ff, season) {
  ls   <- luck_summary(ff, season)
  eff  <- lineup_efficiency(ff, season) %>% dplyr::select(franchise_id, efficiency)
  pidx <- power_rankings(ff, season) %>%
    dplyr::filter(week == max(week)) %>% dplyr::select(franchise_id, power_index)
  stk  <- team_weekly_scores(ff, season) %>%
    dplyr::arrange(franchise_id, week) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(streak = streak_str(result), .groups = "drop")

  out <- ls %>%
    dplyr::transmute(
      franchise_id, franchise_name, franchise_abbrev,
      wins, losses, ties, win_pct,
      pf = points_for, pa = pa_actual, diff = points_for - pa_actual, luck_wins
    ) %>%
    dplyr::left_join(eff,  by = "franchise_id") %>%
    dplyr::left_join(pidx, by = "franchise_id") %>%
    dplyr::left_join(stk,  by = "franchise_id") %>%
    dplyr::arrange(dplyr::desc(win_pct), dplyr::desc(pf)) %>%
    dplyr::mutate(rank = dplyr::row_number(), playoffs = rank <= 4)

  # Playoff odds: once the regular season is complete the field is settled (top 4
  # = 100%, no simulation needed); mid-season, run the player-level Monte Carlo
  # from the latest played week to estimate each team's odds.
  reg_end <- reg_season_end(ff, season)
  last_played <- max(ff$schedule$week[ff$schedule$season == season &
                                        ff$schedule$is_played & ff$schedule$is_regular])
  if (last_played >= reg_end) {
    out$playoff_pct <- as.numeric(out$playoffs)
  } else {
    odds <- simulate_playoffs(ff, season, from_week = last_played,
                              reg_end = reg_end, n_sims = 5000) %>%
      dplyr::select(franchise_id, playoff_pct)
    out <- dplyr::left_join(out, odds, by = "franchise_id")
  }
  out
}

#' "Schedule swap": each team's record had it played every other team's schedule.
#'
#' For team A and schedule-owner B, replay A's weekly scores against the
#' opponents B actually faced. The diagonal (A plays its own schedule) is A's
#' real record. Edge case: in weeks where B played A, A faces B under the swap.
#'
#' @return Long tibble: team_id/name/abbrev, sched_id/abbrev, wins/losses/ties,
#'   games, is_actual (TRUE on the diagonal).
schedule_swap <- function(ff, season, weeks = NULL) {
  sc <- ff$schedule %>%
    dplyr::filter(season == !!season, is_played, is_regular)
  if (!is.null(weeks)) sc <- dplyr::filter(sc, week %in% weeks)

  fr <- season_franchises(ff, season)
  teams <- sort(unique(sc$franchise_id))
  tk <- as.character(teams)
  wk_set <- sort(unique(sc$week))

  score <- matrix(NA_real_, length(wk_set), length(teams), dimnames = list(NULL, tk))
  opp   <- matrix(NA_integer_, length(wk_set), length(teams), dimnames = list(NULL, tk))
  for (i in seq_len(nrow(sc))) {
    wi <- match(sc$week[i], wk_set); ti <- match(sc$franchise_id[i], teams)
    score[wi, ti] <- sc$franchise_score[i]
    opp[wi, ti]   <- sc$opponent_id[i]
  }

  grid <- expand.grid(team_id = teams, sched_id = teams)
  res <- purrr::pmap_dfr(grid, function(team_id, sched_id) {
    a_score <- score[, as.character(team_id)]
    opp_b   <- opp[, as.character(sched_id)]
    faced   <- ifelse(opp_b == team_id, sched_id, opp_b)        # if B played A, A faces B
    their   <- score[cbind(seq_along(wk_set), match(as.character(faced), tk))]
    ok <- !is.na(a_score) & !is.na(their)
    tibble::tibble(team_id, sched_id,
                   wins = sum(a_score[ok] > their[ok]),
                   losses = sum(a_score[ok] < their[ok]),
                   ties = sum(a_score[ok] == their[ok]),
                   games = sum(ok))
  })

  res %>%
    dplyr::left_join(dplyr::transmute(fr, team_id = franchise_id,
                                      team_name = franchise_name,
                                      team_abbrev = franchise_abbrev), by = "team_id") %>%
    dplyr::left_join(dplyr::transmute(fr, sched_id = franchise_id,
                                      sched_abbrev = franchise_abbrev), by = "sched_id") %>%
    dplyr::mutate(is_actual = team_id == sched_id)
}

#' Per-team schedule-luck range from the swap matrix: actual vs best/worst record
#' achievable under any team's schedule.
swap_summary <- function(swap) {
  swap %>%
    dplyr::group_by(team_id, team_name, team_abbrev) %>%
    dplyr::summarise(
      actual = wins[is_actual],
      best   = max(wins),
      worst  = min(wins),
      swing  = max(wins) - min(wins),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(actual), dplyr::desc(best))
}

#' Weekly power ranking = blend of recency-weighted scoring and all-play win%.
#'
#' Each week, a team's all-play win% is the share of the rest of the league it
#' outscored that week (schedule-independent). Both that all-play% and the team's
#' raw points are smoothed with a causal EWMA (weight halves every `half_life`
#' weeks), then each is standardized across the league within the week (z-score)
#' and blended:
#'   power_z = score_weight * z(points) + (1 - score_weight) * z(all-play%).
#' Blending raw points back in rewards the *magnitude* of big weeks, which the
#' per-week-capped all-play% alone flattens. `power_index` rescales `power_z` to a
#' friendly mean-50 / SD-15 scale (≈ 0–100). Teams are ranked 1..N each week by
#' `power_z`. Returns one row per team-week (keeps `ap_smooth`/`pf_smooth` so the
#' table and trend views can show the underlying numbers).
#'
#' @param half_life Weeks for the recency weight to halve (smaller = more recent).
#' @param score_weight Weight on scoring vs all-play in [0,1] (0 = all-play only,
#'   1 = scoring only, 0.5 = even).
power_rankings <- function(ff, season, half_life = 3, score_weight = 0.5, through_week = NULL) {
  d <- team_weekly_scores(ff, season)
  if (!is.null(through_week)) d <- dplyr::filter(d, week <= through_week)
  lambda <- 0.5^(1 / half_life)

  d %>%
    # weekly all-play share (schedule-independent)
    dplyr::group_by(week) %>%
    dplyr::mutate(
      wk_allplay = purrr::map_dbl(franchise_score, ~ sum(franchise_score < .x)) /
        (dplyr::n() - 1)
    ) %>%
    dplyr::ungroup() %>%
    # recency-weighted (causal EWMA) all-play% and points, per team
    dplyr::arrange(franchise_id, week) %>%
    dplyr::group_by(franchise_id, franchise_name, franchise_abbrev) %>%
    dplyr::mutate(
      ap_smooth = ewma_decay(wk_allplay, lambda),
      pf_smooth = ewma_decay(franchise_score, lambda)
    ) %>%
    dplyr::ungroup() %>%
    # standardize each across the league within the week, blend, rank
    dplyr::group_by(week) %>%
    dplyr::mutate(
      power_z     = score_weight * zscore(pf_smooth) + (1 - score_weight) * zscore(ap_smooth),
      # restandardize the blend so the index is exactly mean-50 / SD-15 each week
      # (the blend's own SD < 1 because its two inputs are correlated)
      power_index = 50 + 15 * zscore(power_z),
      rank        = rank(-power_z, ties.method = "min")
    ) %>%
    dplyr::ungroup()
}

# ---- Lineup efficiency / Points-Over-Projected ------------------------------

# League starting lineup: 2 QB, 2 RB, 3 WR, 1 TE, 1 RB/WR/TE flex, 1 DST, 1 K.
LINEUP_SLOTS <- c(QB = 2, RB = 2, WR = 3, TE = 1, FLEX = 1, DST = 1, K = 1)

#' Best possible points from a roster given the league's lineup slots.
#' Greedily fills fixed positions with the top scorers, then the flex from the
#' best remaining RB/WR/TE. (Greedy is optimal here because the only shared slot
#' is a single flex over RB/WR/TE.)
optimal_lineup_points <- function(pos, score, slots = LINEUP_SLOTS) {
  score[is.na(score)] <- 0
  ord <- order(score, decreasing = TRUE)
  pos <- pos[ord]; score <- score[ord]
  used <- logical(length(score))
  total <- 0
  for (p in c("QB", "RB", "WR", "TE", "DST", "K")) {
    idx <- utils::head(which(pos == p & !used), slots[[p]])
    total <- total + sum(score[idx]); used[idx] <- TRUE
  }
  flex <- utils::head(which(pos %in% c("RB", "WR", "TE") & !used), slots[["FLEX"]])
  total + sum(score[flex])
}

#' Team lineup efficiency: actual starting points vs the optimal lineup, points
#' left on the bench, and points over projected. One row per team for a season.
#' Per team-week starting points, projected points, and optimal-lineup points,
#' with the two deviations: pop (actual − projected) and gap_optimal
#' (actual − optimal, always ≤ 0 = points left on the bench).
team_week_efficiency <- function(ff, season, through_week = NULL) {
  st <- ff$starters %>% dplyr::filter(season == !!season, is_regular, lineup_slot != "IR")
  if (!is.null(through_week)) st <- dplyr::filter(st, week <= through_week)

  abbrev <- ff$franchises %>%
    dplyr::filter(season == !!season) %>%
    dplyr::select(franchise_id, franchise_abbrev)

  # A 0 projection means ESPN returned no projection for that player-week (e.g.
  # 2023 wk1, where the projection feed is missing). Count valid (>0) projections
  # so a team-week with mostly missing data gets proj = NA instead of a bogus tiny
  # total (which would otherwise blow up Points-Over-Projected).
  valid <- function(p) !is.na(p) & p > 0
  st %>%
    dplyr::group_by(franchise_id, franchise_name, week) %>%
    dplyr::summarise(
      actual  = sum(player_score[is_starter]),
      optimal = optimal_lineup_points(pos, player_score),
      n_start = sum(is_starter),
      n_proj  = sum(is_starter & valid(projected_score)),
      proj    = sum(projected_score[is_starter & valid(projected_score)]),
      .groups = "drop"
    ) %>%
    dplyr::left_join(abbrev, by = "franchise_id") %>%
    dplyr::mutate(
      proj = ifelse(n_proj >= 0.5 * n_start, proj, NA_real_),
      pop  = actual - proj,
      gap_optimal = actual - optimal
    ) %>%
    dplyr::select(-n_start, -n_proj)
}

#' Team lineup efficiency for a season: actual vs optimal/projected totals,
#' points left on the bench, and % of optimal captured. One row per team.
lineup_efficiency <- function(ff, season, through_week = NULL) {
  team_week_efficiency(ff, season, through_week) %>%
    dplyr::group_by(franchise_id, franchise_name, franchise_abbrev) %>%
    dplyr::summarise(weeks = dplyr::n(),
                     actual = sum(actual), optimal = sum(optimal),
                     proj = sum(proj, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(bench_left = optimal - actual,
                  pop = actual - proj,
                  efficiency = actual / optimal) %>%
    dplyr::arrange(dplyr::desc(efficiency))
}

#' Per-player points-over-projected for the performance scatter.
#'
#' Returns one row per qualifying player with mean projected points/game
#' (`mean_proj`, the x-axis), mean actual (`mean_score`), and mean points over
#' projection/game (`mean_pop`, the y-axis), computed over the selected `weeks`.
#' Eligibility is judged over the **full regular season**: a player must have
#' started at least `min_games` games (with a projection) across the whole season
#' to appear, even though the plotted values reflect only the chosen week range.
#' Each player's "fantasy team" is the franchise that most recently started them
#' within the range (so mid-season adds/drops/trades resolve to the current team).
#' @param weeks Optional vector of weeks for the plotted values (default: all weeks).
#' @param min_games Full-season minimum games to qualify for the plot.
#' @param include_bench If TRUE, count all rostered games (started + benched), not
#'   just started games — for both eligibility and the plotted averages. (Truly
#'   unrostered weeks aren't in the data, so this is "all rostered" not "all weeks".)
player_pop_range <- function(ff, season, weeks = NULL, min_games = 4, include_bench = FALSE) {
  base <- ff$starters %>%
    dplyr::filter(season == !!season, is_regular,
                  !is.na(projected_score), projected_score > 0)
  if (!include_bench) base <- dplyr::filter(base, is_starter)

  # eligibility: games started (with a projection) across the FULL regular season
  eligible <- base %>%
    dplyr::count(player_id, name = "season_games") %>%
    dplyr::filter(season_games >= min_games)

  d <- if (is.null(weeks)) base else dplyr::filter(base, week %in% weeks)
  d <- dplyr::semi_join(d, eligible, by = "player_id")

  # fantasy team = the franchise that most recently started the player in-range
  last_team <- d %>%
    dplyr::group_by(player_id) %>%
    dplyr::slice_max(week, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id, franchise_id, franchise_name)

  d %>%
    dplyr::group_by(player_id, player_name, pos) %>%
    dplyr::summarise(
      games = dplyr::n(),
      mean_score = mean(player_score),
      mean_proj  = mean(projected_score),
      mean_pop   = mean(player_score - projected_score),
      .groups = "drop"
    ) %>%
    dplyr::left_join(last_team, by = "player_id") %>%
    dplyr::left_join(eligible, by = "player_id")
}

#' One player's week-by-week game log for the season: actual, projected, points
#' over projection, and whether they started (vs were benched) each week.
#' @param pid player_id (ESPN athlete id).
#' @param include_bench If TRUE, also include benched (rostered) weeks; otherwise
#'   only started weeks. Bye/inactive weeks (no projection) are excluded either way.
player_gamelog <- function(ff, season, pid, include_bench = FALSE) {
  d <- ff$starters %>%
    dplyr::filter(season == !!season, is_regular,
                  player_id == !!pid, !is.na(projected_score), projected_score > 0)
  if (!include_bench) d <- dplyr::filter(d, is_starter)
  d %>%
    dplyr::arrange(week) %>%
    dplyr::transmute(week, player_name, pos, franchise_name, is_starter,
                     actual = player_score, proj = projected_score,
                     pop = player_score - projected_score)
}

# ---- Draft value / outcomes -------------------------------------------------

#' All drafted picks (every season) joined to each player's season production.
#' Shared base for both the per-pick grading and the expected-by-slot curve.
draft_production <- function(ff) {
  prod <- ff$playerscores %>% dplyr::select(season, player_id, actual_points = score_total)
  ff$draft %>%
    dplyr::left_join(prod, by = c("season", "player_id")) %>%
    dplyr::mutate(actual_points = dplyr::coalesce(actual_points, 0))
}

#' Fit the per-position expected-by-slot model and return a predictor
#' `f(pick) -> expected points`. A **log-linear** fit (`points ~ log(pick)`):
#' a smooth, monotone-decreasing decay (steep early, flat late) that won't wiggle
#' for positions taken in a narrow late range (K/DST) or bend back up mid-draft
#' (the small-sample U-shape loess produced for QB). Fit on ALL seasons' picks, so
#' the baseline is the same every year; sparse positions fall back to the mean.
fit_pos_expected <- function(overall, actual) {
  if (length(overall) < 5 || dplyr::n_distinct(overall) < 3) {
    mu <- mean(actual)
    return(function(pick) rep(mu, length(pick)))
  }
  fit <- stats::lm(actual ~ log(overall))
  function(pick) as.numeric(stats::predict(fit, newdata = data.frame(overall = pick)))
}

#' Grade each draft pick (one season) vs its slot's expectation FOR ITS POSITION.
#' Production = the player's season fantasy-point total (`playerscores`); the
#' expected-by-slot baseline is fit per position on ALL seasons combined, so it's
#' constant year-to-year and value is fair across positions (a 2QB league's QBs
#' don't all look like steals). VOE = actual - expected (positive = steal).
draft_outcomes <- function(ff, season) {
  draft_production(ff) %>%
    dplyr::group_by(pos) %>%
    dplyr::mutate(expected_points = fit_pos_expected(overall, actual_points)(overall)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(season == !!season) %>%
    dplyr::mutate(voe = actual_points - expected_points)
}

#' Season-independent expected-by-slot curve, per position: the pooled fit
#' evaluated over a dense pick grid (its observed range). The plot draws THIS, so
#' the curve is identical every year (it doesn't trace one season's pick positions).
draft_value_curve <- function(ff) {
  draft_production(ff) %>%
    dplyr::group_by(pos) %>%
    dplyr::group_modify(function(g, key) {
      f    <- fit_pos_expected(g$overall, g$actual_points)
      grid <- seq(min(g$overall), max(g$overall))
      tibble::tibble(overall = grid, expected_points = f(grid))
    }) %>%
    dplyr::ungroup()
}

#' Team-level draft ROI: total value over expected across a team's picks, for the
#' graded skill positions only. K/DST are excluded by default — they go in a
#' narrow late band where slot expectation is mostly noise, so their swings would
#' drown the signal of who actually drafts well.
team_draft_roi <- function(outcomes, positions = POS_DEFAULT) {
  outcomes %>%
    dplyr::filter(pos %in% positions) %>%
    dplyr::group_by(franchise_id, franchise_name) %>%
    dplyr::summarise(picks = dplyr::n(),
                     actual_points = sum(actual_points),
                     voe = sum(voe), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(voe))
}

#' Team x position draft ROI: total value-over-expected (and pick count) per team
#' per graded position. Feeds the by-position heatmap (who drafts each position
#' best). K/DST excluded for the same reason as `team_draft_roi()`.
team_pos_roi <- function(outcomes, positions = POS_DEFAULT) {
  outcomes %>%
    dplyr::filter(pos %in% positions) %>%
    dplyr::group_by(franchise_id, franchise_name, pos) %>%
    dplyr::summarise(picks = dplyr::n(), voe = sum(voe), .groups = "drop") %>%
    dplyr::mutate(pos = factor(pos, levels = positions))
}

#' One team's full draft board for a season: every pick in draft order with its
#' production, slot expectation, and value-over-expected. Includes K/DST (it's the
#' literal draft, unlike the skill-only team ROI). `fid` is a franchise_id.
team_draft_board <- function(outcomes, fid) {
  outcomes %>%
    dplyr::filter(franchise_id == fid) %>%
    dplyr::arrange(overall) %>%
    dplyr::transmute(Pick = overall, Player = player_name, Pos = pos,
                     Points = round(actual_points),
                     Expected = round(expected_points),
                     VOE = round(voe))
}

#' Long frame of each team's played weekly scores for a season. Defaults to the
#' regular season so playoff weeks (incl. the merged championship) don't distort
#' league-performance metrics; pass `regular_only = FALSE` to include playoffs.
team_weekly_scores <- function(ff, season, regular_only = TRUE) {
  ff$schedule %>%
    dplyr::filter(season == !!season, is_played, !regular_only | is_regular) %>%
    dplyr::select(week, franchise_id, franchise_name, franchise_abbrev,
                  franchise_score, opponent_score, result) %>%
    dplyr::arrange(franchise_id, week)
}
