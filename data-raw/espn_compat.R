# =============================================================================
# espn_compat.R  --  ESPN API compatibility shims for ffscrapr 1.4.8
# -----------------------------------------------------------------------------
# Two issues with the current ESPN API that break the stock package:
#
#   1. Host moved. Authenticated reads now require
#      `lm-api-reads.fantasy.espn.com`; ffscrapr still calls `fantasy.espn.com`
#      and gets HTML back. patch_ffscrapr_host() rewrites the host at the single
#      low-level fetcher every endpoint funnels through.
#
#   2. Team naming changed. ESPN dropped per-team `location`/`nickname` (which
#      ffscrapr pastes into franchise_name -> "NA NA") in favor of a single
#      `name` field. espn_team_names() reads the real names straight from the
#      mTeam endpoint so the pipeline can restore them.
#
# Pipeline-only; the app never calls ESPN.
# =============================================================================

#' Redirect ffscrapr's ESPN reads to the current API host. Idempotent.
patch_ffscrapr_host <- function(new_host = "lm-api-reads.fantasy.espn.com") {
  ns <- asNamespace("ffscrapr")
  current <- get("espn_getendpoint_raw", envir = ns)
  if (isTRUE(attr(current, "ff_host_patched"))) return(invisible(FALSE))

  orig <- current
  patched <- function(conn, url_query, ...) {
    url_query <- sub("//fantasy.espn.com/", paste0("//", new_host, "/"),
                     url_query, fixed = TRUE)
    orig(conn, url_query, ...)
  }
  attr(patched, "ff_host_patched") <- TRUE
  assignInNamespace("espn_getendpoint_raw", patched, ns = "ffscrapr")
  message("Patched ffscrapr ESPN host -> ", new_host)
  invisible(TRUE)
}

#' Correct per-player projected scores by week, straight from ESPN's boxscore.
#'
#' ffscrapr reads the projection by list position (`stats[[2]]`), but ESPN flips
#' the order of the actual (statSourceId 0) and projected (statSourceId 1) entries
#' week to week, so that value is wrong for ~half the weeks (projected == actual).
#' Here we select the entry explicitly flagged as projected for the week.
#'
#' @return tibble(week, player_id, projected_score).
espn_projections <- function(season, league_id, espn_s2, swid, weeks,
                             host = "lm-api-reads.fantasy.espn.com") {
  cookie <- paste0("espn_s2=", espn_s2, "; SWID=", swid)

  purrr::map_dfr(weeks, function(wk) {
    url <- sprintf(
      "https://%s/apis/v3/games/ffl/seasons/%s/segments/0/leagues/%s?scoringPeriodId=%d&view=mBoxscore&view=mMatchupScore",
      host, season, league_id, wk
    )
    j <- httr2::request(url) |>
      httr2::req_headers(Cookie = cookie, Accept = "application/json") |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    rows <- list()
    for (m in (j$schedule %||% list())) {
      if (!isTRUE(m$matchupPeriodId == wk)) next
      for (side in c("home", "away")) {
        for (e in (m[[side]]$rosterForCurrentScoringPeriod$entries %||% list())) {
          pid   <- e$playerId %||% e$playerPoolEntry$player$id
          stats <- e$playerPoolEntry$player$stats %||% list()
          pr <- purrr::detect(stats, ~ isTRUE(.x$statSourceId == 1) &&
                                       isTRUE(.x$scoringPeriodId == wk) &&
                                       isTRUE(.x$statSplitTypeId == 1))
          rows[[length(rows) + 1]] <- tibble::tibble(
            week = wk,
            player_id = as.integer(pid),
            projected_score = if (is.null(pr) || is.null(pr$appliedTotal)) NA_real_
                              else round(pr$appliedTotal, 1)
          )
        }
      }
    }
    dplyr::bind_rows(rows)
  }) %>% dplyr::distinct(week, player_id, .keep_all = TRUE)
}

#' Fetch real franchise names/abbreviations from ESPN's mTeam endpoint.
#' @return tibble(franchise_id, franchise_name, franchise_abbrev).
espn_team_names <- function(season, league_id, espn_s2, swid,
                            host = "lm-api-reads.fantasy.espn.com") {
  url <- sprintf(
    "https://%s/apis/v3/games/ffl/seasons/%s/segments/0/leagues/%s?view=mTeam",
    host, season, league_id
  )
  teams <- httr2::request(url) |>
    httr2::req_headers(Cookie = paste0("espn_s2=", espn_s2, "; SWID=", swid),
                       Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("teams")

  tibble::tibble(
    franchise_id     = vapply(teams, \(t) as.integer(t$id), integer(1)),
    franchise_name   = vapply(teams, \(t) t$name   %||% NA_character_, character(1)),
    franchise_abbrev = vapply(teams, \(t) t$abbrev %||% NA_character_, character(1))
  )
}
