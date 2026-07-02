# Data Dictionary

All tables live as `.rds` files, produced by `data-raw/03_clean.R` from the ESPN
snapshot. The Shiny app reads **only** these files. Every table carries a `season`
column (2023, 2024, 2025) so seasons can be combined or filtered.

> **Note:** the published dataset is `data-public/` (built by
> `data-raw/05_make_public_data.R`), in which team names are anonymized to **Team
> A–H**; the real-name `data/` is local-only. The schema is identical — only the
> `franchise_name`/`franchise_abbrev` (and schedule `opponent_*`) display strings
> differ. So wherever this dictionary says a "team name", the public data shows
> "Team A", "Team B", … instead.

**League context:** 8-team ESPN league, **2QB** scoring (two starting QB slots),
PPR. Starting lineup = 2 QB, 2 RB, 3 WR, 1 TE, 1 RB/WR/TE flex, 1 DST, 1 K (11
starters). Playoffs = top 4 (H2H wins, then points-for).

**Stable identity:** team *names* change between seasons (owners rename), but
`franchise_id` is stable across seasons — use it as the cross-season team key.

---

## `franchises.rds` — one row per team-season (24 rows)
| column | type | description |
|---|---|---|
| season | int | League season |
| franchise_id | int | Stable team id (consistent across seasons) |
| franchise_name | chr | Team name that season |
| franchise_abbrev | chr | Short team code |

_(Owner names/GUIDs from the API are intentionally dropped — not used by the app.)_

## `league_info.rds` — one row per season (3 rows)
| column | type | description |
|---|---|---|
| season | int | League season |
| league_id | chr | ESPN league id |
| league_name | chr | League name |
| league_type | chr | Redraft/keeper/etc. |
| qb_type | chr | QB configuration (2QB) |
| franchise_count | int | Number of teams (8) |
| roster_size | int | Roster size |
| keeper_count | int | Keepers allowed |

## `schedule.rds` — one row per team-week (392 rows)
Grain: each team's matchup in each week (every matchup appears twice, once per side).
| column | type | description |
|---|---|---|
| season | int | League season |
| week | int | Week number |
| franchise_id / franchise_name / franchise_abbrev | int/chr | The team |
| franchise_score | dbl | Team's points that week |
| opponent_id / opponent_name / opponent_abbrev | int/chr | The opponent |
| opponent_score | dbl | Opponent's points that week |
| result | chr | "W"/"L"/"T" (NA if not yet played) |
| is_played | lgl | TRUE if the matchup has a result |
| margin | dbl | franchise_score − opponent_score |
| is_regular | lgl | TRUE for regular-season weeks (see structure below) |
| week_type | chr | "Regular" / "Playoff R1" / "Championship" |
| week_label | chr | Display label; the merged championship shows e.g. "16–17" |

*Season structure* (from `data-raw/season_structure.csv`):

| season | regular season | playoffs |
|---|---|---|
| 2023 | weeks 1–15 | wk 16 (R1), wk 17 (championship) — single weeks |
| 2024 | weeks 1–14 | wk 15 (R1), wk 16 = championship (**NFL wks 16+17 merged by ESPN → ~2× score**) |
| 2025 | weeks 1–14 | wk 15 (R1), wk 16 = championship (**merged, ~2× score**) |

All league-performance analyses default to `is_regular` weeks, so the merged
championship never distorts the metrics. Playoff weeks remain in the data
(labeled via `week_type`/`week_label`) for a future playoff view.

## `standings.rds` — one row per team-season (24 rows)
ESPN's standings as of the snapshot (straight from `ff_standings`). Includes
`league_rank`, `h2h_wins/losses/ties`, `h2h_winpct`, `points_for`,
`points_against`, and ESPN's own `allplay_wins/losses/winpct`.

## `starters.rds` — one row per rostered player-week (7,194 rows)
Grain: each player on a roster in each week, with the slot they occupied.
| column | type | description |
|---|---|---|
| season | int | League season |
| week | int | Week number |
| franchise_id / franchise_name | int/chr | Team |
| franchise_score | dbl | Team total that week |
| lineup_slot | chr | Slot (QB, RB, WR, TE, RB/WR/TE, DST, K, BE, IR) |
| is_starter | lgl | TRUE unless slot is BE/IR |
| player_id / player_name / pos / team | int/chr | Player identity |
| player_score | dbl | Actual fantasy points |
| projected_score | dbl | ESPN projected points (for Points-Over-Projected) |
| is_regular | lgl | TRUE for regular-season weeks (lineup/POP/draft-value default to these) |

*Note:* `player_score`/`projected_score` here are the player-level source used
across the app. (`scoringhistory` is pulled but not needed by the app; the live
pull requests it per-season, fixing the original snapshot's season bug.)

## `draft.rds` — one row per draft pick (432 rows)
| column | type | description |
|---|---|---|
| season | int | League season |
| round | int | Draft round |
| pick | int | Pick within the round |
| overall | int | Overall pick number |
| franchise_id / franchise_name | int/chr | Drafting team |
| player_id / player_name / pos / team | int/chr | Player drafted |

## `draft_adp.rds` — draft picks joined to FFC 2QB ADP (432 rows: 144 picks × 3 seasons)
Every pick in `draft.rds` left-joined to **Fantasy Football Calculator** 2QB ADP
(12-team), pulled by `data-raw/adp/pull_ffc_adp.R` and matched on normalized
name + position. ~91% (2023) / ~94% (2024) of picks match — deep picks and most
DSTs aren't in FFC's top ~190. **FFC has no 2025 data, so 2025 picks carry `NA`
adp.** Replaces the old 2023-only manual consensus file. Caveat: 12-team 2QB ADP
slightly under-rates QB scarcity vs this 8-team league.
| column | type | description |
|---|---|---|
| season | int | League season |
| overall_pick | int | Actual overall draft slot |
| round | int | Draft round |
| player / player_id / pos / nfl_team | chr/int | Player drafted |
| franchise_id / franchise_name | int/chr | Drafting team (`franchise_id` = stable color key) |
| adp | dbl | FFC 2QB average draft position (NA if unmatched, or any 2025 pick) |
| times_drafted / stdev | dbl | FFC sample size / ADP spread |
| pick_vs_adp | dbl | overall_pick − adp; **positive = value** (fell past ADP), negative = reach |

_Currently **unused by the app** (the Draft Analysis tab was removed); retained for a planned ADP fold-in into the Draft Value tab._

## `playerscores.rds` — season fantasy-point totals per player (3,000 rows: ~1,000 × 3 seasons)
ESPN's per-player season totals/averages for the whole player pool
(`ff_playerscores`), not just rostered players. The **Draft Value** tab uses
`score_total` as a pick's production (joined to draft picks by `player_id`).
| column | type | description |
|---|---|---|
| season | int | League season |
| player_id | int | ESPN athlete id |
| player_name / pos | chr | Player |
| score_total | dbl | Total fantasy points that season |
| score_average | dbl | Average fantasy points per game |

## `playoff_sims.rds` — precomputed playoff-odds simulation (320 rows)
Grain: one row per team-season-week. Produced by **`data-raw/04_playoff_sims.R`**
(not `03_clean.R`) — the player-level Monte Carlo run for the end of every week of
every season. The Playoff Projections tab reads this instead of simulating live; the
sim is deterministic (seeded), so the odds are stable. **Re-run `04_playoff_sims.R`
after refreshing the cleaned data.**
| column | type | description |
|---|---|---|
| season | int | League season |
| from_week | int | Odds computed as of the end of this week |
| franchise_id / franchise_name / franchise_abbrev | int/chr | Team |
| playoff_pct | dbl | Simulated probability of a top-4 (playoff) finish |
| proj_wins | dbl | Mean simulated final regular-season wins |
| proj_pf | dbl | Mean simulated final points-for |
| seed1 … seed4 | dbl | Probability of finishing as the #1 … #4 seed |
