# =============================================================================
# mod_overview.R  --  League Overview tab
# Top-level KPIs and the Weekly Scores & Ranks small-multiples plot.
# (Scoring-trend exploration lives on the Power Rankings page.)
# =============================================================================

#' Small-multiples of weekly scores with weekly rank labels and W/L coloring,
#' faceted by team (ordered by mean score), league average overlaid in gray.
plot_weekly_scores_ranks <- function(ff, season) {
  sched <- ff$schedule %>% dplyr::filter(season == !!season, is_played, is_regular)

  # Fixed y-range across ALL seasons so facets are comparable year to year.
  yr <- range(ff$schedule$franchise_score[ff$schedule$is_played & ff$schedule$is_regular],
              na.rm = TRUE)

  league_mean <- sched %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(mean = mean(franchise_score), .groups = "drop")

  d <- sched %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(rank = rank(-franchise_score, ties.method = "min")) %>%
    dplyr::ungroup()

  team_stats <- d %>%
    dplyr::group_by(franchise_name) %>%
    dplyr::summarise(mean = mean(franchise_score), sd = sd(franchise_score), .groups = "drop") %>%
    dplyr::mutate(lab = paste0("Mean ", round(mean), " | SD ", round(sd)))

  ord <- team_stats %>% dplyr::arrange(dplyr::desc(mean)) %>% dplyr::pull(franchise_name)
  d <- d %>% dplyr::mutate(
    franchise_name = factor(franchise_name, levels = ord),
    tip = sprintf("%s — wk %d: %d pts · rank %d · %s",
                  as.character(franchise_name), week, round(franchise_score), rank, result),
    did = paste0(franchise_id, "_", week))
  team_stats <- team_stats %>% dplyr::mutate(franchise_name = factor(franchise_name, levels = ord))

  # Label defaults to the top-left; it only drops to the bottom-left when a team's
  # early-week points climb high enough that the top corner is crowded. `ylim` adds
  # headroom above the data so the top label clears the points, and `inset` keeps
  # the label off the panel edges.
  span  <- diff(yr)
  ylim  <- c(yr[1], yr[2] + 0.10 * span)   # extra headroom on top for the label
  inset <- 0.01 * span
  need  <- 0.17 * span                     # vertical room needed to sit on top
  left_cut <- min(d$week) + 0.45 * diff(range(d$week))
  placement <- d %>%
    dplyr::filter(week <= left_cut) %>%
    dplyr::group_by(franchise_name) %>%
    dplyr::summarise(max_left = max(franchise_score), .groups = "drop") %>%
    dplyr::mutate(at_top = (ylim[2] - max_left) >= need,
                  lab_y  = ifelse(at_top, ylim[2] - inset, yr[1] + 4*inset),
                  lab_vjust = ifelse(at_top, 1, 0))
  team_stats <- dplyr::left_join(team_stats, placement, by = "franchise_name")

  ggplot2::ggplot(d, ggplot2::aes(week, franchise_score)) +
    ggplot2::geom_line(data = league_mean, ggplot2::aes(week, mean),
                       color = ff_colors$median, linetype = "42", linewidth = 0.8,
                       inherit.aes = FALSE) +
    ggplot2::geom_line(color = ff_colors$neutral) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = result, tooltip = tip, data_id = did), size = 4) +
    # rank label is non-interactive so the hover outline never darkens the white number
    ggplot2::geom_text(ggplot2::aes(label = rank),
                       color = "white", size = 2.5, fontface = "bold") +
    ggplot2::geom_text(data = team_stats,
                       ggplot2::aes(label = lab, y = lab_y, vjust = lab_vjust),
                       x = min(d$week), hjust = 0, size = 2.9, color = ff_colors$anno,
                       inherit.aes = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(franchise_name), nrow = 2) +
    ggplot2::scale_color_manual(values = c(W = ff_colors$win, L = ff_colors$loss, T = ff_colors$neutral)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::labs(
      title = paste0("Weekly Scores & Ranks — ", season),
      subtitle = "League average in <span style='color:#999999;'>gray</span>; point = <span style='color:#0072B2;'>Win</span> / <span style='color:#D55E00;'>Loss</span>; number = that week's rank",
      x = "Week", y = "Points"
    ) +
    theme_ff()
}

#' Standings table for display (HTML cells -> render the DT with escape = FALSE):
#' team-color dot, record, PF/PA, streak, Power index (data bar), efficiency, luck,
#' and a playoff check for the top 4. Record sorts by win% via a hidden key.
standings_dt <- function(st) {
  pal <- ff_team_palette(dplyr::distinct(st, franchise_id, franchise_name))
  st %>%
    dplyr::transmute(
      Rank = rank,
      Team = sprintf('<span style="display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:7px;vertical-align:middle"></span>%s',
                     pal[franchise_name], franchise_name),
      Record = paste0(wins, "-", losses, ifelse(ties > 0, paste0("-", ties), "")),
      PF = round(pf), PA = round(pa),
      Streak = streak,
      Power = round(power_index),
      `Eff %` = round(100 * efficiency),
      `Luck (W)` = round(luck_wins, 1),
      Playoffs = paste0(round(100 * playoff_pct), "%"),
      .win_sort = win_pct,
      # signed streak length (W = +, L = −) for coloring + correct sorting
      .streak_val = dplyr::case_when(
        startsWith(streak, "W") ~  as.integer(gsub("\\D", "", streak)),
        startsWith(streak, "L") ~ -as.integer(gsub("\\D", "", streak)),
        TRUE ~ 0L)
    )
}

# ---- Module -----------------------------------------------------------------

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")),
    bslib::card(
      bslib::card_header("Standings"),
      # non-fillable body so the table flows to its natural height and the caption
      # sits below it (a fillable card body clips the table and overlaps the text)
      bslib::card_body(
        fillable = FALSE,
        DT::DTOutput(ns("standings")),
        shiny::helpText(shiny::HTML(
          "Ordered by record (wins, then points for); top 4 make the playoffs. ",
          "<b>Power</b> = Power Ranking index (league avg 50). ",
          "<b>Eff %</b> = season lineup efficiency. ",
          "<b>Luck (W)</b> = wins above/below all-play expectation."
        ))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      ggiraph::girafeOutput(ns("weekly"), height = "560px")
    )
  )
}

mod_overview_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {

    output$kpis <- shiny::renderUI({
      s <- season()
      tw <- team_weekly_scores(ff, s)  # regular season only
      team_tot <- tw %>%
        dplyr::group_by(franchise_name) %>%
        dplyr::summarise(pf = sum(franchise_score),
                         w = sum(result == "W"), l = sum(result == "L"),
                         .groups = "drop")
      top_pf   <- team_tot %>% dplyr::slice_max(pf, n = 1)
      best_rec <- team_tot %>% dplyr::arrange(dplyr::desc(w), dplyr::desc(pf)) %>% dplyr::slice(1)

      bslib::layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        kpi_box("Current week", current_reg_week(ff, s),
                paste0("out of ", reg_season_end(ff, s), " regular season weeks"),
                       "calendar-week", "#0072B2"),
        kpi_box("League avg score", round(mean(tw$franchise_score), 1),
                "points per game", "chart-line", "#009E73"),
        kpi_box("Top scorer (PF)", scales::comma(round(top_pf$pf)),
                top_pf$franchise_name, "trophy", "#E69F00"),
        kpi_box("Best record", paste0(best_rec$w, "-", best_rec$l),
                best_rec$franchise_name, "medal", "#CC79A7")
      )
    })

    output$standings <- DT::renderDT({
      st <- standings_dt(team_standings(ff, season()))
      pw <- st[["Power"]]
      DT::datatable(
        st, rownames = FALSE, escape = FALSE,
        options = list(
          dom = "t", paging = FALSE, order = list(list(0, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = c(10, 11)),  # hidden sort/color keys
            list(orderData = 10, targets = 2),           # Record sorts by win%
            list(orderData = 11, targets = 5),           # Streak sorts by signed length
            list(className = "dt-center", targets = c(0, 2, 3, 4, 5, 6, 7, 8, 9))
          ),
          # Playoff cut line under the 4th row — but only while the table is sorted
          # by Rank ascending (the standings order); clear it on any other sort.
          # Set on the cells (a <tr> border won't paint in a border-collapsed table).
          drawCallback = DT::JS(
            "function() {",
            "  var api = this.api();",
            "  var nodes = api.rows({order:'current'}).nodes();",
            "  $(nodes).find('td').css('border-bottom', '');",
            "  var ord = api.order();",
            "  if (ord.length && ord[0][0] === 0 && ord[0][1] === 'asc' && nodes.length >= 4) {",
            "    $(nodes[3]).find('td').css('border-bottom', '2px solid #15314B');",
            "  }",
            "}")
        ),
        class = "compact stripe hover row-border"
      ) %>%
        DT::formatStyle("Power",
          background = DT::styleColorBar(c(min(pw) - 3, max(pw)), "#bfe0f2"),
          backgroundSize = "90% 60%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center", fontWeight = "500") %>%
        DT::formatStyle("Streak", valueColumns = ".streak_val",
          color = DT::styleInterval(0, c("#D55E00", "#0072B2"))) %>%
        DT::formatStyle("Luck (W)", color = DT::styleInterval(0, c("#D55E00", "#0072B2")))
    })

    output$weekly <- ggiraph::renderGirafe(
      ff_girafe(plot_weekly_scores_ranks(ff, season()), highlight = FALSE))
  })
}
