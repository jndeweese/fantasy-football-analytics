# =============================================================================
# mod_power_rankings.R  --  Power Rankings tab
# Power = blended z-score of recency-weighted scoring + all-play win% (see
# power_rankings() in utils_metrics). Current ranking table (centerpiece) + a
# sub-tabbed card: a multi-metric Trends view (scoring / all-play% / power index)
# and the weekly rank bump chart.
# =============================================================================

#' Bump chart of weekly power rank (1 = best), one interactive line per team.
plot_power_bump <- function(pr) {
  pal <- ff_team_palette(dplyr::distinct(pr, franchise_id, franchise_name))
  ends <- pr %>% dplyr::group_by(franchise_id) %>% dplyr::slice_max(week, n = 1) %>% dplyr::ungroup()

  ggplot2::ggplot(pr, ggplot2::aes(week, rank, color = franchise_name, group = franchise_name)) +
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = as.character(franchise_id), tooltip = franchise_name), linewidth = 1.1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(data_id = as.character(franchise_id),
                   tooltip = paste0(franchise_name, " — wk ", week, " · rank ", rank)),
      size = 2.4) +
    ggiraph::geom_text_repel_interactive(
      data = ends,
      ggplot2::aes(label = franchise_abbrev, data_id = as.character(franchise_id), tooltip = franchise_name),
      direction = "y", hjust = 0, nudge_x = 0.35, segment.color = NA,
      bg.color = "white", bg.r = 0.12, size = 3.3, fontface = "bold",
      min.segment.length = Inf
    ) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_y_reverse(breaks = 1:dplyr::n_distinct(pr$franchise_id)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(title = "Power Ranking by Week",
                  subtitle = "Rank by the blended power score (scoring + all-play); 1 = best",
                  x = "Week", y = "Rank") +
    theme_ff()
}

#' Interactive weekly trend lines for a chosen metric off power_rankings():
#' the blended power index, recency-weighted scoring, or all-play %. League
#' average dashed in for reference; end labels + hover/click highlighting.
plot_trend <- function(pr, metric = c("power", "score", "allplay")) {
  metric <- match.arg(metric)
  spec <- switch(metric,
    power   = list(col = "power_index", title = "Power Index Trend",
                   sub = "Blend of scoring & all-play, rescaled to mean 50 / SD 15; <span style='color:#9aa0a8;'>dashed = league average</span>",
                   ylab = "Power index", pct = FALSE),
    score   = list(col = "pf_smooth", title = "Scoring Trend",
                   sub = "Recency-weighted weekly points; <span style='color:#9aa0a8;'>dashed = league average</span>",
                   ylab = "Smoothed points", pct = FALSE),
    allplay = list(col = "ap_smooth", title = "All-Play % Trend",
                   sub = "Recency-weighted all-play win%; <span style='color:#9aa0a8;'>dashed = league average</span>",
                   ylab = "All-play %", pct = TRUE)
  )
  d <- pr %>% dplyr::mutate(y = .data[[spec$col]])
  league <- d %>% dplyr::group_by(week) %>% dplyr::summarise(y = mean(y), .groups = "drop")
  pal  <- ff_team_palette(dplyr::distinct(pr, franchise_id, franchise_name))
  ends <- d %>% dplyr::group_by(franchise_name) %>% dplyr::slice_max(week, n = 1) %>% dplyr::ungroup()
  fmt  <- if (spec$pct) function(v) paste0(round(100 * v), "%") else function(v) round(v)

  p <- ggplot2::ggplot(d, ggplot2::aes(week, y, color = franchise_name, group = franchise_name)) +
    ggplot2::geom_line(data = league, ggplot2::aes(week, y), inherit.aes = FALSE,
                       color = ff_colors$median, linetype = "dashed", linewidth = 0.8) +
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = as.character(franchise_id), tooltip = franchise_name), linewidth = 1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(data_id = as.character(franchise_id),
                   tooltip = paste0(franchise_name, " — wk ", week, ": ", fmt(y))),
      size = 1.6) +
    geom_ff_endlabel(ends, ggplot2::aes(label = franchise_abbrev, color = franchise_name,
                                        data_id = as.character(franchise_id), tooltip = franchise_name)) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(title = spec$title, subtitle = spec$sub, x = "Week", y = spec$ylab) +
    theme_ff()
  if (spec$pct) p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  p
}

#' Inline SVG sparkline of a team's weekly rank trajectory (rank 1 = top).
rank_sparkline <- function(ranks, color, n_teams, w = 88, h = 22) {
  n <- length(ranks)
  if (n < 2) return("")
  x <- seq(3, w - 3, length.out = n)
  y <- 3 + (ranks - 1) / (n_teams - 1) * (h - 6)   # rank 1 near the top
  pts <- paste0(round(x, 1), ",", round(y, 1), collapse = " ")
  sprintf(
    '<svg width="%d" height="%d" style="vertical-align:middle"><polyline points="%s" fill="none" stroke="%s" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"/><circle cx="%.1f" cy="%.1f" r="2.6" fill="%s"/></svg>',
    w, h, pts, color, x[n], y[n], color)
}

#' Current power ranking table: team-color dot, medals for the top 3, the blended
#' Power index (data bar) plus its two inputs (Pts/wk, All-play %), a weekly-rank
#' sparkline, and a colored week-over-week change.
#' Returns HTML cells (render the DT with escape = FALSE).
power_table <- function(pr) {
  pal <- ff_team_palette(dplyr::distinct(pr, franchise_id, franchise_name))
  n_teams <- dplyr::n_distinct(pr$franchise_id)
  last_wk <- max(pr$week)

  spark <- pr %>%
    dplyr::arrange(franchise_id, week) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(Trend = rank_sparkline(rank, pal[[franchise_name[1]]], n_teams),
                     .groups = "drop")
  prev <- pr %>% dplyr::filter(week == last_wk - 1) %>%
    dplyr::select(franchise_id, prev_rank = rank)

  medal <- c("\U0001F947", "\U0001F948", "\U0001F949")  # gold / silver / bronze
  pr %>%
    dplyr::filter(week == last_wk) %>%
    dplyr::left_join(prev, by = "franchise_id") %>%
    dplyr::left_join(spark, by = "franchise_id") %>%
    dplyr::arrange(rank) %>%
    dplyr::transmute(
      Rank = ifelse(rank <= 3, paste0(medal[rank], " ", rank), as.character(rank)),
      Team = sprintf('<span style="display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:7px;vertical-align:middle"></span>%s',
                     pal[franchise_name], franchise_name),
      Power = round(power_index),
      `Pts/wk` = round(pf_smooth),
      `All-play %` = round(100 * ap_smooth),
      Trend = Trend,
      `Δ wk` = dplyr::case_when(
        is.na(prev_rank)     ~ '<span style="color:#9aa0a8">–</span>',
        prev_rank - rank > 0 ~ sprintf('<span style="color:#0072B2">▲ %d</span>', prev_rank - rank),
        prev_rank - rank < 0 ~ sprintf('<span style="color:#D55E00">▼ %d</span>', rank - prev_rank),
        TRUE                 ~ '<span style="color:#9aa0a8">–</span>'
      )
    )
}

mod_power_rankings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Current Power Ranking",
        bslib::popover(
          shiny::icon("circle-info", title = "How it works"),
          title = "How the power ranking works",
          shiny::p("Each week we take two recency-weighted measures of a team: its ",
                   shiny::strong("all-play win%"), " (share of the league it outscored ",
                   "that week, schedule-independent) and its ", shiny::strong("points scored"),
                   ". Both weight recent weeks more — the weight halves every ",
                   shiny::em("half-life"), " weeks."),
          shiny::p("Each measure is standardized across the league and blended per the ",
                   shiny::strong("scoring weight"), " slider; teams are ranked by the blend."),
          shiny::p("The ", shiny::strong("Power"), " column rescales that blend to a ",
                   "mean-50 / SD-15 index (≈ a 0–100 scale). Both sliders also drive the ",
                   shiny::strong("Trends"), " charts below.")
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = "right", width = 260,
          shiny::sliderInput(ns("half_life"), "Recency weight half-life (weeks)",
                             min = 1, max = 8, value = 3, step = .5),
          shiny::helpText("Half-life: lower = recent weeks dominate."),
          shiny::sliderInput(ns("score_weight"), "Scoring weight (vs all-play)",
                             min = 0, max = 100, value = 50, step = 5, post = "%"),
          shiny::helpText("Scoring weight: 0% = all-play only, 100% = scoring only.")
        ),
        DT::DTOutput(ns("tbl"))
      )
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Trends",
        shiny::selectInput(ns("trend_metric"), NULL, width = "220px",
          choices = c("Power index" = "power", "Scoring" = "score", "All-play %" = "allplay"),
          selected = "power"),
        ggiraph::girafeOutput(ns("trend"), height = "470px"),
        shiny::helpText("Weekly trajectory of the chosen metric (uses the sliders above). Click a team in the table to highlight it, or hover a line or its end label.")
      ),
      bslib::nav_panel(
        "Rank by week",
        ggiraph::girafeOutput(ns("bump"), height = "470px"),
        shiny::helpText("Click a team in the table to highlight it, or hover a line or its end label.")
      )
    )
  )
}

mod_power_rankings_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {

    pr <- shiny::reactive({
      shiny::req(input$half_life, input$score_weight)
      power_rankings(ff, season(), half_life = input$half_life,
                     score_weight = input$score_weight / 100)
    })

    # Teams in table (display) order, so a clicked row maps to a franchise_name
    # (which is the data_id used by both interactive plots).
    # franchise_id (clean key, no apostrophes/spaces) is the plots' data_id, so
    # selection matches even for team names containing apostrophes or spaces.
    ranked_teams <- shiny::reactive({
      pr() %>% dplyr::filter(week == max(week)) %>%
        dplyr::arrange(rank) %>% dplyr::pull(franchise_id) %>% as.character()
    })
    selected_team <- shiny::reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || length(i) == 0) NULL else ranked_teams()[i]
    })

    output$tbl <- DT::renderDT({
      tb <- power_table(pr())
      v <- tb[["Power"]]
      DT::datatable(
        tb, rownames = FALSE, escape = FALSE, selection = "single",
        options = list(
          dom = "t", paging = FALSE, ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0, 2, 3, 4, 5, 6)),
            list(className = "dt-left",   targets = 1)
          )
        ),
        class = "compact hover row-border"
      ) %>%
        DT::formatStyle(
          "Power",
          background = DT::styleColorBar(c(min(v) - 3, max(v)), "#bfe0f2"),
          backgroundSize = "90% 60%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center", fontWeight = "500"
        )
    })

    # Both plots re-render with the table-clicked team pre-selected (bold, others
    # faded); hovering a line or its end label highlights too.
    output$bump <- ggiraph::renderGirafe(
      ff_girafe(plot_power_bump(pr()), selected = selected_team())
    )
    output$trend <- ggiraph::renderGirafe({
      shiny::req(input$trend_metric)
      ff_girafe(plot_trend(pr(), input$trend_metric), selected = selected_team())
    })
  })
}
