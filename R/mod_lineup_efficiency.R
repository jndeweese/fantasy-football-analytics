# =============================================================================
# mod_lineup_efficiency.R  --  Lineup Efficiency tab (under Performance)
# How well each team set its lineup: actual vs optimal lineup points (% of optimal
# captured) and points left on the bench. Weekly points-vs-projection lives on the
# Weekly Scoring tab; player over/under-performance on Player Performance.
# =============================================================================

#' Faceted weekly lineup-efficiency lines (% of optimal), one panel per team,
#' with the league-average week curve (dashed) repeated in each panel.
plot_weekly_efficiency <- function(twe) {
  d <- twe %>% dplyr::mutate(eff = actual / optimal)
  league <- d %>% dplyr::group_by(week) %>% dplyr::summarise(eff = mean(eff), .groups = "drop")
  season_eff <- d %>%
    dplyr::group_by(franchise_id, franchise_name) %>%
    dplyr::summarise(avg = sum(actual) / sum(optimal), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(avg))
  lev <- season_eff$franchise_name
  d <- d %>% dplyr::mutate(franchise_name = factor(franchise_name, levels = lev),
                           tip = sprintf("%s — wk %d: %.0f%% efficiency (%.0f of %.0f optimal)",
                                         as.character(franchise_name), week, 100 * eff, actual, optimal),
                           did = paste0(franchise_id, "_", week))
  season_eff <- season_eff %>%
    dplyr::mutate(franchise_name = factor(franchise_name, levels = lev),
                  lab = paste0("Mean = ", scales::percent(avg, accuracy = 1)))
  pal <- ff_team_palette(dplyr::distinct(twe, franchise_id, franchise_name))

  ggplot2::ggplot(d, ggplot2::aes(week, eff)) +
    ggplot2::geom_line(data = league, ggplot2::aes(week, eff), inherit.aes = FALSE,
                       color = ff_colors$median, linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(color = franchise_name), linewidth = 1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(color = franchise_name, tooltip = tip, data_id = did), size = 1.6) +
    ggplot2::geom_text(data = season_eff, ggplot2::aes(label = lab),
                       x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.7,
                       inherit.aes = FALSE, color = ff_colors$anno, size = 3.1) +
    ggplot2::facet_wrap(ggplot2::vars(franchise_name), nrow = 2) +
    ggplot2::scale_color_manual(values = pal, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(4)) +
    ggplot2::labs(
      title = "Weekly Lineup Efficiency",
      subtitle = "% of the optimal lineup started each week; <span style='color:#9aa0a8;'>dashed = league average</span>",
      x = "Week", y = "Efficiency"
    ) +
    theme_ff()
}

#' Per-team lineup-efficiency summary table.
efficiency_table <- function(eff) {
  eff %>%
    dplyr::transmute(
      Team = franchise_name,
      Efficiency = scales::percent(efficiency, accuracy = 0.1),
      `Pts benched` = round(bench_left)
    )
}

mod_lineup_efficiency_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")),
    bslib::card(
      bslib::card_header("Lineup efficiency by team"),
      DT::DTOutput(ns("tbl")),
      shiny::helpText(
        "Efficiency = actual starting points as a share of the optimal lineup. ",
        "Pts benched = points left on the bench. Regular season only."
      )
    ),
    bslib::card(
      full_screen = TRUE,
      ggiraph::girafeOutput(ns("eff_weekly"), height = "560px")
    )
  )
}

mod_lineup_efficiency_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {
    twe <- shiny::reactive(team_week_efficiency(ff, season()))
    eff <- shiny::reactive(lineup_efficiency(ff, season()))

    output$kpis <- shiny::renderUI({
      e <- eff()
      best <- e %>% dplyr::slice_max(efficiency, n = 1)
      worst <- e %>% dplyr::slice_max(bench_left, n = 1)
      bslib::layout_columns(
        fill = FALSE, col_widths = c(6, 6),
        kpi_box("Best lineup efficiency", scales::percent(best$efficiency, accuracy = 0.1),
                best$franchise_name, "bullseye", "#009E73"),
        kpi_box("Most points benched", round(worst$bench_left),
                worst$franchise_name, "couch", "#D55E00")
      )
    })

    output$eff_weekly <- ggiraph::renderGirafe(
      ff_girafe(plot_weekly_efficiency(twe()), highlight = FALSE))
    output$tbl <- DT::renderDT({
      DT::datatable(efficiency_table(eff()), rownames = FALSE,
        class = "compact stripe hover",
        options = list(dom = "t", paging = FALSE,
          columnDefs = list(
            list(className = "dt-left",   targets = 0),
            list(className = "dt-center", targets = c(1, 2))
          )))
    })
  })
}
