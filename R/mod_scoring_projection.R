# =============================================================================
# mod_scoring_projection.R  --  Scoring vs Projection tab (under Performance)
# Each team's weekly points relative to its projection (actual − projected), one
# panel per team. The raw weekly-scores small-multiples live on the Overview page.
# =============================================================================

#' Faceted weekly points-over-projected bars (actual − projected), one per team.
#' Weeks with no projection data (proj = NA, e.g. 2023 wk1) are dropped but the
#' x-axis keeps the full range so the gap is visible, with a caption naming them.
plot_weekly_scoring <- function(twe, season = NULL) {
  dropped <- sort(unique(twe$week[is.na(twe$pop)]))
  wk_rng  <- range(twe$week)                     # full range incl. dropped weeks
  twe <- twe %>% dplyr::filter(!is.na(pop))
  note <- if (length(dropped))
    sprintf("No player projection data for week%s %s%s — bars omitted.",
            if (length(dropped) > 1) "s" else "",
            paste(dropped, collapse = ", "),
            if (!is.null(season)) paste0(" of the ", season, " season") else "")
  means <- twe %>%
    dplyr::group_by(franchise_name) %>%
    dplyr::summarise(mean = mean(pop), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(mean)) %>%
    dplyr::mutate(franchise_name = factor(franchise_name, levels = franchise_name),
                  lab = paste0("Mean = ", round(mean, 1)), positive = mean >= 0)
  d <- twe %>% dplyr::mutate(
    franchise_name = factor(franchise_name, levels = levels(means$franchise_name)),
    positive = pop >= 0,
    tip = sprintf("%s — wk %d: %.0f actual vs %.0f projected · %+.1f",
                  as.character(franchise_name), week, actual, proj, pop),
    did = paste0(franchise_id, "_", week))

  ggplot2::ggplot(d, ggplot2::aes(week, pop, fill = positive)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = tip, data_id = did), width = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = ff_colors$rule) +
    ggplot2::geom_text(data = means, ggplot2::aes(label = lab, color = positive),
                       x = -Inf, y = Inf, hjust = -0.06, vjust = 1.5,
                       inherit.aes = FALSE, size = 3.2, fontface = "bold") +
    ggplot2::facet_wrap(ggplot2::vars(franchise_name), nrow = 2) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = ff_colors$over, `FALSE` = ff_colors$under), guide = "none") +
    ggplot2::scale_color_manual(values = c(`TRUE` = ff_colors$over, `FALSE` = ff_colors$under), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(c(1, seq(4, wk_rng[2], by = 4)))),
      limits = c(wk_rng[1] - 0.5, wk_rng[2] + 0.5)) +
    ggplot2::labs(
      title = "Weekly Points Over Projected",
      subtitle = "Actual − projected, per team; <span style='color:#0072B2;'>over</span> / <span style='color:#D55E00;'>under</span> projection; label = season mean",
      x = "Week", y = "Actual − projected", caption = note
    ) +
    theme_ff()
}

mod_scoring_projection_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    ggiraph::girafeOutput(ns("pop"), height = "560px"),
    shiny::helpText(
      "Each team's weekly points minus its projected total. Hover a bar for the ",
      "matchup numbers. Regular season only."
    )
  )
}

mod_scoring_projection_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {
    twe <- shiny::reactive(team_week_efficiency(ff, season()))
    output$pop <- ggiraph::renderGirafe(
      ff_girafe(plot_weekly_scoring(twe(), season()), highlight = FALSE))
  })
}
