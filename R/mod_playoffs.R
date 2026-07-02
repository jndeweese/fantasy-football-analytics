# =============================================================================
# mod_playoffs.R  --  Playoff Projections tab
# Player-level Monte Carlo (see R/utils_sim.R), but PRECOMPUTED offline by
# data-raw/04_playoff_sims.R and read from data/playoff_sims.rds (the sim is
# deterministic, so there's no reason to run it live). The table reads the saved
# odds for the selected week; the trajectory plot shows all weeks; the slider just
# filters. Re-run 04_playoff_sims.R whenever the cleaned data is refreshed.
# =============================================================================

#' Playoff-projection table for one week: playoff odds (+ per-seed odds), current
#' wins (as of that week), simulated projected final wins, and actual final wins.
playoff_table <- function(res, ff, season, from_week, reg_end) {
  wins_to <- function(maxwk) {
    ff$schedule %>%
      dplyr::filter(season == !!season, is_played, is_regular, week <= maxwk) %>%
      dplyr::group_by(franchise_id) %>%
      dplyr::summarise(w = sum(result == "W"), .groups = "drop")
  }
  cur <- wins_to(from_week) %>% dplyr::rename(cur_w = w)
  fin <- wins_to(reg_end)   %>% dplyr::rename(fin_w = w)

  # round AFTER scaling to % so values like 0.7 don't pick up float noise
  seed_pct <- round((res %>% dplyr::select(dplyr::starts_with("seed"))) * 100, 1)
  names(seed_pct) <- paste0("#", seq_len(ncol(seed_pct)))

  res %>%
    dplyr::left_join(cur, by = "franchise_id") %>%
    dplyr::left_join(fin, by = "franchise_id") %>%
    dplyr::transmute(
      Team = franchise_name,
      `Playoff %` = round(100 * playoff_pct, 1),
      `W to date` = dplyr::coalesce(cur_w, 0L),
      `Proj W`    = round(proj_wins, 1),
      `Final W`   = fin_w
    ) %>%
    dplyr::bind_cols(seed_pct) %>%
    # Playoff % leads (primary result), with its per-seed breakdown right beside it
    dplyr::relocate(dplyr::starts_with("#"), .after = `Playoff %`)
}

#' Line plot of each team's simulated playoff odds after each week of the season.
plot_playoff_trajectory <- function(sims) {
  pal  <- ff_team_palette(dplyr::distinct(sims, franchise_id, franchise_name))
  ends <- sims %>% dplyr::group_by(franchise_name) %>% dplyr::slice_max(from_week, n = 1) %>% dplyr::ungroup()

  ggplot2::ggplot(sims, ggplot2::aes(from_week, playoff_pct, color = franchise_name, group = franchise_name)) +
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = as.character(franchise_id), tooltip = franchise_name), linewidth = 1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(data_id = as.character(franchise_id),
                   tooltip = sprintf("%s — through wk %d: %.0f%% playoff odds",
                                     franchise_name, from_week, 100 * playoff_pct)), size = 1.5) +
    geom_ff_endlabel(ends, ggplot2::aes(label = franchise_abbrev, color = franchise_name,
                                        data_id = as.character(franchise_id), tooltip = franchise_name)) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(
      title = "Playoff Odds Over the Season",
      subtitle = "Simulated playoff probability using games through each week; odds firm up as the season plays out",
      x = "Through week", y = "Playoff odds"
    ) +
    theme_ff()
}

mod_playoffs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Playoff Projections (player-level Monte Carlo simulation)",
        bslib::popover(
          shiny::icon("circle-info", title = "How it works"),
          title = "How the playoff projection works",
          shiny::p("Each remaining week, every team's score is the sum of bootstrap ",
                   "draws from its recent starters' game logs, shrunk toward",
                   "ESPN's player projections; the shrinkage factor decreases", 
                   "as more games are played. The rest of the schedule is",
                   "replayed 10,000 times and the ", shiny::strong("top 4"),
                   " (wins, then points-for) make the playoffs."),
          shiny::p("Early in the season, with only a few games played, the model ",
                   "stays deliberately uncertain about each team's true strength, ",
                   "so the odds don't overreact to a small sample.")
        )
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 290,
          shiny::uiOutput(ns("week_ui")),
          shiny::helpText(
            shiny::HTML("<b>Playoff %</b> &amp; <b>#1–#4</b>: playoff / per-seed odds. ",
                        "<b>W to date</b>: wins through the selected week. ",
                        "<b>Proj W</b>: simulated final wins. <b>Final W</b>: actual final wins.")
          )
        ),
        DT::DTOutput(ns("seeds"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      ggiraph::girafeOutput(ns("traj"), height = "460px"),
      shiny::helpText(
        "Simulated playoff odds as of the end of each week. ",
        "Click a team in the table above to highlight it, or hover a line or its end label."
      )
    )
  )
}

mod_playoffs_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Regular-season end is fixed per season (2023 -> 15, 2024/25 -> 14).
    reg_end <- shiny::reactive(reg_season_end(ff, season()))

    output$week_ui <- shiny::renderUI({
      cw <- current_week(ff, season()); re <- reg_end()
      shiny::sliderInput(ns("from_week"), "Show table as of end of week",
                         min = 1, max = re - 1, value = min(cw, re - 2), step = 1)
    })

    # Precomputed odds for every week of the season (data-raw/04_playoff_sims.R).
    # Both the table and the trajectory plot read from this; the slider just filters.
    sims_all <- shiny::reactive({
      shiny::validate(shiny::need(
        !is.null(ff$playoff_sims),
        "Playoff simulations not found — run data-raw/04_playoff_sims.R."))
      ff$playoff_sims %>% dplyr::filter(season == season())
    })

    sim_week <- shiny::reactive({
      shiny::req(input$from_week)
      sims_all() %>% dplyr::filter(from_week == input$from_week)
    })

    # franchise_ids in the table's data order, so a clicked row maps to that
    # team's line in the trajectory plot below (which keys data_id on franchise_id).
    selected_team <- shiny::reactive({
      i <- input$seeds_rows_selected
      if (is.null(i) || length(i) == 0) NULL else as.character(sim_week()$franchise_id)[i]
    })

    output$seeds <- DT::renderDT({
      DT::datatable(
        playoff_table(sim_week(), ff, season(), input$from_week, reg_end()),
        rownames = FALSE, selection = "single",
        options = list(dom = "t", paging = FALSE, order = list(list(1, "desc"))),
        class = "compact stripe hover"
      ) %>%
        DT::formatStyle("Playoff %",
          background = DT::styleColorBar(c(0, 100), "#9ecae1"),
          backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center")
    })

    output$traj <- ggiraph::renderGirafe(
      ff_girafe(plot_playoff_trajectory(sims_all()), highlight = TRUE,
                selected = selected_team()))
  })
}
