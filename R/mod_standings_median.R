# =============================================================================
# mod_standings_median.R  --  Standings vs Median (all-play) tab
# Each team's actual record alongside its all-play record (vs every other team's
# weekly score) and its record vs the weekly league median. The gap between
# actual and all-play win% is schedule luck.
# =============================================================================

#' Cumulative-luck trend: one interactive line per team of running wins above/below
#' all-play expectation, with a zero reference for neutral luck.
plot_luck_trend <- function(traj) {
  pal  <- ff_team_palette(dplyr::distinct(traj, franchise_id, franchise_name))
  ends <- traj %>% dplyr::group_by(franchise_name) %>% dplyr::slice_max(week, n = 1) %>% dplyr::ungroup()

  ggplot2::ggplot(traj, ggplot2::aes(week, cum_luck, color = franchise_name, group = franchise_name)) +
    ggplot2::geom_hline(yintercept = 0, color = ff_colors$rule, linewidth = 0.6) +
    ggiraph::geom_line_interactive(
      ggplot2::aes(data_id = as.character(franchise_id), tooltip = franchise_name), linewidth = 1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(data_id = as.character(franchise_id),
                   tooltip = sprintf("%s — through wk %d: %+.1f luck wins",
                                     franchise_name, week, cum_luck)), size = 1.5) +
    geom_ff_endlabel(ends, ggplot2::aes(label = franchise_abbrev, color = franchise_name,
                                        data_id = as.character(franchise_id), tooltip = franchise_name)) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2),
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(
      title = "Cumulative Luck Over the Season",
      subtitle = paste0("Running actual − expected (all-play) wins; ",
                        "above 0 = lucky,",
                        "below 0 = unlucky"),
      x = "Week", y = "Cumulative luck (wins)"
    ) +
    theme_ff()
}

#' Format the records + luck table for display. The "W-L" columns are strings, so
#' trailing hidden numeric keys (`.*_sort`) are added for correct ordering — the DT
#' points each text column's `orderData` at its key (see the module).
allplay_table <- function(rec) {
  rec %>%
    dplyr::transmute(
      Team = franchise_name,
      Record = paste0(wins, "-", losses, ifelse(ties > 0, paste0("-", ties), "")),
      `All-Play` = paste0(ap_wins, "-", ap_losses),
      `All-Play %` = round(100 * allplay_pct, 1),
      `vs Median` = paste0(med_wins, "-", med_losses),
      `Close (W-L)` = paste0(close_w, "-", close_l),
      `Luck (W)` = round(luck_wins, 1),
      .record_sort  = win_pct,
      .allplay_sort = allplay_pct,
      .median_sort  = med_wins - med_losses,
      .close_sort   = close_w - close_l
    )
}

#' Heatmap of the schedule-swap matrix (rows/cols ordered by actual wins).
plot_schedule_swap <- function(swap) {
  ord  <- swap %>% dplyr::filter(is_actual) %>% dplyr::arrange(dplyr::desc(wins)) %>% dplyr::pull(team_abbrev)
  gmax <- max(swap$games)
  d <- swap %>%
    dplyr::mutate(team_abbrev  = factor(team_abbrev, levels = rev(ord)),
                  sched_abbrev = factor(sched_abbrev, levels = ord),
                  tip = sprintf("%s with %s's schedule: %d-%d",
                                as.character(team_abbrev), as.character(sched_abbrev),
                                wins, games - wins),
                  did = paste0(as.character(team_abbrev), "_", as.character(sched_abbrev)))

  ggplot2::ggplot(d, ggplot2::aes(sched_abbrev, team_abbrev, fill = wins)) +
    ggiraph::geom_tile_interactive(ggplot2::aes(tooltip = tip, data_id = did),
                                   color = "white", linewidth = 0.6) +
    ggplot2::geom_tile(data = dplyr::filter(d, is_actual),
                       fill = NA, color = "black", linewidth = 1.1) +
    # numbers stay plain black (readable on the fill) and non-interactive (no hover change)
    ggplot2::geom_text(ggplot2::aes(label = wins), size = 4, color = "black") +
    ggplot2::scale_fill_gradient2(low = "#f4b183", mid = "#f2f2f2", high = "#9cc3e0",
                                  midpoint = gmax / 2, guide = "none") +
    ggplot2::labs(
      title = "Record Under Every Team's Schedule",
      subtitle = "Row = a team's wins if it played that column's schedule; black box = actual record",
      x = "…playing this team's schedule", y = NULL
    ) +
    theme_ff() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Per-team schedule-luck summary table (actual vs best/worst possible).
swap_table <- function(swap) {
  swap_summary(swap) %>%
    dplyr::transmute(Team = team_name, Actual = actual, Best = best, Worst = worst,
                     `Swing (luck)` = swing)
}

mod_standings_median_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("kpis")),
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = "right", width = 250,
          shiny::uiOutput(ns("week_ui")),
          shiny::helpText(
            "Every metric reflects the selected week range (drag either handle ",
            "to focus on a span, e.g. the second half)."
          )
        ),
        DT::DTOutput(ns("tbl"))
      ),
      shiny::helpText(
        shiny::HTML(
          "<b>All-Play</b>: record vs every team each week. ",
          "<b>vs Median</b>: record vs the weekly league median. ",
          "<b>Close (W-L)</b>: games decided by &le;10 pts. ",
          "<b>Luck (W)</b>: actual wins minus wins expected from all-play."
        )
      )
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Luck over time",
        ggiraph::girafeOutput(ns("luck"), height = "480px"),
        shiny::helpText(
          "Each team's running wins above/below all-play expectation; the last ",
          "point equals its Luck (W) in the table. Click a team in the table above ",
          "to highlight it, or hover a line or its end label."
        )
      ),
      bslib::nav_panel(
        "Schedule comparison",
        ggiraph::girafeOutput(ns("swap"), height = "460px"),
        DT::DTOutput(ns("swap_tbl")),
        shiny::helpText(
          "Each team's record had it played every other team's schedule ",
          "(diagonal = actual). Best / Worst / Swing summarize the range."
        )
      )
    )
  )
}

mod_standings_median_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {

    output$week_ui <- shiny::renderUI({
      cw <- reg_season_end(ff, season())
      shiny::sliderInput(session$ns("week_range"), "Week range",
                         min = 1, max = cw, value = c(1, cw), step = 1)
    })

    # (season, week-range) kept consistent so the page computes ONCE per change.
    # On a season switch the range snaps to the full new season *immediately*,
    # driven by the season rather than the lagging (renderUI) slider — otherwise
    # the metrics compute once with the old season's range (flashing a stale,
    # wrong-length number) before the slider resets. Integer weeks in both branches
    # so the slider's echo of a full range is `identical()` and doesn't recompute.
    query <- shiny::reactiveVal(NULL)
    shiny::observeEvent(season(), {
      query(list(season = season(), weeks = seq_len(reg_season_end(ff, season()))))
    })
    shiny::observeEvent(input$week_range, {
      rng <- as.integer(input$week_range)
      if (rng[2] <= reg_season_end(ff, season()))
        query(list(season = season(), weeks = seq.int(rng[1], rng[2])))
    }, ignoreInit = TRUE)
    q <- shiny::reactive(shiny::req(query()))

    rec <- shiny::reactive({
      # luck_summary() is a superset of all_play_record() (adds close-game record
      # and schedule difficulty), so it powers the table.
      luck_summary(ff, q()$season, weeks = q()$weeks)
    })

    # Headline KPIs: luckiest / unluckiest team (actual − all-play-expected wins) for
    # the selected week range. Matches the Lineup Efficiency card style.
    output$kpis <- shiny::renderUI({
      r     <- rec()
      best  <- r %>% dplyr::slice_max(luck_wins, n = 1, with_ties = FALSE)
      worst <- r %>% dplyr::slice_min(luck_wins, n = 1, with_ties = FALSE)
      meta  <- function(lw) {                 # color the wins-vs-expected by sign
        col <- if (lw >= 0) ff_colors$over else ff_colors$under
        shiny::HTML(sprintf("<span style='color:%s;font-weight:600;'>%+.1f</span> wins vs expected",
                            col, lw))
      }
      bslib::layout_columns(
        fill = FALSE, col_widths = c(6, 6),
        kpi_box("Best luck", best$franchise_name, meta(best$luck_wins),
                "clover", "#009E73", class = "ff-kpi-sm"),
        kpi_box("Worst luck", worst$franchise_name, meta(worst$luck_wins),
                "cloud-rain", "#D55E00", class = "ff-kpi-sm")
      )
    })

    # franchise_ids in the table's data order, so a clicked row highlights that
    # team's line in the luck-over-time plot (which keys data_id on franchise_id).
    selected_team <- shiny::reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || length(i) == 0) NULL else as.character(rec()$franchise_id)[i]
    })

    # cumulative-luck trend (spaghetti lines -> hover-highlight + table-click select)
    output$luck <- ggiraph::renderGirafe(
      ff_girafe(plot_luck_trend(luck_trajectory(ff, q()$season, weeks = q()$weeks)),
                highlight = TRUE, selected = selected_team()))

    output$tbl <- DT::renderDT({
      DT::datatable(
        allplay_table(rec()),
        rownames = FALSE, selection = "single",
        options = list(
          dom = "t", paging = FALSE, order = list(list(3, "desc")),
          columnDefs = list(
            # hide the numeric sort keys, then sort each "W-L" text column by its key
            list(visible = FALSE, targets = 7:10),
            list(orderData = 7,  targets = 1),   # Record  -> win%
            list(orderData = 8, targets = 2),   # All-Play -> all-play%
            list(orderData = 9, targets = 4),   # vs Median -> net
            list(orderData = 10, targets = 5)    # Close (W-L) -> net
          )
        ),
        class = "compact stripe hover"
      ) %>%
        DT::formatStyle("Luck (W)",
          color = DT::styleInterval(0, c("#D55E00", "#0072B2")))
    })

    swap <- shiny::reactive({
      schedule_swap(ff, q()$season, weeks = q()$weeks)
    })
    output$swap <- ggiraph::renderGirafe(
      ff_girafe(plot_schedule_swap(swap()), highlight = FALSE))
    output$swap_tbl <- DT::renderDT({
      DT::datatable(
        swap_table(swap()), rownames = FALSE,
        options = list(dom = "t", paging = FALSE, order = list(list(1, "desc"))),
        class = "compact stripe hover"
      )
    })
  })
}
