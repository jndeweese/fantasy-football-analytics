# =============================================================================
# mod_draft_value.R  --  Draft Value / Hit-Rate tab
# Outcome-based draft grading: each pick's actual production (player's season point
# total, from playerscores) vs what its draft slot typically returns FOR ITS
# POSITION. Faceted by position (2 cols) on a fixed y-axis [0, global max] so the
# scoring gap between positions is visible and the axis is stable across seasons.
# Sidebar mirrors Player Performance: player search, position + team filters with
# All/None. See draft_outcomes() (utils_metrics.R) for the data.
# =============================================================================

#' Faceted scatter of draft slot vs production, one panel per position (2 cols),
#' fixed y-axis [0, `ymax`] shared across panels/seasons. `curve` is the
#' season-independent expected-by-slot line (`draft_value_curve()`). All picks stay
#' visible; the team checkboxes fade (not remove) the unselected teams via a
#' plot-level **alpha** (`emphasize_teams`) — independent of the `data_id`-based
#' player search/click. The searched player (`highlight_pid`) is kept full so the
#' search (which fades the rest via selection) still pops. Points colored by team,
#' keyed on `player_id` for tooltip + search/click.
plot_draft_value_scatter <- function(points, curve, emphasize_teams = NULL,
                                      highlight_pid = NULL, ymax = NULL,
                                      label_extremes = TRUE) {
  pts <- points %>% dplyr::mutate(pos = factor(pos, levels = POS_LEVELS))
  emp <- if (is.null(emphasize_teams)) unique(pts$franchise_name) else emphasize_teams
  hp  <- if (is.null(highlight_pid)) -1L else as.integer(highlight_pid)
  # Team-emphasis alpha (points + labels). The searched/clicked player is kept full
  # so it still pops; the search/click *fade* itself is done client-side by ggiraph
  # (opts_selection_inv on data_id), so it's instant — the labels are interactive
  # (below) so they fade in step with the points, not a beat later via a re-render.
  pts$emph <- ifelse(pts$franchise_name %in% emp | pts$player_id == hp, 1, 0.05)
  crv <- curve %>% dplyr::mutate(pos = factor(pos, levels = POS_LEVELS)) %>% dplyr::arrange(pos, overall)
  pal <- ff_team_palette(dplyr::distinct(pts, franchise_id, franchise_name))

  # Headline annotations: name the top 3 steals and bottom 3 busts in each facet.
  ext <- dplyr::bind_rows(
    pts %>% dplyr::group_by(pos) %>% dplyr::slice_max(voe, n = 3, with_ties = FALSE),
    pts %>% dplyr::group_by(pos) %>% dplyr::slice_min(voe, n = 3, with_ties = FALSE)
  ) %>% dplyr::ungroup() %>% dplyr::distinct() %>% dplyr::mutate(steal = voe >= 0)

  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = crv, ggplot2::aes(overall, expected_points),
                       color = "grey55", linewidth = 0.9) +
    ggiraph::geom_point_interactive(
      data = pts,
      ggplot2::aes(overall, actual_points, color = franchise_name, alpha = emph,
                   data_id = as.character(player_id),
                   tooltip = sprintf(
                     "<b>%s</b> (%s) · %s<br>Pick %d · %.0f pts · <b>%+.0f</b> vs expected",
                     player_name, pos, franchise_name, overall, actual_points, voe)),
      size = 2.6) +
    ggplot2::scale_alpha_identity() +
    ggplot2::facet_wrap(ggplot2::vars(pos), ncol = 2) +
    ggplot2::scale_color_manual(values = pal, name = NULL) +
    ggplot2::labs(
      title = "Draft Slot vs Production, by Position",
      subtitle = "Each position vs its own expected-by-slot curve (grey); above = steal, below = bust",
      x = "Overall draft pick", y = "Fantasy points (season total)"
    ) +
    theme_ff() +
    ggplot2::theme(legend.position = "none")
  if (label_extremes && nrow(ext)) {
    # Interactive labels (data_id = player_id) so ggiraph fades them client-side, in
    # step with the points, on search/click — instant, no server re-render. alpha =
    # emph still fades them server-side on team de-emphasis. Same tooltip as the
    # point, so the label is also a hover target.
    lab <- function(d, col) ggiraph::geom_text_repel_interactive(
      data = d, ggplot2::aes(overall, actual_points, label = player_name, alpha = emph,
                             data_id = as.character(player_id),
                             tooltip = sprintf(
                               "<b>%s</b> (%s) · %s<br>Pick %d · %.0f pts · <b>%+.0f</b> vs expected",
                               player_name, pos, franchise_name, overall, actual_points, voe)),
      color = col, size = 2.9, fontface = "bold", bg.color = "white", bg.r = 0.12,
      seed = 1, min.segment.length = 0, segment.color = "grey65",
      point.padding = 0.2, max.overlaps = Inf, inherit.aes = FALSE, show.legend = FALSE)
    g <- g + lab(dplyr::filter(ext, steal), ff_colors$over) +
             lab(dplyr::filter(ext, !steal), ff_colors$under)
  }
  if (!is.null(ymax)) g <- g + ggplot2::coord_cartesian(ylim = c(0, ymax))
  g
}

#' Team draft ROI bars (interactive): total value over expected across a team's
#' skill-position picks. Hover for the numbers; clicking a bar (data_id =
#' franchise_id, apostrophe-safe) loads that team's draft board below.
plot_team_draft_roi <- function(roi) {
  d <- roi %>%
    dplyr::mutate(franchise_name = forcats::fct_reorder(franchise_name, voe),
                  positive = voe >= 0,
                  tip = sprintf("<b>%s</b><br>%+.0f pts vs expected · %d picks",
                                franchise_name, voe, picks))
  ggplot2::ggplot(d, ggplot2::aes(voe, franchise_name, fill = positive)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = tip, data_id = as.character(franchise_id)), width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = ff_colors$rule) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = ff_colors$over, `FALSE` = ff_colors$under),
                               guide = "none") +
    ggplot2::labs(title = "Team Draft ROI",
                  subtitle = "Total points above/below pick-slot expectation",
                  x = "Value over expected (pts)", y = NULL) +
    theme_ff()
}

#' By-position draft ROI heatmap (interactive): teams (rows, best total at top) x
#' graded positions (cols), tiles colored by total value-over-expected (blue =
#' value, orange = reach) with the value printed; hover for picks + VOE. Cells a
#' team never drafted show grey.
plot_team_pos_roi_heat <- function(tpr) {
  full <- tidyr::complete(tpr, tidyr::nesting(franchise_id, franchise_name), pos,
                          fill = list(picks = 0L, voe = NA_real_))
  tot <- full %>% dplyr::group_by(franchise_name) %>%
    dplyr::summarise(total = sum(voe, na.rm = TRUE), .groups = "drop")
  d <- full %>% dplyr::left_join(tot, by = "franchise_name") %>%
    dplyr::mutate(
      team = forcats::fct_reorder(franchise_name, total),
      hot  = !is.na(voe) & abs(voe) > 70,
      tip  = ifelse(is.na(voe),
                    sprintf("<b>%s</b> · %s<br>no picks", franchise_name, pos),
                    sprintf("<b>%s</b> · %s<br>%+.0f vs expected · %d pick%s",
                            franchise_name, pos, voe, picks, ifelse(picks == 1, "", "s"))))
  ggplot2::ggplot(d, ggplot2::aes(pos, team, fill = voe)) +
    ggiraph::geom_tile_interactive(
      ggplot2::aes(tooltip = tip, data_id = paste0(franchise_id, "_", pos)),
      color = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(voe), "–", sprintf("%+.0f", voe)),
                                    color = hot), size = 3.3, fontface = "bold",
                       show.legend = FALSE) +
    ggplot2::scale_fill_gradient2(low = ff_colors$under, mid = "white", high = ff_colors$over,
                                  midpoint = 0, na.value = "grey90", name = "VOE",
                                  labels = function(x) sprintf("%+.0f", x)) +
    ggplot2::scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#333333")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(title = "Draft Value by Position",
                  subtitle = "Total points over/under expected per team × position",
                  x = NULL, y = NULL) +
    theme_ff() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.text.y = ggplot2::element_text(face = "bold"))
}

mod_draft_value_ui <- function(id) {
  ns <- shiny::NS(id)
  toggle_links <- function(all_id, none_id) shiny::span(
    shiny::actionLink(ns(all_id), "All"), " · ",
    shiny::actionLink(ns(none_id), "None"), class = "small")

  bslib::navset_tab(
    # --- By Player: per-pick scatter + the headline steal/bust of the draft ----
    bslib::nav_panel(
      "By Player",
      shiny::uiOutput(ns("kpis_player")),
      bslib::card(
        full_screen = TRUE,
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = "right", width = 280,
            shiny::selectizeInput(
              ns("search"), "Find a player", choices = NULL,
              options = list(
                placeholder = "Type a name…",
                onInitialize = I('function() { this.setValue(""); }'))),
            shiny::div(
              class = "d-flex justify-content-between align-items-center",
              shiny::tags$label("Positions", class = "control-label mb-0"),
              toggle_links("pos_all", "pos_none")),
            shiny::checkboxGroupInput(
              ns("pos"), NULL, choices = POS_LEVELS, selected = POS_DEFAULT, inline = TRUE),
            shiny::div(
              class = "d-flex justify-content-between align-items-center",
              shiny::tags$label("Fantasy teams", class = "control-label mb-0"),
              toggle_links("teams_all", "teams_none")),
            shiny::uiOutput(ns("team_ui")),
            shiny::helpText(
              "Production = each player's season fantasy-point total, graded against ",
              "that position's expected-by-slot curve. Above = steal, below = bust.")
          ),
          ggiraph::girafeOutput(ns("scatter"), height = "760px")
        )
      )
    ),
    # --- By Team: ROI + by-position skill, the draft board, best/worst drafter --
    bslib::nav_panel(
      "By Team",
      shiny::uiOutput(ns("kpis_team")),
      bslib::layout_columns(
        col_widths = c(5, 7),
        bslib::card(
                    ggiraph::girafeOutput(ns("roi"), height = "420px"),
                    shiny::helpText("Skill positions only (K/DST excluded). ",
                                    "Click a team to load its draft board below.")),
        bslib::card(
                    shiny::div(class = "ff-heatmap",
                               ggiraph::girafeOutput(ns("heat"), height = "420px")),
                    shiny::helpText("Click a cell to filter the board to that ",
                                    "team's picks at that position."))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Team Draft Board",
          shiny::div(class = "ff-board-team", shiny::uiOutput(ns("board_team_ui")))
        ),
        DT::DTOutput(ns("board"))
      )
    )
  )
}

mod_draft_value_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    season_teams <- shiny::reactive(sort(season_franchises(ff, season())$franchise_name))

    # Global production max across ALL seasons -> fixed y-axis so the scale is the
    # same across positions and across years (computed once; ff is constant).
    y_max <- local({
      prod <- ff$playerscores %>% dplyr::select(season, player_id, sc = score_total)
      m <- ff$draft %>% dplyr::left_join(prod, by = c("season", "player_id")) %>%
        dplyr::summarise(m = max(dplyr::coalesce(sc, 0))) %>% dplyr::pull(m)
      ceiling(m / 20) * 20
    })

    # Season-independent expected-by-slot curve (same every year); computed once.
    dv_curve <- draft_value_curve(ff)

    outcomes_all <- shiny::reactive(draft_outcomes(ff, season()))

    output$team_ui <- shiny::renderUI({
      team_chip_checkboxes(ns("teams"), season_franchises(ff, season()))
    })

    # All/None quick toggles for the position + team filters
    shiny::observeEvent(input$pos_all,
      shiny::updateCheckboxGroupInput(session, "pos", selected = POS_LEVELS))
    shiny::observeEvent(input$pos_none,
      shiny::updateCheckboxGroupInput(session, "pos", selected = character(0)))
    shiny::observeEvent(input$teams_all,
      shiny::updateCheckboxGroupInput(session, "teams", selected = season_teams()))
    shiny::observeEvent(input$teams_none,
      shiny::updateCheckboxGroupInput(session, "teams", selected = character(0)))

    # `input$teams` is NULL both before the dynamic checkbox initializes AND when
    # every team is unchecked. Distinguish them: until it first reports a value,
    # treat as "emphasize all" (avoids an init flash); afterward an empty selection
    # means "fade all" (character(0)), not "show all" (NULL).
    teams_ready <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$teams, teams_ready(TRUE), once = TRUE)
    emph_teams <- shiny::reactive({
      if (!teams_ready()) NULL
      else if (is.null(input$teams)) character(0)
      else input$teams
    })
    # click-to-highlight on the chips (isolate from all-selected) is handled
    # client-side, once, by team_chip_js() in app.R — no server round-trip.

    # picks for the selected positions (which facets show); ALL teams are kept —
    # the team checkboxes fade (not remove) the unselected teams in the plot.
    pos_data <- shiny::reactive({
      shiny::req(input$pos)
      outcomes_all() %>% dplyr::filter(pos %in% input$pos)
    })

    # keep the search box in sync with the displayed players (preserve a live pick)
    shiny::observe({
      d <- pos_data()
      choices <- stats::setNames(
        as.character(d$player_id),
        sprintf("%s (%s, %s)", d$player_name, d$pos, d$franchise_name))
      cur  <- shiny::isolate(input$search)
      keep <- if (!is.null(cur) && cur %in% choices) cur else ""
      shiny::updateSelectizeInput(session, "search", choices = choices, selected = keep)
    })
    # clicking a point selects that player -> funnel into the search box (single source)
    shiny::observeEvent(input$scatter_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      sel <- input$scatter_selected
      sel <- if (length(sel) && nzchar(sel)) sel else ""
      if (!identical(sel, shiny::isolate(input$search)))
        shiny::updateSelectizeInput(session, "search", selected = sel)
    })
    selected_player <- shiny::reactive({
      s <- input$search
      if (is.null(s) || !nzchar(s)) NULL else s
    })

    # Push pick changes to the girafe *client-side* (ggiraph's "<id>_set" custom
    # message) instead of re-rendering, so a click or a search updates the
    # highlight without blinking the scatter (the re-render was the double refresh).
    shiny::observeEvent(selected_player(), ignoreNULL = FALSE, ignoreInit = TRUE, {
      sp <- selected_player()
      session$sendCustomMessage(paste0(session$ns("scatter"), "_set"),
                                if (is.null(sp)) character(0) else as.character(sp))
    })

    output$scatter <- ggiraph::renderGirafe({
      shiny::validate(shiny::need(length(input$pos) > 0, "Select at least one position."))
      d <- pos_data()
      shiny::validate(shiny::need(nrow(d) > 0, "No picks match these filters."))
      crv <- dv_curve %>% dplyr::filter(pos %in% input$pos)
      # The pick is applied/kept client-side (observer above), so isolate it: the
      # scatter re-renders only on data/filter changes, re-applying the pick then.
      sp <- shiny::isolate(selected_player())
      ff_girafe(plot_draft_value_scatter(d, crv, emphasize_teams = emph_teams(),
                                         highlight_pid = if (is.null(sp)) NULL else as.integer(sp),
                                         ymax = y_max),
                width_svg = 10, height_svg = 9,
                highlight = FALSE, select = TRUE, selected = sp)
    })

    fr_tbl   <- shiny::reactive(dplyr::distinct(season_franchises(ff, season()),
                                                franchise_id, franchise_name))
    roi_data <- shiny::reactive(team_draft_roi(outcomes_all()))

    # Team draft board: the dropdown (defaults to the best drafter) picks the team.
    # Clicking a bar in the ROI chart, or a cell of the by-position heatmap, also
    # selects the team here; a heatmap click additionally remembers its position so
    # the board can highlight that team's picks at that position.
    output$board_team_ui <- shiny::renderUI({
      teams <- sort(fr_tbl()$franchise_name)
      best  <- roi_data() %>% dplyr::slice_max(voe, n = 1, with_ties = FALSE) %>%
        dplyr::pull(franchise_name)
      shiny::selectInput(ns("board_team"), NULL, choices = teams,
                         selected = if (length(best)) best else teams[1], width = "230px")
    })

    set_board_team <- function(fid) {
      nm <- fr_tbl()$franchise_name[match(as.integer(fid), fr_tbl()$franchise_id)]
      if (length(nm) && !is.na(nm) && !identical(nm, shiny::isolate(input$board_team)))
        shiny::updateSelectInput(session, "board_team", selected = nm)
    }

    # heatmap cell selection = list(fid, pos); drives the board's row highlight
    heat_sel <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$roi_selected, {
      sel <- input$roi_selected
      if (length(sel) && nzchar(sel)) { heat_sel(NULL); set_board_team(sel) }
    })
    shiny::observeEvent(input$heat_selected, ignoreNULL = FALSE, {
      sel <- input$heat_selected            # data_id is "<franchise_id>_<pos>"
      if (length(sel) && nzchar(sel)) {
        parts <- strsplit(sel, "_", fixed = TRUE)[[1]]
        heat_sel(list(fid = as.integer(parts[1]), pos = parts[2]))
        set_board_team(parts[1])
      } else {
        heat_sel(NULL)                       # cell deselected -> drop the filter
      }
    })
    # the position filter follows the heatmap cell only: the moment the board moves
    # to a *different* team (via the dropdown or a ROI bar) drop it, so it isn't
    # sticky. A heatmap click sets heat_sel to the new team before the dropdown
    # updates, so this never clears the filter it just set.
    shiny::observeEvent(input$board_team, {
      hl <- heat_sel()
      if (!is.null(hl)) {
        fid <- fr_tbl()$franchise_id[match(input$board_team, fr_tbl()$franchise_name)]
        if (length(fid) && !is.na(fid) && hl$fid != fid) heat_sel(NULL)
      }
    })

    # No `selected =`: the ROI bar carries no outline until the user actually clicks
    # one (the dropdown's default team must not pre-outline a bar). svg sized to its
    # column so the text scale + height match the heatmap.
    output$roi <- ggiraph::renderGirafe(
      ff_girafe(plot_team_draft_roi(roi_data()),
                width_svg = 4.7, height_svg = 4.6,
                highlight = FALSE, select = TRUE, select_fade = FALSE))

    output$heat <- ggiraph::renderGirafe(
      ff_girafe(plot_team_pos_roi_heat(team_pos_roi(outcomes_all())),
                width_svg = 6.6, height_svg = 4.6,
                highlight = FALSE, select = TRUE, select_fade = FALSE))

    # Player headliners (By Player tab): biggest single steal / bust of the draft
    # (skill positions, to match the scatter framing); meta names the drafting team.
    output$kpis_player <- shiny::renderUI({
      o     <- outcomes_all() %>% dplyr::filter(pos %in% POS_DEFAULT)
      steal <- o %>% dplyr::slice_max(voe, n = 1, with_ties = FALSE)
      bust  <- o %>% dplyr::slice_min(voe, n = 1, with_ties = FALSE)
      meta  <- function(r, accent) shiny::HTML(sprintf(
        "%s · Pick %d · %s · <span style='color:%s;font-weight:600;'>%+.0f</span>",
        r$pos, r$overall, r$franchise_name, accent, r$voe))
      bslib::layout_columns(
        fill = FALSE, col_widths = c(6, 6), class = "mt-3",
        kpi_box("Steal of the draft", steal$player_name, meta(steal, ff_colors$over),
                "gem", ff_colors$over, class = "ff-kpi-sm"),
        kpi_box("Bust of the draft", bust$player_name, meta(bust, ff_colors$under),
                "trash", ff_colors$under, class = "ff-kpi-sm")
      )
    })

    # Team headliners (By Team tab): best and worst overall drafter.
    output$kpis_team <- shiny::renderUI({
      r     <- roi_data()
      best  <- r %>% dplyr::slice_max(voe, n = 1, with_ties = FALSE)
      worst <- r %>% dplyr::slice_min(voe, n = 1, with_ties = FALSE)
      meta  <- function(x, accent) shiny::HTML(sprintf(
        "<span style='color:%s;font-weight:600;'>%+.0f</span> pts vs expected",
        accent, x$voe))
      bslib::layout_columns(
        fill = FALSE, col_widths = c(6, 6), class = "mt-3",
        kpi_box("Best draft", best$franchise_name, meta(best, ff_colors$over),
                "trophy", ff_colors$over, class = "ff-kpi-sm"),
        kpi_box("Worst draft", worst$franchise_name, meta(worst, ff_colors$under),
                "thumbs-down", ff_colors$under, class = "ff-kpi-sm")
      )
    })

    output$board <- DT::renderDT({
      shiny::req(input$board_team)
      fid <- fr_tbl()$franchise_id[match(input$board_team, fr_tbl()$franchise_name)]
      shiny::req(length(fid) == 1, !is.na(fid))
      brd <- team_draft_board(outcomes_all(), fid)
      # a heatmap-cell click filters the board to that position, but only while the
      # board is showing the clicked team (switching teams, or clicking a ROI bar,
      # restores the full board)
      hl <- heat_sel()
      if (!is.null(hl) && hl$fid == fid)
        brd <- dplyr::filter(brd, Pos == hl$pos)
      DT::datatable(
        brd,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, order = list(list(0, "asc"))),
        class = "compact stripe hover"
      ) %>%
        DT::formatStyle("VOE", fontWeight = "bold",
                        color = DT::styleInterval(0, c(ff_colors$under, ff_colors$over)))
    })
  })
}
