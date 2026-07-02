# =============================================================================
# mod_player_performance.R  --  Player Performance tab
# Small-multiple scatters (one facet per position) of each qualifying player's
# points over projection per game (y) vs projected points per game (x). Faceting
# keeps each position on its own x-scale (projections differ a lot by position);
# within a facet, right = higher projected ("good on paper"), top = beat
# projection. Points are colored by fantasy team, with rich tooltips, and can be
# filtered by position and team, scoped to a week range, with a name search that
# highlights one player. A toggle includes benched (rostered) games alongside
# started ones. Clicking a point (or searching) drills into that player's week-by-
# week actual vs projected in the game-log card below (benched weeks faded). See
# player_pop_range() and player_gamelog() (utils_metrics.R) for the data.
# =============================================================================

# Full-season games threshold to qualify for talhe plot. An analytic constant (not
# a UI control): eligibility is judged over the whole regular season, while the
# plotted values reflect only the selected week range.
PLAYER_MIN_GAMES <- 4L

# POS_LEVELS / POS_DEFAULT are shared constants defined in theme_ff.R.

#' Faceted quadrant scatter: projected pts/game (x) vs points over projection/game
#' (y), one facet per position (free x-scale). A solid rule at y=0 marks beat-vs-
#' missed projection; a dashed rule at each position's median projection marks
#' good-vs-modest "on paper". Points colored by fantasy team and keyed on
#' `player_id` (apostrophe-safe) for tooltips + search/click. The team checkboxes
#' fade (not remove) the unselected teams via a plot-level **alpha**
#' (`emphasize_teams`), independent of the `data_id`-based player search/click. The
#' searched player (`highlight_pid`) is kept full so the search (which fades the rest
#' via selection) still pops. `ylim` fixes the (shared) y-range.
plot_player_scatter <- function(pp, ylim = NULL, emphasize_teams = NULL, highlight_pid = NULL,
                                label_extremes = TRUE) {
  d   <- pp %>% dplyr::mutate(pos = factor(pos, levels = POS_LEVELS))
  emp <- if (is.null(emphasize_teams)) unique(d$franchise_name) else emphasize_teams
  hp  <- if (is.null(highlight_pid)) -1L else as.integer(highlight_pid)
  d$emph <- ifelse(d$franchise_name %in% emp | d$player_id == hp, 1, 0.05)
  pal <- ff_team_palette(dplyr::distinct(d, franchise_id, franchise_name))
  meds <- d %>% dplyr::group_by(pos) %>%
    dplyr::summarise(xmid = stats::median(mean_proj), .groups = "drop")

  # Headline annotations: name the top 3 over- and bottom 3 under-performers per
  # facet (by points over projection). Interactive (data_id) so they fade in step
  # with the points on search/click; alpha = emph fades them on team de-emphasis.
  ext <- dplyr::bind_rows(
    d %>% dplyr::group_by(pos) %>% dplyr::slice_max(mean_pop, n = 3, with_ties = FALSE),
    d %>% dplyr::group_by(pos) %>% dplyr::slice_min(mean_pop, n = 3, with_ties = FALSE)
  ) %>% dplyr::ungroup() %>% dplyr::distinct() %>% dplyr::mutate(over = mean_pop >= 0)

  g <- ggplot2::ggplot(d, ggplot2::aes(mean_proj, mean_pop)) +
    ggplot2::geom_hline(yintercept = 0, color = ff_colors$rule, linewidth = 0.6) +
    ggplot2::geom_vline(data = meds, ggplot2::aes(xintercept = xmid),
                        color = ff_colors$rule, linetype = "dashed", linewidth = 0.5) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        color = franchise_name, alpha = emph, data_id = as.character(player_id),
        tooltip = sprintf(
          "<b>%s</b> (%s) · %s<br>Projected: %.1f/gm · Actual: %.1f/gm<br><b>%+.1f</b> over projection · %d gms",
          player_name, pos, franchise_name, mean_proj, mean_score, mean_pop, games)),
      size = 2.6) +
    ggplot2::scale_alpha_identity() +
    ggplot2::facet_wrap(ggplot2::vars(pos), ncol = 2, scales = "free_x") +
    ggplot2::scale_color_manual(values = pal, name = NULL) +
    ggplot2::labs(
      title = "Which players over/underperformed their projections?",
      subtitle = paste0("Projected points/game (x) vs points ",
                        "over / under projection/game (y); ",
                        "dashed = that position's median projection"),
      x = "Projected points per game", y = "Points over projection per game"
    ) +
    theme_ff() +
    ggplot2::theme(legend.position = "none")
  if (label_extremes && nrow(ext)) {
    lab <- function(dd, col) ggiraph::geom_text_repel_interactive(
      data = dd, ggplot2::aes(mean_proj, mean_pop, label = player_name, alpha = emph,
                              data_id = as.character(player_id),
                              tooltip = sprintf(
                                "<b>%s</b> (%s) · %s<br>Projected: %.1f/gm · Actual: %.1f/gm<br><b>%+.1f</b> over projection · %d gms",
                                player_name, pos, franchise_name, mean_proj, mean_score, mean_pop, games)),
      color = col, size = 2.9, fontface = "bold", bg.color = "white", bg.r = 0.12,
      seed = 1, min.segment.length = 0, segment.color = "grey65",
      point.padding = 0.2, max.overlaps = Inf, inherit.aes = FALSE, show.legend = FALSE)
    g <- g + lab(dplyr::filter(ext, over), ff_colors$over) +
             lab(dplyr::filter(ext, !over), ff_colors$under)
  }
  if (!is.null(ylim)) g <- g + ggplot2::coord_cartesian(ylim = ylim)
  g
}

#' Drill-down: one player's weekly actual vs projected (a dumbbell per week --
#' grey dot = projected, colored dot = actual, segment = the gap), colored by
#' over/under projection and faded when the player was benched. Dashed lines mark
#' the player's typical range (p25-p75 of weekly points); a caption summarizes
#' spread (SD, CV). The whole dumbbell shares one tooltip/hover. `weeks_hl` shades
#' the currently-selected scatter week range when it's a strict subset of the season.
#' `week_max` fixes the x-axis to the full regular season (1..week_max) so the axis
#' is stable across players/weeks instead of spanning only a player's played weeks.
plot_player_gamelog <- function(gl, weeks_hl = NULL, week_max = NULL) {
  info <- gl[1, ]
  d <- gl %>% dplyr::mutate(
    positive = actual >= proj,
    did = as.character(week),
    tip = sprintf("Wk %d%s: %.1f actual vs %.1f projected (%+.1f)",
                  week, ifelse(is_starter, "", " · benched"), actual, proj, pop))
  ov_un <- c(`TRUE` = ff_colors$over, `FALSE` = ff_colors$under)

  # consistency summary over the games shown: typical range (p25-p75) + mean/volatility.
  n   <- nrow(d); mu <- mean(d$actual); sdv <- stats::sd(d$actual)
  flo <- stats::quantile(d$actual, 0.25, names = FALSE)
  cei <- stats::quantile(d$actual, 0.75, names = FALSE)
  cv_txt <- if (is.finite(sdv) && mu > 0) sprintf(" · CV %.2f", sdv / mu) else ""
  cap <- sprintf("Typical weekly range %.1f–%.1f (p25–p75) · M %.1f · SD %.1f%s · %d games",
                 flo, cei, mu, sdv, cv_txt, n)
  ymax <- max(d$actual, d$proj, cei) * 1.08

  # full season span for the fixed x-axis (falls back to the player's own weeks)
  wlo <- if (is.null(week_max)) min(d$week) else 1
  whi <- if (is.null(week_max)) max(d$week) else week_max
  p <- ggplot2::ggplot(d, ggplot2::aes(week))
  if (!is.null(weeks_hl) && length(weeks_hl)) {
    rng <- range(weeks_hl)
    if (rng[1] > wlo || rng[2] < whi)            # shade only a strict subset of the season
      p <- p + ggplot2::annotate("rect", xmin = rng[1] - 0.5, xmax = rng[2] + 0.5,
                                 ymin = -Inf, ymax = Inf, fill = "#15314B", alpha = 0.06)
  }
  p +
    ggplot2::geom_hline(yintercept = c(flo, cei), color = ff_colors$median,
                        linetype = "dashed", linewidth = 0.4) +
    # benched weeks read as "didn't count": hollow marker + dotted stem (no alpha,
    # which looked muddy). Whole dumbbell shares data_id + tooltip -> hover anywhere.
    ggiraph::geom_segment_interactive(
      ggplot2::aes(xend = week, y = proj, yend = actual, color = positive,
                   data_id = did, tooltip = tip), linewidth = 1.1) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = proj, data_id = did, tooltip = tip),
      color = ff_colors$median, size = 2) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = actual, color = positive, shape = is_starter,
                   data_id = did, tooltip = tip), size = 3, stroke = 1.2, fill = "white") +
    ggplot2::scale_color_manual(values = ov_un, guide = "none") +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
    ggplot2::scale_x_continuous(
      limits = if (is.null(week_max)) NULL else c(0.5, week_max + 0.5),
      breaks = if (is.null(week_max)) scales::breaks_width(2) else seq(2, week_max, by = 2)) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::labs(
      title = sprintf("%s (%s) · %s", info$player_name, info$pos, info$franchise_name),
      subtitle = paste0("Weekly actual vs <span style='color:#9aa0a8;'>projected</span> ",
                        "(grey dot); <span style='color:#0072B2;'>over</span> / ",
                        "<span style='color:#D55E00;'>under</span> projection in color; ",
                        "<span style='color:#9aa0a8;'>dashed grey = typical range</span>; ",
                        "hollow marker = benched"),
      x = "Week", y = "Fantasy points", caption = cap
    ) +
    theme_ff()
}

mod_player_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  toggle_links <- function(all_id, none_id) shiny::span(
    shiny::actionLink(ns(all_id), "All"), " · ",
    shiny::actionLink(ns(none_id), "None"), class = "small")

  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Player Performance vs Projection"),
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
          shiny::radioButtons(
            ns("games"), "Games counted",
            choices = c("Started only" = "started", "All rostered (incl. bench)" = "all"),
            selected = "started"),
          shiny::helpText(sprintf(
            "Qualifiers: ≥ %d games on the season; plotted values reflect the selected week range.",
            PLAYER_MIN_GAMES)),
          shiny::uiOutput(ns("week_ui"))
        ),
        ggiraph::girafeOutput(ns("scatter"), height = "780px")
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Weekly Game Log",
        bslib::popover(
          shiny::icon("circle-info", title = "What the numbers mean"),
          title = "Reading the game log",
          shiny::tags$ul(
            class = "mb-0 ps-3",
            shiny::tags$li(shiny::HTML("<b>Typical range</b> — the player's 25th–75th percentile of weekly points (the dashed lines): a normal bad week up to a normal good week.")),
            shiny::tags$li(shiny::HTML("<b>M</b> — the player's average (mean) weekly points.")),
            shiny::tags$li(shiny::HTML("<b>SD</b> — standard deviation of weekly points; higher = more boom/bust.")),
            shiny::tags$li(shiny::HTML("<b>CV</b> — coefficient of variation (SD ÷ M): volatility on a common scale, so high- and low-scoring players compare fairly.")),
            shiny::tags$li(shiny::HTML("<b>Color</b> — <span style='color:#0072B2;'>blue</span> beat that week's projection, <span style='color:#D55E00;'>orange</span> fell short.")),
            shiny::tags$li(shiny::HTML("<b>Hollow</b> markers (open dots) are weeks the player was benched.")))
        )
      ),
      ggiraph::girafeOutput(ns("gamelog"), height = "340px"),
      shiny::helpText(
        "Click any point above (or use the search box) to drill into a player's ",
        "week-by-week actual vs projected.")
    )
  )
}

mod_player_performance_server <- function(id, ff, season) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    season_teams <- shiny::reactive(sort(season_franchises(ff, season())$franchise_name))
    include_bench <- shiny::reactive(identical(input$games, "all"))

    output$team_ui <- shiny::renderUI({
      team_chip_checkboxes(ns("teams"), season_franchises(ff, season()))
    })
    output$week_ui <- shiny::renderUI({
      re <- reg_season_end(ff, season())
      shiny::sliderInput(ns("week_range"), "Week range", min = 1, max = re,
                         value = c(1, re), step = 1)
    })

    # quick bulk-toggles for the position + team filters (friendlier than chips)
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

    weeks_sel <- shiny::reactive({
      shiny::req(input$week_range)
      seq(input$week_range[1], input$week_range[2])
    })

    # all eligible players (used for the fixed y-range). Only the POSITION filter
    # narrows the plotted points (which facets show); ALL teams stay visible — the
    # team checkboxes fade (not remove) the unselected teams in the plot.
    pp_base <- shiny::reactive({
      player_pop_range(ff, season(), weeks = weeks_sel(),
                       min_games = PLAYER_MIN_GAMES, include_bench = include_bench())
    })
    pp <- shiny::reactive({
      pp_base() %>% dplyr::filter(pos %in% input$pos)
    })
    # Fixed, symmetric y-range for the facets: the union of BOTH "games counted"
    # modes (all positions/teams) at the current week range, so neither the
    # position/team filters nor the started/all-rostered toggle rescale y.
    y_range <- shiny::reactive({
      w <- weeks_sel()
      v <- c(player_pop_range(ff, season(), weeks = w,
                              min_games = PLAYER_MIN_GAMES, include_bench = FALSE)$mean_pop,
             player_pop_range(ff, season(), weeks = w,
                              min_games = PLAYER_MIN_GAMES, include_bench = TRUE)$mean_pop)
      if (!length(v)) return(NULL)
      m <- max(abs(v)) * 1.05
      c(-m, m)
    })

    # keep the search box choices in sync with what's currently plotted; preserve
    # the current pick if it's still visible (isolate() avoids a reactive loop).
    shiny::observe({
      d <- pp()
      choices <- stats::setNames(
        as.character(d$player_id),
        sprintf("%s (%s, %s)", d$player_name, d$pos, d$franchise_name))
      cur  <- shiny::isolate(input$search)
      keep <- if (!is.null(cur) && cur %in% choices) cur else ""
      shiny::updateSelectizeInput(session, "search", choices = choices, selected = keep)
    })

    # clicking a point selects it -> funnel both click and search through the
    # search box so there's a single source of truth for the drill-down.
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
    # message) instead of re-rendering. Clicking a point or using the search box
    # then updates the highlight (and the game log) without blinking the scatter —
    # a full re-render on every pick was the visible "double refresh".
    shiny::observeEvent(selected_player(), ignoreNULL = FALSE, ignoreInit = TRUE, {
      sp <- selected_player()
      session$sendCustomMessage(paste0(session$ns("scatter"), "_set"),
                                if (is.null(sp)) character(0) else as.character(sp))
    })

    output$scatter <- ggiraph::renderGirafe({
      shiny::validate(shiny::need(length(input$pos) > 0, "Select at least one position."))
      d <- pp()
      shiny::validate(shiny::need(nrow(d) > 0, "No players match these filters."))
      # The pick is applied/kept client-side (observer above), so isolate it here:
      # the scatter must NOT re-render when the pick changes. It re-renders only on
      # data/filter changes, and this re-applies the current pick on those.
      sp <- shiny::isolate(selected_player())
      ff_girafe(plot_player_scatter(d, ylim = y_range(), emphasize_teams = emph_teams(),
                                    highlight_pid = if (is.null(sp)) NULL else as.integer(sp)),
                width_svg = 10, height_svg = 9,
                highlight = FALSE, select = TRUE, selected = sp)
    })

    output$gamelog <- ggiraph::renderGirafe({
      shiny::validate(shiny::need(
        !is.null(selected_player()),
        "Click a point or search a player above to see their week-by-week actual vs projected."))
      gl <- player_gamelog(ff, season(), as.integer(selected_player()),
                           include_bench = include_bench())
      shiny::validate(shiny::need(nrow(gl) > 0, "No games with a projection for this player in this view."))
      ff_girafe(plot_player_gamelog(gl, weeks_hl = weeks_sel(),
                                    week_max = reg_season_end(ff, season())),
                width_svg = 10, height_svg = 3.6, highlight = FALSE)
    })
  })
}
