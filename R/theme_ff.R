# =============================================================================
# theme_ff.R  --  Shared visual identity (colors + ggplot theme)
# -----------------------------------------------------------------------------
# One palette + one theme, reused by every tab, so the dashboard reads as a
# single coherent product. Palette is the colorblind-safe Okabe-Ito set.
# =============================================================================

# Okabe-Ito qualitative palette (colorblind-safe). 8 colors for 8 teams.
# NOTE: the 8th color (black) reads great on the light theme but vanishes on a
# dark background -- swap it for a neutral grey if dark mode is reintroduced.
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#000000"  # black
)

# Position colors (used on draft / lineup plots), drawn from the same family.
ff_pos_colors <- c(
  QB  = "#0072B2",
  RB  = "#009E73",
  WR  = "#E69F00",
  TE  = "#CC79A7",
  K   = "#999999",
  DST = "#56B4E9",
  "RB/WR/TE" = "#D55E00"
)

# Position display order + the skill-position default, shared by the tabs with a
# position filter (Player Performance, Draft Value) so their selectors match.
POS_LEVELS  <- c("QB", "RB", "WR", "TE", "K", "DST")
POS_DEFAULT <- c("QB", "RB", "WR", "TE")

# Semantic colors used consistently across tabs. `anno`/`muted`/`rule` are mid
# greys chosen to stay legible on BOTH light and dark card backgrounds.
ff_colors <- list(
  win      = "#0072B2",
  loss     = "#D55E00",
  over     = "#0072B2",  # over-performed / value
  under    = "#D55E00",  # under-performed / reach
  neutral  = "#7f868f",
  median   = "#9aa0a8",
  anno     = "#333333",  # in-plot annotation text (near-black for readability)
  rule     = "#9aa0a8",  # reference lines (hline/vline)
  highlight = "#000000"
)

#' Build a stable team -> color map for a set of franchises.
#'
#' Color is a fixed function of `franchise_id` (1..8 in this league), so a team
#' keeps the same color no matter which *subset* of teams is passed in — the
#' mapping is identical across every tab (even when a tab filters to a few teams)
#' and across seasons (same owner keeps their color even after a rename). Returned
#' keyed by `franchise_name` for `scale_color_manual(values = ...)`.
#'
#' NOTE: keying off `franchise_id` directly (not its rank within the input) is
#' what makes this subset-invariant — ranking the input remapped colors whenever
#' only some teams were shown.
#'
#' @param franchises A data frame with `franchise_id` and `franchise_name`.
#' @return Named character vector: names = franchise_name, values = hex color.
ff_team_palette <- function(franchises) {
  idx <- ((franchises$franchise_id - 1L) %% length(okabe_ito)) + 1L
  stats::setNames(okabe_ito[idx], franchises$franchise_name)
}

#' Team "highlight" selector rendered as colored toggle chips.
#'
#' A plain `checkboxGroupInput` (so all the usual server wiring — `input$teams`,
#' All/None, the fade logic — works unchanged) re-skinned via CSS into pills: each
#' chip is tinted with its team color (checked = filled in the team's color,
#' unchecked = outline + a color dot). The structural CSS lives once in app.R
#' (`.ff-teamchips`); the per-team color rules are emitted here because team names
#' (the checkbox values the CSS keys on) can change by season, so they're generated
#' per render. Returns a tagList of the scoped <style> + the wrapped input.
#'
#' @param input_id Namespaced input id (e.g. `ns("teams")`).
#' @param franchises Data frame with `franchise_id` + `franchise_name` for the season.
team_chip_checkboxes <- function(input_id, franchises) {
  fr  <- dplyr::distinct(franchises, franchise_id, franchise_name)
  fr  <- fr[order(fr$franchise_name), , drop = FALSE]
  pal <- ff_team_palette(fr)              # name -> hex (stable per franchise_id)
  teams <- fr$franchise_name
  # readable text on a filled chip: dark ink on light hues, white on dark ones.
  ink <- function(hex) {
    v <- grDevices::col2rgb(hex)[, 1]
    if (sum(c(0.299, 0.587, 0.114) * v) > 150) "#1a1a1a" else "#ffffff"
  }
  rules <- vapply(teams, function(t) {
    col  <- pal[[t]]
    base <- sprintf('.ff-teamchips input[value="%s"]', t)
    paste0(
      base, " + span::before{background:", col, ";}\n",
      base, ":checked + span{background:", col, ";color:", ink(col),
      ";border-color:", col, ";}"
    )
  }, character(1))
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste(rules, collapse = "\n"))),
    shiny::div(
      class = "ff-teamchips",
      shiny::checkboxGroupInput(input_id, NULL, choices = teams,
                                selected = teams, inline = TRUE)
    )
  )
}

#' One <script> (include once, in the page head) giving the team-chip selectors
#' "click to highlight" semantics: from the all-selected (neutral) state, clicking
#' a chip isolates that team; from any subset the native checkbox toggle (add an
#' unselected team, remove a selected one) already does the right thing; and
#' unchecking the *last* remaining team (clicking the sole highlighted chip)
#' restores all — same feel as clicking a highlighted point to clear it. (The
#' explicit "None" link is still the way to reach the all-faded empty state.)
#'
#' Done on the client so there is no server round-trip (which made the clicked chip
#' blink off then on): a **capture-phase** `change` listener corrects the DOM
#' *before* Shiny's own (bubble-phase) handler reads it, so Shiny sends the right
#' value once and nothing flickers. The only case the native checkbox gets wrong is
#' the first click out of all-selected — it would merely remove the clicked team,
#' but we want to keep *only* it; every other transition is the checkbox's own
#' toggle. We detect "all were checked before this change" by reconstructing the
#' pre-change state (the changed input was in the opposite state), so no extra
#' state is tracked. Scoped to `.ff-teamchips`, so other checkbox groups are
#' untouched.
team_chip_js <- function() {
  shiny::tags$script(shiny::HTML(
    "document.addEventListener('change', function(e){
       var input = e.target;
       if (!input || input.type !== 'checkbox' || !input.closest('.ff-teamchips')) return;
       var group = input.closest('.shiny-options-group');
       if (!group) return;
       var inputs = Array.prototype.slice.call(group.querySelectorAll('input[type=\"checkbox\"]'));
       var prevAll = inputs.every(function(i){ return (i === input) ? !i.checked : i.checked; });
       if (prevAll) {
         inputs.forEach(function(i){ i.checked = (i === input); });   // all selected -> isolate the clicked team
       } else if (inputs.every(function(i){ return !i.checked; })) {
         inputs.forEach(function(i){ i.checked = true; });            // unchecked the last -> restore all
       }
     }, true);"
  ))
}

#' Shared ggplot2 theme: clean, minimal gridlines, readable type.
#'
#' Text colors are intentionally left to inherit so that `thematic` can recolor
#' them to match the active light/dark bslib theme at render time. Backgrounds are
#' transparent so plots sit directly on their card in either mode.
theme_ff <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title          = ggtext::element_markdown(face = "bold", size = ggplot2::rel(1.15)),
      plot.subtitle       = ggtext::element_markdown(size = ggplot2::rel(0.95)),
      plot.title.position  = "plot",
      plot.caption         = ggplot2::element_text(size = ggplot2::rel(0.8), 
                                                   hjust = 0, color = "gray40"),
      plot.background      = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background     = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major.x   = ggplot2::element_blank(),
      axis.title.x         = ggplot2::element_text(margin = margin(t=10)),
      axis.title.y         = ggplot2::element_text(margin = margin(r=10)),
      axis.text            = ggplot2::element_text(size = ggplot2::rel(0.9)),
      strip.text           = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9)),
      strip.background     = ggplot2::element_rect(fill = "#8888881F", color = NA),
      legend.position      = "none"  # default to direct labeling; override per-plot
    )
}

#' Render a ggplot (with ggiraph interactive geoms) as an interactive widget:
#' hovering a series (data_id) keeps it bold and fades the others. Shared by the
#' Power Ranking bump and the Scoring Trend so the hover behavior is consistent.
#'
#' @param selected Optional data_id(s) to pre-select (e.g. the team clicked in a
#'   linked table). The selected series stays bold while the rest fade; passing
#'   NULL leaves everything at full strength.
#' @param width_svg,height_svg SVG canvas size in inches; their ratio sets the
#'   plot's aspect ratio. girafe scales the SVG to its container *preserving that
#'   ratio* (unlike renderPlot, it does not re-render to fit), so a container with
#'   a different aspect letterboxes. We keep a near-square-ish ~1.9:1 and accept
#'   side whitespace rather than flatten the lines with a very wide ratio.
#' @param highlight Interaction style. `TRUE` (the spaghetti-line charts): hovering
#'   or selecting a series bolds it and fades the rest, and `selected` pre-selects
#'   one (driven from a linked table). `FALSE` (everything else): tooltip-only —
#'   hovering an element shows its values and gives it a subtle outline, with no
#'   fading or click-to-select. Either way hover/tooltip behavior is consistent.
#' @param select When `highlight = FALSE`, set `TRUE` to also enable single-element
#'   selection (e.g. the Player Performance scatter's name search): the `selected`
#'   element is emphasized and the rest fade, while hover stays tooltip-only.
#' @param select_fade When selecting, whether to also fade the non-selected
#'   elements (`TRUE`, the scatter search) or merely outline the selected one
#'   (`FALSE`, the Team Draft ROI bars — clicking a bar drives the board without
#'   dimming the chart).
ff_girafe <- function(ggobj, width_svg = 10, height_svg = 5.2, selected = NULL,
                      highlight = TRUE, select = FALSE, select_fade = TRUE) {
  frame <- list(
    ggiraph::opts_tooltip(css = "background:#15314B;color:#fff;padding:3px 8px;border-radius:4px;font-size:12px;"),
    ggiraph::opts_sizing(rescale = TRUE),
    ggiraph::opts_toolbar(saveaspng = FALSE)
  )
  # Emphasis outlines apply to non-text geoms (points, lines, tiles) only; text
  # (interactive ggrepel labels + line end-labels) gets opacity-only, since a stroke
  # on glyphs looks bad. `girafe_css(text = ...)` strips the stroke for text while
  # keeping it for everything else.
  inter <- if (highlight) list(
    ggiraph::opts_hover(css = ggiraph::girafe_css(
      css = "stroke-width:3.5px;opacity:1;", text = "stroke:none;opacity:1;")),
    ggiraph::opts_hover_inv(css = "opacity:0.12;"),
    # Click-to-select (driven from a linked table): emphasize the picked
    # series, fade the rest -- same visual language as hover.
    ggiraph::opts_selection(type = "single", only_shiny = FALSE, selected = selected,
      css = ggiraph::girafe_css(css = "stroke-width:3.5px;opacity:1;",
                                text = "stroke:none;opacity:1;")),
    ggiraph::opts_selection_inv(css = "opacity:0.12;")
  ) else c(
    # tooltip-only: outline the hovered non-text element (text excluded), no fade
    list(ggiraph::opts_hover(css = ggiraph::girafe_css(
      css = "stroke:#15314B;stroke-width:1px;", text = "stroke:none;"))),
    # optional search/click selection: outline the pick (2px on points/tiles, no
    # stroke on text) and (unless select_fade = FALSE) fade the rest.
    if (select) list(ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                       selected = selected, css = ggiraph::girafe_css(
                         css = "opacity:1;stroke:#15314B;stroke-width:2px;",
                         text = "opacity:1;stroke:none;"))),
    if (select && select_fade) list(ggiraph::opts_selection_inv(css = "opacity:0.15;"))
  )
  ggiraph::girafe(
    ggobj = ggobj, width_svg = width_svg, height_svg = height_svg,
    options = c(frame, inter)
  )
}

#' Consistent KPI value box: a short value in the big slot, a muted caption
#' beneath, and a colored Font-Awesome icon. Used across Overview/Luck/Lineup so
#' every KPI card looks identical (same fonts, same layout).
#' @param value Short value (number or e.g. "10-4").
#' @param caption One-line descriptor (team name, units, ...).
#' @param icon_name Font Awesome icon name (bundled with Shiny).
#' @param color Accent color for the icon.
#' @param class Optional extra class(es) on the value box (e.g. "ff-kpi-sm" for the
#'   Draft tab's more compact variant).
kpi_box <- function(title, value, caption, icon_name, color = "#0072B2", class = NULL) {
  bslib::value_box(
    title = title,
    value = value,
    showcase = shiny::span(shiny::icon(icon_name), style = paste0("color:", color, ";")),
    showcase_layout = "left center",
    class = class,
    shiny::span(caption, class = "text-muted small")
  )
}

#' Direct end-of-line labels (legend replacement) with a white halo so even
#' light Okabe-Ito hues stay legible. Expects the last point per group.
#' @param data Last-row-per-group data frame.
#' @param label_col,color_col Bare column names for the label text and color.
#' Interactive variant: pass `data_id`/`tooltip` in the mapping so hovering a
#' label highlights its line (labels are a far bigger hover target than the line).
geom_ff_endlabel <- function(data, mapping = NULL, ...) {
  ggiraph::geom_text_repel_interactive(
    data = data,
    mapping = mapping,
    direction = "y", hjust = 0, nudge_x = 0.3,
    segment.color = NA, bg.color = "white", bg.r = 0.12,
    size = 3.4, fontface = "bold", min.segment.length = Inf, ...
  )
}
