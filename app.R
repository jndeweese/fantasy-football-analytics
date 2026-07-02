# =============================================================================
# app.R  --  Fantasy Football Analytics Dashboard (Shiny + bslib)
# -----------------------------------------------------------------------------
# Reads only the cleaned data/*.rds snapshot (see data-raw/ for the pipeline).
# Run locally with:  shiny::runApp()
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(forcats)
  library(tibble)
  library(ggplot2)
  library(ggtext)
  library(ggrepel)
  library(ggiraph)
  library(DT)
  library(scales)
  library(thematic)
})

# Make ggplot output follow the active light/dark bslib theme automatically
# (transparent backgrounds so plots sit on their card; text recolors per mode).
thematic_shiny(bg = "transparent", fg = "auto", accent = "auto")

# --- Source helpers + modules ------------------------------------------------
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# --- Load cleaned data once (shared across sessions) -------------------------
# Default to the PUBLIC (anonymized) data for safety: runApp() and the public
# deploy show "Team A–H". Real names load ONLY when explicitly requested via
# FF_DATA_DIR=data (see run_private.R and the private deploy in tools/deploy.R).
data_dir <- Sys.getenv("FF_DATA_DIR", "data-public")
if (!dir.exists(data_dir)) {
  stop("Data dir '", data_dir, "' not found. Build public data with ",
       "data-raw/05_make_public_data.R, or set FF_DATA_DIR=data for real names.")
}
is_private <- basename(data_dir) == "data"
ff <- load_ff_data(data_dir)
all_seasons <- seasons_available(ff)
snapshot_note <- sprintf("Data through %d wk %d", max(all_seasons), current_week(ff, max(all_seasons)))
# Make the real-names view unmistakable locally (never shown on the public app).
if (is_private) snapshot_note <- paste0(snapshot_note, " · PRIVATE (real names)")

# --- Theme: "clean modern" ---------------------------------------------------
ff_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = "#1f2d3d",
  primary = "#0072B2", secondary = "#5b6b7b",
  success = "#009E73", info = "#56B4E9",
  warning = "#E69F00", danger = "#D55E00",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius" = "0.6rem",
  "navbar-bg" = "#15314B"
) |>
  bs_add_rules("
    [data-bs-theme=light] body { background-color: #eef1f4; }
    [data-bs-theme=light] .card { background-color: #ffffff; }
    .card { box-shadow: 0 1px 3px rgba(0,0,0,.06), 0 1px 2px rgba(0,0,0,.04); }
    .navbar { box-shadow: 0 2px 8px rgba(0,0,0,.18); }
    .card-header { font-weight: 600; }
    /* compact season picker in the navbar */
    .ff-navctl { display:flex; align-items:center; gap:.4rem; }
    .ff-navctl label { color: rgba(255,255,255,.85); margin:0; font-size:.85rem; white-space:nowrap; }
    .ff-navctl .shiny-input-container { margin-bottom:0; width:110px; }
    .ff-navctl .form-select { padding-top:.2rem; padding-bottom:.2rem; }
    .ff-footer { padding:.35rem 1rem; font-size:.78rem; text-align:right;
                 color: var(--bs-secondary-color); }
    /* Team 'highlight' selector: re-skin a checkboxGroupInput as colored toggle
       pills. Per-team colors are injected per render (team_chip_checkboxes). */
    .ff-teamchips .shiny-options-group { display:flex; flex-wrap:wrap; gap:6px; margin-top:.25rem; }
    .ff-teamchips .checkbox-inline { margin:0; padding:0; font-weight:400; }
    .ff-teamchips input[type=checkbox] { position:absolute; width:1px; height:1px;
                 opacity:0; margin:0; pointer-events:none; }
    .ff-teamchips input + span { display:inline-flex; align-items:center; gap:6px;
                 font-size:.8rem; line-height:1; padding:5px 11px; border-radius:999px;
                 border:1px solid var(--bs-border-color); color:var(--bs-secondary-color);
                 cursor:pointer; white-space:nowrap;
                 transition:background-color .12s, color .12s, border-color .12s; }
    .ff-teamchips input + span::before { content:''; flex:0 0 auto; width:9px; height:9px;
                 border-radius:50%; background:#bbb; }
    .ff-teamchips input:checked + span::before { display:none; }
    .ff-teamchips input:hover + span { border-color:var(--bs-secondary-color); }
    .ff-teamchips input:focus-visible + span { outline:2px solid var(--bs-primary); outline-offset:1px; }
    /* Heatmap: let clicks/hover fall through the cell number text to the tile
       beneath, so clicking a cell (incl. on its number) selects it. */
    .ff-heatmap text { pointer-events: none; }
    /* Compact KPI value boxes on the Draft tab (smaller value/title/icon than the
       default kpi_box used elsewhere). !important: bslib's own .value-box-value
       rule has equal specificity and loads after this, so it would win otherwise. */
    .ff-kpi-sm .value-box-title { font-size:.95rem !important; margin-bottom:.1rem; }
    .ff-kpi-sm .value-box-value { font-size:1.25rem !important; margin-bottom:.1rem; }
    .ff-kpi-sm .value-box-showcase { font-size:1.4rem !important; }
  ")

# --- UI ----------------------------------------------------------------------
navbar <- page_navbar(
  title = "Fantasy Football Analytics",
  theme = ff_theme,
  # No tab needs to fill the window; multi-card tabs scroll naturally.
  fillable = FALSE,
  footer = div(class = "ff-footer", snapshot_note,
               " · seasons 2023–2025 · ESPN via ffscrapr"),

  nav_panel("Overview", mod_overview_ui("overview")),
  nav_panel("Power Rankings", mod_power_rankings_ui("power")),

  nav_menu(
    "In-Season Performance",
    nav_panel("Player Scoring vs Projection",    mod_player_performance_ui("playerperf")),
    nav_panel("All-Play & Luck",       mod_standings_median_ui("standings")),
    nav_panel("Lineup Efficiency",     mod_lineup_efficiency_ui("lineup")),
    nav_panel("Team Scoring vs Projection", mod_scoring_projection_ui("scoringproj"))
  ),
  nav_panel("Draft Analysis", mod_draft_value_ui("draftvalue")),
  nav_panel("Playoff Projections",   mod_playoffs_ui("playoffs")),

  nav_spacer(),
  nav_item(div(class = "ff-navctl",
               tags$label("Season", `for` = "season"),
               selectInput("season", NULL, choices = all_seasons,
                           selected = max(all_seasons)))),
  nav_item(tags$a("Source", href = "https://github.com/jndeweese", target = "_blank"))
)

# Load the team-chip "click to highlight" script once (see team_chip_js()).
ui <- tagList(tags$head(team_chip_js()), navbar)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  season <- reactive(as.integer(input$season))

  mod_overview_server("overview", ff, season)
  mod_standings_median_server("standings", ff, season)
  mod_power_rankings_server("power", ff, season)
  mod_lineup_efficiency_server("lineup", ff, season)
  mod_scoring_projection_server("scoringproj", ff, season)
  mod_player_performance_server("playerperf", ff, season)
  mod_draft_value_server("draftvalue", ff, season)
  mod_playoffs_server("playoffs", ff, season)
}

shinyApp(ui, server)
