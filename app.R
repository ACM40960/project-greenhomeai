# ============================================================
# GreenHomeAI — Shiny App Entrypoint (Live Mode)
#
# Purpose:
#   Launch a single‑page Shiny app that loads trained assets,
#   collects user home details, and displays CO₂ & €‑savings results.
#
# What this file does:
#   - Loads UI/server modules and utilities (utils_live, inputs, results)
#   - Loads model assets once (ASSETS) for live predictions
#   - Defines a styled navbar (title, tagline, optional CSS)
#   - Builds layout: sidebar inputs + “Recommendations” results panel
#   - Wires modules together in server and runs the app
#
# How to run:
#   - Source this file in an interactive R session, or
#   - Rscript run; the last lines start the app on 127.0.0.1:4242
# ============================================================




options(shiny.launch.browser = TRUE)

library(shiny); library(bslib); library(htmltools); library(plotly)

source("R/utils_live.R"); source("R/mod_inputs_live.R"); source("R/mod_results_live.R")

ASSETS <- load_live_assets()

style_href <- if (file.exists("www/styles.css")) "styles.css" else
  if (file.exists("www/styles")) "styles" else NULL

app_title <- tags$div(
  style = "width:100%; text-align:center; display:flex; flex-direction:column; align-items:center; gap:4px;",
  tags$div("GreenHomeAI",
           style = "font-weight:800; letter-spacing:.3px; font-size:24px;"),
  tags$div("Smart Insights, Smaller Footprints: AI for Greener Homes",
           style = "font-weight:600; font-size:14px; opacity:.85;")
)

brand_css <- HTML("
  <style>
    .navbar { position: relative; }
    .navbar .navbar-brand {
      width: 100%;
      display: flex !important;
      justify-content: center !important;
      align-items: center;
      gap: 8px;
    }
    /* Hide tab labels */
    .navbar .navbar-nav { display: none !important; }
    .navbar .nav-link.active { border-bottom: 0 !important; }

    /* Right-side tools container */
    #nav-tools {
      position: absolute;
      right: 16px;
      top: 8px;
      display: flex;
      align-items: center;
      gap: 8px;
      z-index: 10;
    }
    #nav-tools .btn-sm {
      padding: 4px 10px;
      border-radius: 999px;
      font-weight: 600;
    }
  </style>
")

ui <- page_navbar(
  title  = app_title,
  theme  = bs_theme(version = 5, primary = "#169873"),
  header = tagList(
    if (!is.null(style_href)) tags$link(rel="stylesheet", href=style_href),
    brand_css,
  ),
  sidebar = sidebar(mod_inputs_live_ui("inputs"), title = "DESCRIBE YOUR HOME", open = TRUE),
  nav_panel("Recommendations", mod_results_live_ui("results")),
)


server <- function(input, output, session) {
  features <- mod_inputs_server("inputs")
  mod_results_live_server("results", features = features, assets = ASSETS)
}

app <- shinyApp(ui, server)
if (interactive()) runApp(app, host = "127.0.0.1", port = 4242, launch.browser = TRUE)
