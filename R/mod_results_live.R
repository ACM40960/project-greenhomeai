# =========================================
# GreenHomeAI — Shiny Results Module
#
# Provides the UI + server logic for displaying results
# from the baseline prediction and per-upgrade ΔCO₂ models.
#
# Features:
#  - Metric cards (Baseline CO₂, Top Upgrade, € Saved/year)
#  - Interactive charts: Waterfall, Bubble impact, Gauge (% saved)
#  - Scenario builder: combine upgrades and view new CO₂ & savings
#  - Upgrade options table ranked by €/kg CO₂
#  - Scenario summary table with reductions and savings
#  - Informational block linking to SEAI grant programs
#
# Exports:
#  - mod_results_live_ui
#  - mod_results_live_server
# =========================================






mod_results_live_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="row gy-3",
        div(class="col-md-4",
            div(class="card metric",
                div(class="label strong","BASELINE CO₂"),
                div(class="value strong", textOutput(ns("co2_baseline_card"))),
                
                div(class="muted", "Estimated annual emissions for your inputs.")
            )
        ),
        div(class="col-md-4",
            div(class="card metric",
                div(class="label strong","TOP UPGRADE"),
                div(class="value strong", textOutput(ns("top_upgrade_card"))),
                div(class="muted", "Highest annual €-savings among available upgrades.")
            )
        ),
        div(class="col-md-4",
            div(class="card metric",
                div(class="label strong","€ SAVED / YEAR"),
                div(class="value strong", textOutput(ns("euro_saved_card"))),
                div(class="muted", "Sum of selected upgrades’ yearly saving.")
            )
        )
    ),
    
    tags$hr(style = "border-top: 3px solid #333; margin: 15px 0;"),
    
    div(class="row gx-3 gy-4 mt-1",
        div(class="col-md-6",
            div(class="plot-card", plotlyOutput(ns("wf"), height="360px")),
            div(class="figure-note strong", textOutput(ns("wf_note")))
        ),
        div(class="col-md-6",
            div(class="plot-card", plotlyOutput(ns("bubble"), height="360px")),
            div(class="figure-note strong", textOutput(ns("bubble_note")))
        )
    ),
    
    tags$hr(style = "border-top: 3px solid #333; margin: 15px 0;"),
    
    div(class="row gx-3 gy-4",
        div(class="col-md-6",
            div(class="plot-card", plotlyOutput(ns("gauge_pct"), height="240px")),
            div(class="chip-row",
                checkboxGroupInput(
                  ns("scenario"), strong("Build a scenario"),
                  choices = c("insulation_wall","insulation_roof","windows"),
                  selected = "insulation_wall", inline = TRUE
                ),
                div(class="pill", span("New CO₂:"),  span(textOutput(ns("new_co2"), inline=TRUE), class="pill-value")),
                div(class="pill", span("Total € / yr:"), span(textOutput(ns("scenario_euro"), inline=TRUE), class="pill-value"))
            ),
            div(class="figure-note strong", textOutput(ns("gauge_note")))
        ),
        div(class="col-md-6",
            div(class="plot-card",
                h5(strong("Upgrade options (lower €/kg is better)")),
                DT::dataTableOutput(ns("tbl"), width = "100%")
            )
        )
    ),
    
    tags$hr(style = "border-top: 3px solid #333; margin: 15px 0;"),
    
    div(class="row gx-3 gy-4",
        div(class="col-md-12",
            div(class="plot-card",
                h5(strong("Scenario summary")),
                DT::dataTableOutput(ns("scenario_tbl"), width = "100%")
            )
        )
    ),
    
    
    div(class="row gx-3 gy-4",
        div(class="col-md-12",
            div(class="plot-card",
                h5(strong("Did you know?")),
                p("Government grants are often available for home energy upgrades such as 
              wall insulation, roof insulation, or window replacements."),
                p("Grant eligibility depends on your county, dwelling type, fuel source, and year built.
              You can check available grants on the official ", 
                  a("SEAI website", href="https://www.seai.ie/grants/", target="_blank"), ".")
            )
        )
    ),
    
    
    
    
    hr()
  )
}


mod_results_live_server <- function(id, features, assets){
  moduleServer(id, function(input, output, session){
    req(assets)
    
    seed <- reactive({ req(features()); pick_seed_row(assets$baseline, features()) })
    Xb   <- reactive({ req(seed()); encode_row_to_p3(seed(), features(), assets$p3_names) })
    baseline_pred <- reactive({ req(Xb()); predict_baseline(assets$booster3, Xb(), assets$b_meta) })
    
    deltas_df <- reactive({
      req(seed(), features())
      ds <- as.list(predict_deltas(assets$boosters4, seed(), features(), assets$p4_names))
      ds <- lapply(ds, function(z) pmax(0, as.numeric(z)))
      upg <- names(ds); delta_co2 <- as.numeric(unlist(ds))
      
      econ <- lapply(delta_co2, econ_layer,
                     fuel = features()$main_fuel, emfs = assets$emfs, tariffs = assets$tariffs)
      delta_kwh  <- sapply(econ, `[[`, "delta_kwh")
      euro_saved <- sapply(econ, `[[`, "euro_saved")
      
      df <- data.frame(
        upgrade    = upg,
        delta_co2  = delta_co2,
        delta_kwh  = delta_kwh,
        euro_saved = euro_saved,
        stringsAsFactors = FALSE
      )
      
      present <- c(
        insulation_wall    = isTRUE(features()$has_wall),
        insulation_roof    = isTRUE(features()$has_roof),
        windows            = isTRUE(features()$has_windows),
        heating_efficiency = FALSE   # not in UI
      )
      df$available <- !(present[match(df$upgrade, names(present))] %||% FALSE)
      df$delta_co2[!df$available]  <- 0
      df$delta_kwh[!df$available]  <- 0
      df$euro_saved[!df$available] <- 0
      
      subset(df, upgrade != "heating_efficiency")
    })
    
    output$co2_baseline_card <- renderText({
      req(baseline_pred())
      format(round(baseline_pred()), big.mark = ",")
    })
    output$top_upgrade_card  <- renderText({
      req(deltas_df())
      df <- deltas_df()
      df <- df[order(-ifelse(is.na(df$euro_saved), 0, df$euro_saved)), ]
      if (nrow(df)) .pretty_upgrade(df$upgrade[1]) else "—"
    })
    output$euro_saved_card   <- renderText({
      req(deltas_df())
      sel <- input$scenario %||% character(0)
      df  <- deltas_df()
      tot <- sum(df$euro_saved[df$upgrade %in% sel], na.rm = TRUE)
      paste0("€", format(round(tot), big.mark = ","))
    })
    
    output$wf <- plotly::renderPlotly({
      req(baseline_pred(), deltas_df())
      waterfall_co2(baseline_pred(), deltas_df(), input$scenario %||% character(0))
    })
    output$wf_note <- renderText({
      "Blue bars = baseline and new total; red segment(s) = CO₂ reduction from selected upgrades."
    })
    
    output$bubble <- plotly::renderPlotly({
      req(deltas_df())
      bubble_impact(deltas_df())
    })
    output$bubble_note <- renderText({
      "Top-right bubbles are most impactful (more € and more CO₂ saved). Bubble size scales with kWh saved."
    })
    
    output$gauge_pct <- plotly::renderPlotly({
      req(baseline_pred(), deltas_df())
      gauge_pct_saved(baseline_pred(), deltas_df(), input$scenario %||% character(0))
    })
    output$gauge_note <- renderText({
      "Overall CO₂ reduction from the selected scenario vs baseline."
    })
    
    output$new_co2 <- renderText({
      req(baseline_pred(), deltas_df())
      sel <- input$scenario %||% character(0)
      df  <- deltas_df()
      new_co2 <- baseline_pred() - sum(df$delta_co2[df$upgrade %in% sel], na.rm = TRUE)
      format(round(max(new_co2, 0)), big.mark = ",")
    })
    output$scenario_euro <- renderText({
      req(deltas_df())
      sel <- input$scenario %||% character(0)
      df  <- deltas_df()
      tot <- sum(df$euro_saved[df$upgrade %in% sel], na.rm = TRUE)
      paste0("€", format(round(tot), big.mark = ","))
    })
    
    output$tbl <- DT::renderDataTable({
      req(deltas_df())
      df <- deltas_df()
      df$upgrade_pretty <- .pretty_upgrade(df$upgrade)
      df$euro_per_kg <- ifelse(df$delta_co2 > 0, df$euro_saved / df$delta_co2, NA_real_)
      df <- subset(df, (!is.na(euro_saved) & euro_saved > 0) | (!is.na(euro_per_kg) & euro_per_kg > 0))
      df <- df[order(df$euro_per_kg, df$upgrade_pretty, na.last = TRUE), ]
      out <- df[, c("upgrade_pretty","delta_co2","delta_kwh","euro_saved","euro_per_kg")]
      names(out) <- c("Upgrade","CO₂ saved (kg/yr)","kWh saved/yr","€ saved/yr","€/kg CO₂")
      out$`CO₂ saved (kg/yr)` <- round(out$`CO₂ saved (kg/yr)`, 1)
      out$`kWh saved/yr`      <- round(out$`kWh saved/yr`, 0)
      out$`€ saved/yr`        <- round(out$`€ saved/yr`, 0)
      out$`€/kg CO₂`          <- round(out$`€/kg CO₂`, 2)
      DT::datatable(out, rownames = FALSE, options = list(pageLength = 5, searching = FALSE))
    })
    
    output$scenario_tbl <- DT::renderDataTable({
      req(baseline_pred(), deltas_df())
      sel <- input$scenario %||% character(0)
      df  <- deltas_df()
      baseline <- baseline_pred()
      saved_kg <- sum(df$delta_co2[df$upgrade %in% sel], na.rm = TRUE)
      new_co2  <- max(baseline - saved_kg, 0)
      pct      <- if (baseline > 0) 100 * saved_kg / baseline else NA_real_
      euro     <- sum(df$euro_saved[df$upgrade %in% sel], na.rm = TRUE)
      
      tbl <- data.frame(
        Metric = c("Baseline CO₂ (kg/yr)", "Selected upgrades", "New CO₂ (kg/yr)",
                   "CO₂ reduction (kg)", "CO₂ reduction (%)", "€ saved / year"),
        Value  = c(round(baseline),
                   paste(.pretty_upgrade(sel), collapse = ", "),
                   round(new_co2),
                   round(saved_kg),
                   if (is.finite(pct)) paste0(round(pct, 1), "%") else "—",
                   paste0("€", format(round(euro), big.mark=","))),
        stringsAsFactors = FALSE
      )
      DT::datatable(tbl, rownames = FALSE, options = list(dom = 't', ordering = FALSE))
    })
    
  })
}
