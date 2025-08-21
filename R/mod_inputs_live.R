# =========================================
# GreenHomeAI — Shiny Input Module (Live)
#
# Provides a reusable UI + server module for collecting
# user/home inputs used by the CO₂ predictor.
#
# What it does:
#  - UI with county, dwelling type, year built, floor area, fuel, and retrofit flags
#  - Populates select choices from a provided baseline (fallback defaults if missing)
#  - On “Calculate”, emits a structured features list via eventReactive()
#  - Defers all computation until the button is clicked (ignoreInit=TRUE)
#  - Keeps a stable schema (e.g., has_heating fixed FALSE for downstream logic)
#
# Exports:
#  - mod_inputs_live_ui / mod_inputs_live_server
#  - mod_inputs_ui / mod_inputs_server (wrappers with baseline auto-discovery)
# =========================================




`%||%` <- function(a, b) if (!is.null(a)) a else b

mod_inputs_live_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="input-card",
        selectInput(ns("county"), "County", choices = NULL),
        selectInput(ns("dwelling_type"), "Dwelling type", choices = NULL),
        numericInput(ns("year_built"), "Year built", value = 1980, min = 1800, max = as.integer(format(Sys.Date(), "%Y")), step = 1),
        numericInput(ns("floor_area_m2"), "Floor area (m²)", value = 120, min = 20, step = 1),
       
        selectInput(ns("main_fuel"), "Fuel", choices = NULL),
        tags$hr(),
        checkboxInput(ns("has_wall"),    "Wall insulation present", value = FALSE),
        checkboxInput(ns("has_roof"),    "Roof insulation present", value = FALSE),
        checkboxInput(ns("has_windows"), "Upgraded windows",        value = FALSE),
       
        actionButton(ns("go"), "Calculate", class = "btn-primary btn-lg w-100")
    )
  )
}

mod_inputs_live_server <- function(id, baseline){
  moduleServer(id, function(input, output, session){
    
    uniq_chr <- function(x) sort(unique(stats::na.omit(as.character(x))))
    county_choices <- {
      if (!is.null(baseline) && "CountyName" %in% names(baseline)) uniq_chr(baseline$CountyName)
      else if (!is.null(baseline) && "county" %in% names(baseline)) uniq_chr(baseline$county)
      else c("Dublin","Cork","Galway","Limerick","Waterford")
    }
    dwelling_choices <- {
      if (!is.null(baseline) && "DwellingTypeDescr" %in% names(baseline)) uniq_chr(baseline$DwellingTypeDescr)
      else c("Detached house","Semi-detached house","Terraced house","Apartment")
    }
  
    fuel_choices <- c("Electricity","Heating Oil","Mains Gas")
    
    updateSelectInput(session, "county",        choices = county_choices,   selected = county_choices[1])
    updateSelectInput(session, "dwelling_type", choices = dwelling_choices, selected = dwelling_choices[1])
    updateSelectInput(session, "main_fuel",     choices = fuel_choices,     selected = fuel_choices[1])
    
    features_evt <- eventReactive(
      input$go,
      {
        list(
          county         = input$county,
          dwelling_type  = input$dwelling_type,
          main_fuel      = input$main_fuel,
          floor_area_m2  = as.numeric(input$floor_area_m2),
          year_built     = as.numeric(input$year_built),
          no_storeys     = NA_real_,
          has_wall       = isTRUE(input$has_wall),
          has_roof       = isTRUE(input$has_roof),
          has_windows    = isTRUE(input$has_windows),
        
          has_heating    = FALSE
        )
      },
      ignoreInit = TRUE   
    )

    features_evt
  })
}

mod_inputs_ui <- function(id) { mod_inputs_live_ui(id) }

mod_inputs_server <- function(id, baseline = NULL){
  if (is.null(baseline)) {
    baseline <- tryCatch(get("ASSETS", envir = .GlobalEnv)$baseline, error = function(e) NULL)
  }
  mod_inputs_live_server(id, baseline = baseline)
}
