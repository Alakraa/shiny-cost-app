
library(shiny)
library(readr)
library(dplyr)

drug_data <- tryCatch(
  read_csv("products_23_master_all_structured_updated.csv", show_col_types = FALSE),
  error = function(e) data.frame()
)

safe_col <- function(d, col, default = NA) {
  if (is.data.frame(d) && col %in% names(d) && length(d[[col]]) >= 1) {
    val <- d[[col]][[1]]
    if (is.null(val) || length(val) == 0) default else val
  } else {
    default
  }
}

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  as.character(x)
}

calc_annual <- function(d, input) {
  
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    return(list(
      annual_dose_mg = NA_real_,
      annual_packs = NA_real_,
      annual_cost = NA_real_,
      formula = "Keine Daten verfügbar"
    ))
  }
  
  model <- safe_chr(safe_col(d, "Calc_Model", "manual"))
  berechnung <- safe_chr(safe_col(d, "Berechnungstyp", "mg"))
  
  if (isTRUE(input$manual_mode)) {
    if (berechnung == "packung") {
      annual_packs <- safe_num(input$manual_annual_packs)
      pack_size_mg <- safe_num(input$pack_size_mg)
      price_per_pack <- safe_num(input$price_per_pack)
      
      annual_dose_mg <- if (!is.na(annual_packs) && !is.na(pack_size_mg)) annual_packs * pack_size_mg else NA_real_
      annual_cost <- if (!is.na(annual_packs) && !is.na(price_per_pack)) annual_packs * price_per_pack else NA_real_
      
      return(list(
        annual_dose_mg = annual_dose_mg,
        annual_packs = annual_packs,
        annual_cost = annual_cost,
        formula = "Manuelle Eingabe: Jahrespackungen × Preis pro Packung"
      ))
    } else {
      annual_dose_mg <- safe_num(input$manual_annual_dose_mg)
      price_per_mg <- safe_num(input$price_per_mg)
      annual_cost <- if (!is.na(annual_dose_mg) && !is.na(price_per_mg)) annual_dose_mg * price_per_mg else NA_real_
      
      return(list(
        annual_dose_mg = annual_dose_mg,
        annual_packs = NA_real_,
        annual_cost = annual_cost,
        formula = "Manuelle Eingabe: Jahresdosis (mg) × Preis pro mg"
      ))
    }
  }
  
  months <- ifelse(is.null(input$months), 12, input$months)
  duration_days <- safe_num(months) * 30.4375
  duration_weeks <- duration_days / 7
  weight <- safe_num(input$weight_kg)
  
  dose_mg <- safe_num(safe_col(d, "Dosis_mg"))
  dose_mg_per_kg <- safe_num(safe_col(d, "Dosis_mg_pro_kg"))
  interval_days <- safe_num(safe_col(d, "Intervall_Tage"))
  threshold_kg <- safe_num(safe_col(d, "Schwelle_kg"))
  dose_under_threshold <- safe_num(safe_col(d, "Dosis_unter_Schwelle_mg"))
  dose_ab_threshold <- safe_num(safe_col(d, "Dosis_ab_Schwelle_mg"))
  maintenance_dose <- safe_num(safe_col(d, "Erhaltungsdosis_mg"))
  
  annual_dose_mg <- NA_real_
  annual_packs <- NA_real_
  annual_cost <- NA_real_
  formula <- ""
  
  if (model == "fixed_interval_mg") {
    if (!is.na(dose_mg) && !is.na(interval_days) && interval_days > 0) {
      annual_dose_mg <- dose_mg * (duration_days / interval_days)
      formula <- paste0("Jahresdosis = Dosis × (Behandlungsdauer_Tage / ", interval_days, ")")
    }
    
  } else if (model == "daily_fixed_mg") {
    if (!is.na(dose_mg)) {
      annual_dose_mg <- dose_mg * duration_days
      formula <- "Jahresdosis = Tagesdosis × Behandlungsdauer_Tage"
    }
    
  } else if (model == "weight_interval_mgkg") {
    if (!is.na(weight) && !is.na(dose_mg_per_kg) && !is.na(interval_days) && interval_days > 0) {
      dose <- weight * dose_mg_per_kg
      
      if (!is.na(threshold_kg) && !is.na(dose_ab_threshold) && weight >= threshold_kg) {
        if (!is.na(dose_under_threshold)) {
          dose <- ifelse(weight <= threshold_kg, dose_under_threshold, dose_ab_threshold)
        } else {
          dose <- min(dose, dose_ab_threshold)
        }
      }
      
      annual_dose_mg <- dose * (duration_days / interval_days)
      formula <- paste0(
        "Jahresdosis = Gewicht × ", dose_mg_per_kg,
        " mg/kg × (Behandlungsdauer_Tage / ", interval_days, ")"
      )
    }
    
  } else if (model == "threshold_fixed_interval_mg") {
    if (!is.na(weight) && !is.na(interval_days) && interval_days > 0 &&
        !is.na(threshold_kg) && !is.na(dose_under_threshold) && !is.na(dose_ab_threshold)) {
      dose <- ifelse(weight <= threshold_kg, dose_under_threshold, dose_ab_threshold)
      annual_dose_mg <- dose * (duration_days / interval_days)
      formula <- paste0("Jahresdosis = Dosis nach Gewichtsschwelle × (Behandlungsdauer_Tage / ", interval_days, ")")
    }
    
  } else if (model == "one_time_weight_mgkg") {
    if (!is.na(weight) && !is.na(dose_mg_per_kg)) {
      annual_dose_mg <- weight * dose_mg_per_kg
      formula <- paste0("Jahresdosis = Gewicht × ", dose_mg_per_kg, " mg/kg (Einmalgabe)")
    }
    
  } else if (model == "weight_cycle_ugkg") {
    if (!is.na(weight) && !is.na(dose_mg_per_kg) && !is.na(interval_days) && interval_days > 0) {
      annual_dose_mg <- weight * dose_mg_per_kg * 5 * (duration_days / interval_days)
      formula <- paste0(
        "Jahresdosis = Gewicht × ", dose_mg_per_kg,
        " mg/kg × 5 Gaben/Zyklus × (Behandlungsdauer_Tage / ", interval_days, ")"
      )
    }
    
  } else if (model == "step_fixed_microgram") {
    step1 <- safe_num(safe_col(d, "Schritt_1"))
    step2 <- safe_num(safe_col(d, "Schritt_2"))
    step3 <- safe_num(safe_col(d, "Schritt_3"))
    
    weekly_steps <- c(step1, step2, step3)
    weekly_steps <- weekly_steps[!is.na(weekly_steps)]
    
    if (length(weekly_steps) > 0 && !is.na(maintenance_dose)) {
      annual_dose_mg <- sum(weekly_steps) + max(duration_weeks - 3, 0) * maintenance_dose
      formula <- "Jahresdosis = Step-up-Dosen + Erhaltungsdosis × verbleibende Wochen"
    }
    
  } else if (model == "lunsumio_cycles") {
    total_cycles <- safe_num(ifelse(is.null(input$total_cycles), 8, input$total_cycles))
    
    if (!is.na(total_cycles)) {
      annual_dose_mg <- 63 + 60 + max(total_cycles - 2, 0) * 30
      formula <- "Jahresdosis = 63 mg + 60 mg + 30 mg × (Zyklen ab 3)"
    }
    
  } else if (model == "phased_weight_interval") {
    total_cycles <- safe_num(ifelse(is.null(input$total_cycles), 12, input$total_cycles))
    
    if (!is.na(weight) && !is.na(total_cycles)) {
      annual_dose_mg <- weight * 0.15 * min(total_cycles, 2) + weight * 0.075 * max(total_cycles - 2, 0)
      formula <- "Jahresdosis = Gewicht × 0,15 mg/kg × 2 Zyklen + Gewicht × 0,075 mg/kg × restliche Zyklen"
    }
    
  } else if (model == "talvey_model") {
    step1 <- safe_num(safe_col(d, "Talvey_StepUp1_mgkg"))
    step2 <- safe_num(safe_col(d, "Talvey_StepUp2_mgkg"))
    weekly <- safe_num(safe_col(d, "Talvey_Maintenance_Weekly_mgkg"))
    q2w <- safe_num(safe_col(d, "Talvey_Maintenance_Q2W_mgkg"))
    regimen <- if (!is.null(input$talvey_regimen) && length(input$talvey_regimen) == 1) input$talvey_regimen else "weekly"
    
    if (!is.na(weight)) {
      step_dose <- weight * (step1 + step2)
      
      if (identical(regimen, "weekly")) {
        maint_admin <- max(duration_weeks - 2, 0)
        annual_dose_mg <- step_dose + weight * weekly * maint_admin
        formula <- "Talvey: Gewicht × (0,01 + 0,06) mg/kg + Gewicht × 0,4 mg/kg × (Wochen - 2)"
      } else if (identical(regimen, "q2w")) {
        maint_admin <- max(duration_days / 14 - 1, 0)
        annual_dose_mg <- step_dose + weight * q2w * maint_admin
        formula <- "Talvey: Gewicht × (0,01 + 0,06) mg/kg + Gewicht × 0,8 mg/kg × (14-Tage-Gaben nach Step-up)"
      }
    }
    
  } else if (model == "tecvayli_model") {
    step1 <- safe_num(safe_col(d, "TECVAYLI_Step1_mgkg"))
    step2 <- safe_num(safe_col(d, "TECVAYLI_Step2_mgkg"))
    weekly <- safe_num(safe_col(d, "TECVAYLI_Maint_Weekly_mgkg"))
    q2w <- safe_num(safe_col(d, "TECVAYLI_Maint_Q2W_mgkg"))
    regimen <- if (!is.null(input$tecvayli_regimen) && length(input$tecvayli_regimen) == 1) input$tecvayli_regimen else "weekly"
    
    if (!is.na(weight)) {
      step_dose <- weight * (step1 + step2)
      
      if (identical(regimen, "weekly")) {
        maint_admin <- max(duration_weeks - 2, 0)
        annual_dose_mg <- step_dose + weight * weekly * maint_admin
        formula <- "Tecvayli: Gewicht × (0,06 + 0,3) mg/kg + Gewicht × 1,5 mg/kg × (Wochen - 2)"
      } else if (identical(regimen, "q2w")) {
        maint_admin <- max(duration_days / 14 - 1, 0)
        annual_dose_mg <- step_dose + weight * q2w * maint_admin
        formula <- "Tecvayli: Gewicht × (0,06 + 0,3) mg/kg + Gewicht × 1,5 mg/kg × (14-Tage-Gaben nach Step-up)"
      }
    }
    
  } else if (model == "elrexfio_model") {
    step1 <- safe_num(safe_col(d, "ELREXFIO_Step1_mg"))
    step2 <- safe_num(safe_col(d, "ELREXFIO_Step2_mg"))
    weekly <- safe_num(safe_col(d, "ELREXFIO_Maint_Weekly_mg"))
    q2w <- safe_num(safe_col(d, "ELREXFIO_Maint_Q2W_mg"))
    regimen <- if (!is.null(input$elrexfio_regimen) && length(input$elrexfio_regimen) == 1) input$elrexfio_regimen else "default"
    
    step_dose <- step1 + step2
    
    if (identical(regimen, "default")) {
      first_phase_weeks <- min(duration_weeks, 24)
      weekly_admin <- max(first_phase_weeks - 1, 0)
      second_phase_days <- max(duration_days - 24 * 7, 0)
      q2w_admin <- second_phase_days / 14
      annual_dose_mg <- step_dose + weekly * weekly_admin + q2w * q2w_admin
      formula <- "Elrexfio: 12 mg + 32 mg + 76 mg wöchentlich bis Woche 24, danach 76 mg alle 2 Wochen"
    } else if (identical(regimen, "weekly_only")) {
      annual_dose_mg <- step_dose + weekly * max(duration_weeks - 1, 0)
      formula <- "Elrexfio: 12 mg + 32 mg + 76 mg wöchentlich"
    }
    
  } else if (model == "lynozyfic_model") {
    s1 <- safe_num(safe_col(d, "LYNOZYFIC_Step1_mg"))
    s2 <- safe_num(safe_col(d, "LYNOZYFIC_Step2_mg"))
    s3 <- safe_num(safe_col(d, "LYNOZYFIC_Step3_mg"))
    weekly <- safe_num(safe_col(d, "LYNOZYFIC_Maint_Weekly_mg"))
    q2w <- safe_num(safe_col(d, "LYNOZYFIC_Maint_Q2W_mg"))
    regimen <- if (!is.null(input$lynozyfic_regimen) && length(input$lynozyfic_regimen) == 1) input$lynozyfic_regimen else "default"
    
    step_dose <- s1 + s2 + s3
    
    if (identical(regimen, "default")) {
      weekly_phase_weeks <- max(min(duration_weeks, 13) - 3, 0)
      second_phase_days <- max(duration_days - 13 * 7, 0)
      q2w_admin <- second_phase_days / 14
      annual_dose_mg <- step_dose + weekly * weekly_phase_weeks + q2w * q2w_admin
      formula <- "Lynozyfic: 5 + 25 + 200 mg; dann 200 mg wöchentlich bis Woche 13, danach 200 mg alle 2 Wochen"
    } else if (identical(regimen, "weekly_only")) {
      annual_dose_mg <- step_dose + weekly * max(duration_weeks - 3, 0)
      formula <- "Lynozyfic: 5 + 25 + 200 mg; danach 200 mg wöchentlich"
    }
    
  } else if (model == "ordspono_model") {
    indication <- if (!is.null(input$ordspono_indikation) && length(input$ordspono_indikation) == 1) input$ordspono_indikation else "FL"
    
    if (identical(indication, "FL")) {
      s1 <- safe_num(safe_col(d, "Ordspono_FL_Step1_mg"))
      s2 <- safe_num(safe_col(d, "Ordspono_FL_Step2_mg"))
      s3 <- safe_num(safe_col(d, "Ordspono_FL_Step3_mg"))
      maint <- safe_num(safe_col(d, "Ordspono_FL_Maint_mg"))
      maint_int <- safe_num(safe_col(d, "Ordspono_FL_Maint_Interval_Days"))
    } else {
      s1 <- safe_num(safe_col(d, "Ordspono_DLBCL_Step1_mg"))
      s2 <- safe_num(safe_col(d, "Ordspono_DLBCL_Step2_mg"))
      s3 <- safe_num(safe_col(d, "Ordspono_DLBCL_Step3_mg"))
      maint <- safe_num(safe_col(d, "Ordspono_DLBCL_Maint_mg"))
      maint_int <- safe_num(safe_col(d, "Ordspono_DLBCL_Maint_Interval_Days"))
    }
    
    step_dose <- s1 + s2 + s3
    
    if (!is.na(maint_int) && maint_int > 0) {
      maint_admin <- max(duration_days / maint_int - 3, 0)
      annual_dose_mg <- step_dose + maint * maint_admin
      formula <- paste0("Ordspono ", indication, ": Step-up + ", maint, " mg Erhaltung alle ", maint_int, " Tage")
    }
    
  } else if (model == "rybrevant_model") {
    if (!is.na(weight)) {
      if (weight < 80) {
        init_dose <- safe_num(safe_col(d, "Rybrevant_Under80_Init_mg"))
        maint_dose <- safe_num(safe_col(d, "Rybrevant_Under80_Maint_mg"))
      } else {
        init_dose <- safe_num(safe_col(d, "Rybrevant_Over80_Init_mg"))
        maint_dose <- safe_num(safe_col(d, "Rybrevant_Over80_Maint_mg"))
      }
      
      weekly_weeks <- safe_num(safe_col(d, "Rybrevant_Weekly_Weeks"))
      maint_int <- safe_num(safe_col(d, "Rybrevant_Maint_Interval_Days"))
      
      if (!is.na(weekly_weeks) && !is.na(maint_int) && maint_int > 0) {
        init_total <- init_dose * weekly_weeks
        remaining_days <- max(duration_days - weekly_weeks * 7, 0)
        maint_admin <- remaining_days / maint_int
        annual_dose_mg <- init_total + maint_dose * maint_admin
        formula <- "Rybrevant: 4 wöchentliche Anfangsdosen nach Gewicht, danach Erhaltungsdosis alle 3 Wochen"
      }
    }
    
  } else {
    formula <- "Für dieses komplexe Schema bitte 'Manuelle Jahresdosis / Jahrespackungen eingeben' verwenden."
  }
  
  if (berechnung == "packung") {
    pack_size_mg <- safe_num(input$pack_size_mg)
    price_per_pack <- safe_num(input$price_per_pack)
    
    if (!is.na(annual_dose_mg) && !is.na(pack_size_mg) && pack_size_mg > 0) {
      annual_packs <- ceiling(annual_dose_mg / pack_size_mg)
    }
    
    if (!is.na(annual_packs) && !is.na(price_per_pack)) {
      annual_cost <- annual_packs * price_per_pack
    }
    
    formula <- paste(formula, "| Jahrespackungen = Aufrunden(Jahresdosis / Packungsgröße)")
  } else {
    price_per_mg <- safe_num(input$price_per_mg)
    
    if (!is.na(annual_dose_mg) && !is.na(price_per_mg)) {
      annual_cost <- annual_dose_mg * price_per_mg
    }
  }
  
  list(
    annual_dose_mg = annual_dose_mg,
    annual_packs = annual_packs,
    annual_cost = annual_cost,
    formula = formula
  )
}

ui <- fluidPage(
  titlePanel("Jahrestherapiekosten – INA 23 Produkte"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "produkt",
        "Produkt auswählen",
        choices = if (nrow(drug_data) > 0) sort(unique(drug_data$Produkt)) else character(0),
        selected = if (nrow(drug_data) > 0) sort(unique(drug_data$Produkt))[1] else NULL
      ),
      uiOutput("dynamic_inputs"),
      hr(),
      checkboxInput("manual_mode", "Manuelle Jahresdosis / Jahrespackungen eingeben", FALSE)
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.produkt != null",
        
        h3("Arzneimittelinformation"),
        tableOutput("info_tbl"),
        
        h3("Abschnitt 4.2 – Kurzfassung"),
        verbatimTextOutput("section42"),
        
        h3("Berechnungslogik"),
        verbatimTextOutput("formula_txt"),
        
        h3("Ergebnis"),
        tableOutput("result_tbl")
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected <- reactive({
    req(nrow(drug_data) > 0)
    req(input$produkt)
    
    d <- drug_data %>% filter(Produkt == input$produkt)
    
    if (nrow(d) == 0) return(NULL)
    
    d %>% slice(1)
  })
  
  output$dynamic_inputs <- renderUI({
    d <- selected()
    
    if (is.null(d)) return(tagList())
    
    model <- safe_chr(safe_col(d, "Calc_Model", "manual"))
    berechnung <- safe_chr(safe_col(d, "Berechnungstyp", "mg"))
    items <- list()
    
    if (berechnung == "mg") {
      items <- append(items, list(
        numericInput("price_per_mg", "Preis pro mg (€)", 1, min = 0, step = 0.01)
      ))
    } else {
      default_pack <- safe_num(safe_col(d, "Dosis_mg", 300))
      if (is.na(default_pack)) default_pack <- 300
      
      items <- append(items, list(
        numericInput("pack_size_mg", "Packungsgröße (mg)", default_pack, min = 0, step = 1),
        numericInput("price_per_pack", "Preis pro Packung (€)", 100, min = 0, step = 0.01)
      ))
    }
    
    if (model %in% c(
      "weight_interval_mgkg","one_time_weight_mgkg","weight_cycle_ugkg",
      "threshold_fixed_interval_mg","phased_weight_interval",
      "talvey_model","tecvayli_model","rybrevant_model"
    )) {
      items <- append(items, list(
        numericInput("weight_kg", "Körpergewicht (kg)", 75, min = 1, step = 0.1)
      ))
    }
    
    if (model == "talvey_model") {
      items <- append(items, list(
        selectInput("talvey_regimen", "Talvey Erhaltungsschema",
                    choices = c("0,4 mg/kg wöchentlich" = "weekly", "0,8 mg/kg alle 2 Wochen" = "q2w"),
                    selected = "weekly"
        )
      ))
    }
    
    if (model == "tecvayli_model") {
      items <- append(items, list(
        selectInput("tecvayli_regimen", "Tecvayli Erhaltungsschema",
                    choices = c("1,5 mg/kg wöchentlich" = "weekly", "1,5 mg/kg alle 2 Wochen" = "q2w"),
                    selected = "weekly"
        )
      ))
    }
    
    if (model == "elrexfio_model") {
      items <- append(items, list(
        selectInput("elrexfio_regimen", "Elrexfio Schema",
                    choices = c(
                      "Standard: bis Woche 24 wöchentlich, danach alle 2 Wochen" = "default",
                      "Immer wöchentlich" = "weekly_only"
                    ),
                    selected = "default"
        )
      ))
    }
    
    if (model == "lynozyfic_model") {
      items <- append(items, list(
        selectInput("lynozyfic_regimen", "Lynozyfic Schema",
                    choices = c(
                      "Standard: bis Woche 13 wöchentlich, danach alle 2 Wochen" = "default",
                      "Immer wöchentlich" = "weekly_only"
                    ),
                    selected = "default"
        )
      ))
    }
    
    if (model == "ordspono_model") {
      items <- append(items, list(
        selectInput("ordspono_indikation", "Ordspono Indikation",
                    choices = c("FL" = "FL", "DLBCL" = "DLBCL"),
                    selected = "FL"
        )
      ))
    }
    
    if (model %in% c("lunsumio_cycles", "phased_weight_interval")) {
      default_cycles <- ifelse(model == "lunsumio_cycles", 8, 12)
      items <- append(items, list(
        numericInput("total_cycles", "Gesamtzahl Zyklen", default_cycles, min = 1, step = 1)
      ))
    } else if (!(model %in% c("one_time_weight_mgkg", "manual"))) {
      items <- append(items, list(
        numericInput("months", "Behandlungsdauer (Monate)", 12, min = 0.1, step = 0.5)
      ))
    }
    
    if (isTRUE(input$manual_mode)) {
      if (berechnung == "mg") {
        items <- append(items, list(
          numericInput("manual_annual_dose_mg", "Jahresdosis manuell (mg)", 1000, min = 0, step = 0.1)
        ))
      } else {
        items <- append(items, list(
          numericInput("manual_annual_packs", "Jahrespackungen manuell", 12, min = 0, step = 1)
        ))
      }
    }
    
    do.call(tagList, items)
  })
  
  output$info_tbl <- renderTable({
    d <- selected()
    req(!is.null(d))
    
    vals <- list(
      "Produkt" = safe_chr(safe_col(d, "Produkt", "")),
      "Wirkstoff" = safe_chr(safe_col(d, "Wirkstoff", "")),
      "Dosierungstyp" = safe_chr(safe_col(d, "Dosierungstyp", "")),
      "Therapiedauer" = safe_chr(safe_col(d, "Therapiedauer_Typ", "")),
      "Berechnungstyp" = safe_chr(safe_col(d, "Berechnungstyp", ""))
    )
    
    data.frame(
      Feld = names(vals),
      Wert = unname(unlist(vals)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, bordered = TRUE, striped = TRUE)
  
  output$section42 <- renderText({
    d <- selected()
    req(!is.null(d))
    safe_chr(safe_col(d, "Abschnitt_4_2_Kurz", "Keine Daten verfügbar"))
  })
  
  res <- reactive({
    
    d <- selected()
    
   
    req(!is.null(d))
    req(nrow(d) > 0)
    
    
    req(!is.null(input$produkt))
    

    if (is.null(input$manual_mode)) {
      return(NULL)
    }
    
    calc_annual(d, input)
  })
  
  output$formula_txt <- renderText({
    x <- res()
    req(!is.null(x))
    safe_chr(x$formula)
  })
  
  output$result_tbl <- renderTable({
    x <- res()
    req(!is.null(x))
    
    dose_val <- if (!is.null(x$annual_dose_mg) && length(x$annual_dose_mg) == 1 && !is.na(x$annual_dose_mg)) {
      round(x$annual_dose_mg, 2)
    } else {
      "NA"
    }
    
    packs_val <- if (!is.null(x$annual_packs) && length(x$annual_packs) == 1 && !is.na(x$annual_packs)) {
      x$annual_packs
    } else {
      "NA"
    }
    
    cost_val <- if (!is.null(x$annual_cost) && length(x$annual_cost) == 1 && !is.na(x$annual_cost)) {
      round(x$annual_cost, 2)
    } else {
      "NA"
    }
    
    data.frame(
      Kennzahl = c("Jahresdosis (mg)", "Jahrespackungen", "Jahrestherapiekosten (€)"),
      Wert = c(dose_val, packs_val, cost_val),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, bordered = TRUE, striped = TRUE)
}

shinyApp(ui, server)
