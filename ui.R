
# ─────────────────────────────────────────────────────────────────────────────
# ui.R 
# ─────────────────────────────────────────────────────────────────────────────

# Imposta la UI dell'app Shiny usando un layout a pagina fluida
ui <- fluidPage(
  # >>> Includo file CSS personalizzato per stili aggiuntivi
  includeCSS("style.css"),
  
  # Wrapper principale per contenuto con classe definita in CSS
  div(class = "content-wrapper",
      # >>> Header principale con titolo e sottotitolo
      div(class = "main-header",
          h1("IPSS-M-Evo", style = "display: inline-block;"),   # Titolo app
          div(class = "subtitle",                              # Sottotitolo descrittivo
              "Evolutionary Routes Analysis & Risk Calculator"
          )
      ),
      
      # Tabset principale con due schede
      tabsetPanel(id = "main_tabs",
                  
                  # ── Tab 1: Evolutionary Routes ──────────────────────────
                  tabPanel(
                    title = tagList(icon("dna"), "Evolutionary Routes"),
                    br(),
                    sidebarLayout(
                      # >>> Pannello laterale per selezione geni
                      sidebarPanel(width = 3,
                                   div(class = "sidebar-card",
                                       div(class = "sidebar-title",
                                           icon("dna"),                       # Icona a sinistra
                                           "Gene Selection"                   # Titolo sezione
                                       ),
                                       
                                       # >>> Info box esplicativa sotto il titolo
                                       div(class = "info-box",
                                           icon("info-circle", class = "info-box-icon"),
                                           "Select one or more mutations detected in the patient to visualize potential evolutionary pathways."
                                       ),
                                       
                                       # >>> Checkbox group per la selezione dei geni
                                       div(class = "checkbox-group",
                                           checkboxGroupInput(
                                             inputId = "mutations",             # id input usato in server.R
                                             label   = NULL,
                                             choices = genes,                     # vettore genes da global.R
                                             selected = NULL,
                                             width   = "100%"
                                           )
                                       ),
                                       
                                       br(),
                                       # >>> Pulsanti Generate e Clear All affiancati
                                       div(style = "text-align: center;",
                                           fluidRow(
                                             column(6,
                                                    actionButton(
                                                      inputId = "draw",               # attiva il reactive
                                                      label   = "Generate",
                                                      icon    = icon("project-diagram"),
                                                      class   = "btn-custom btn-primary-custom",
                                                      style   = "width: 100%;"
                                                    )
                                             ),
                                             column(6,
                                                    actionButton(
                                                      inputId = "clear_all",          # reset selezione geni
                                                      label   = "Clear All",
                                                      icon    = icon("trash"),
                                                      class   = "btn-custom btn-danger-custom",
                                                      style   = "width: 100%;"
                                                    )
                                             )
                                           )
                                       )
                                   )
                      ),
                      
                      # >>> Pannello principale che mostra grafici e tabella
                      mainPanel(width = 9,
                                div(class = "content-card",
                                    # >>> Sottotab per Force Network e Sankey
                                    tabsetPanel(
                                      # Force-Directed Graph
                                      tabPanel(
                                        title = tagList(icon("share-alt"), "Force Network"),
                                        br(),
                                        div(style = "position: relative; min-height: 650px;",
                                            # Stato vuoto prima di click su Generate
                                            conditionalPanel(
                                              condition = "!input.draw || input.draw == 0",
                                              div(class = "empty-state",
                                                  style = "height: 600px; display: flex; flex-direction: column; justify-content: center;",
                                                  icon("project-diagram", "fa-4x", style = "color: #dee2e6; margin-bottom: 20px;"),
                                                  h4("Network Visualization", style = "margin-bottom: 10px;"),
                                                  p("Select genes and click 'Generate' to visualize evolutionary pathways")
                                              )
                                            ),
                                            # Force Network output dopo click
                                            conditionalPanel(
                                              condition = "input.draw > 0",
                                              forceNetworkOutput("net_now", height = "650px")
                                            )
                                        )
                                      ),
                                      # Sankey Diagram
                                      tabPanel(
                                        title = tagList(icon("flow-chart"), "Sankey Diagram"),
                                        br(),
                                        div(style = "position: relative; min-height: 550px;",
                                            # Empty state prima di Generate
                                            conditionalPanel(
                                              condition = "!input.draw || input.draw == 0",
                                              div(class = "empty-state",
                                                  style = "height: 500px; display: flex; flex-direction: column; justify-content: center;",
                                                  icon("flow-chart", "fa-4x", style = "color: #dee2e6; margin-bottom: 20px;"),
                                                  h4("Sankey Diagram", style = "margin-bottom: 10px;"),
                                                  p("Select genes and click 'Generate' to visualize flow pathways")
                                              )
                                            ),
                                            # Sankey output dopo click
                                            conditionalPanel(
                                              condition = "input.draw > 0",
                                              sankeyNetworkOutput("net_sankey", height = "550px"),
                                              br(),
                                              uiOutput("legend_routes")           # legenda dei colori
                                            )
                                        )
                                      )
                                    )
                                ),
                                
                                # >>> Sezione per la tabella delle probabilità downstream
                                div(class = "content-card results-section",
                                    h4(icon("percentage"), " Downstream Event Probabilities", 
                                       style = "color: #495057; margin-bottom: 15px;"),
                                    DTOutput("prob_table")
                                )
                      )
                    )
                  ),
                  
                  # ── Tab 2: IPSS-M-Evo Calculator ────────────────────────────
                  tabPanel(
                    title = tagList(icon("calculator"), "Risk Calculator"),
                    br(),
                    sidebarLayout(
                      # >>> Pannello laterale per input parametri paziente
                      sidebarPanel(width = 3,
                                   div(class = "sidebar-card",
                                       div(class = "sidebar-title",
                                           icon("calculator"),
                                           "Patient Parameters"
                                       ),
                                       
                                       # Info box
                                       div(class = "info-box",
                                           icon("user-md", class = "info-box-icon"),
                                           "Enter the patient's clinical and molecular characteristics to calculate the IPSS-M-Evo risk score."
                                       ),
                                       
                                       # Sezione parametri clinici
                                       div(
                                         h5("Clinical Parameters", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                                         numericInput(
                                           inputId = "ipssm",
                                           label   = tagList(icon("chart-line"), "IPSS-M Score"),
                                           value   = NA,
                                           min     = 0, max = 5, step = 0.01,
                                           width   = "100%"
                                         ),
                                         numericInput(
                                           inputId = "age",
                                           label   = tagList(icon("birthday-cake"), "Age (years)"),
                                           value   = 70,
                                           min     = 18, max = 100, step = 1,
                                           width   = "100%"
                                         )
                                       ),
                                       br(),
                                       # Sezione mutazioni molecolari
                                       div(
                                         h5("Molecular Features", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                                         div(style = "padding: 10px; background: #f8f9fa; border-radius: 8px;",
                                             checkboxInput("axk", tagList(icon("arrow-right"), "ASXL1 → KRAS"), value = FALSE),
                                             checkboxInput("snr", tagList(icon("arrow-right"), "SRSF2 → NRAS"), value = FALSE),
                                             checkboxInput("nrr", tagList(icon("plus"), "NRAS + RUNX1"), value = FALSE),
                                             checkboxInput("atr", tagList(icon("dna"), "ATRX mutation"), value = FALSE),
                                             checkboxInput("jak", tagList(icon("dna"), "JAK2 mutation"), value = FALSE)
                                         )
                                       ),
                                       br(),
                                       # Pulsanti Calculate e Reset
                                       fluidRow(
                                         column(6,
                                                actionButton("go", "Calculate Risk", icon = icon("calculator"),
                                                             class = "btn-custom btn-primary-custom", style = "width:100%")
                                         ),
                                         column(6,
                                                actionButton("reset_calc", "Reset Values", icon = icon("trash"),
                                                             class = "btn-custom btn-danger-custom", style = "width:100%")
                                         )
                                       )
                                   )
                      ),
                      
                      # >>> Pannello principale per visualizzazione risultati
                      mainPanel(width = 9,
                                div(class = "content-card",
                                    div(id = "results_container",
                                        # Empty state prima di Calculate
                                        conditionalPanel(
                                          condition = "input.go == 0",
                                          div(class = "empty-state",
                                              icon("arrow-left", "fa-2x", style = "color: #dee2e6;"),
                                              h4("Enter patient parameters and click 'Calculate Risk'", style = "margin-top: 15px; color: #6c757d;")
                                          )
                                        ),
                                        # Output dopo il click
                                        conditionalPanel(
                                          condition = "input.go > 0",
                                          div(
                                            fluidRow(
                                              # Card Score
                                              column(4,
                                                     div(class = "result-card",
                                                         h5(icon("calculator"), " Score", style = "margin-bottom: 10px;"),
                                                         verbatimTextOutput("score_txt")
                                                     )
                                              ),
                                              # Card Risk Category
                                              column(4,
                                                     div(class = "result-card", style = "border-left-color: #ffc107;",
                                                         h5(icon("exclamation-triangle"), " Risk Category", style = "margin-bottom: 10px;"),
                                                         verbatimTextOutput("risk_txt")
                                                     )
                                              ),
                                              # Card Survival
                                              column(4,
                                                     div(class = "result-card", style = "border-left-color: #17a2b8;",
                                                         h5(icon("clock"), " Survival", style = "margin-bottom: 10px;"),
                                                         verbatimTextOutput("med_txt")
                                                     )
                                              )
                                            ),
                                            br(),
                                            fluidRow(
                                              column(6,
                                                     h3("Leukemia‑Free Survival", align = "center"),
                                                     plotlyOutput("km_lfs", height = "550px")
                                              ),
                                              column(6,
                                                     h3("Overall Survival", align = "center"),
                                                     plotlyOutput("km_os", height = "550px")
                                              )
                                            )
                                          )
                                        )
                                    )
                                )
                      )
                    )
                  )
      )
  )
)
