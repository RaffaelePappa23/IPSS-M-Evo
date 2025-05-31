# ─────────────────────────────────────────────────────────────────────────────
# ui.R
# ─────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("IPSS-M-Evo: Evolutionary Routes & Calculator"),
  
  # Tabset di primo livello: contiene due tab separate
  tabsetPanel(
    
    # ── Tab 1: “Evolutionary Routes” ────────────────────────────────────────────
    tabPanel("Evolutionary Routes",
             sidebarLayout(
               sidebarPanel(
                 h4("Select Mutations (genes) ↓"),
                 checkboxGroupInput(
                   inputId = "mutations",
                   label   = "Mutations detected in the patient",
                   choices = genes,   # definito in global.R
                   selected = NULL,
                   width = "100%"
                 ),
                 actionButton(
                   inputId = "draw",
                   label   = "Draw networks",
                   icon    = icon("project-diagram"),
                   class   = "btn-primary"
                 )
               ),
               mainPanel(
                 # Sottotab: Force-Directed Graph / Sankey Diagram
                 tabsetPanel(
                   tabPanel(
                     "Force-Directed Graph",
                     br(),
                     forceNetworkOutput("net_now", height = "650px")
                   ),
                   tabPanel(
                     "Sankey Diagram",
                     br(),
                     sankeyNetworkOutput("net_sankey", height = "550px"),
                     br(),
                     uiOutput("legend_routes")
                   )
                 ),
                 hr(),
                 h4("Probabilities of downstream events"),
                 DTOutput("prob_table")
               )
             )  # /sidebarLayout
    ),   # /tabPanel “Evolutionary Routes”
    
    
    # ── Tab 2: “IPSS-M-Evo Calculator” ──────────────────────────────────────────
    tabPanel("IPSS-M-Evo Calculator",
             sidebarLayout(
               sidebarPanel(
                 h4("Insert parameters of the patient ↓"),
                 numericInput(
                   inputId = "ipssm",
                   label   = "IPSS-M score",
                   value   = NA,
                   min     = 0,
                   max     = 5,
                   step    = 0.01
                 ),
                 numericInput(
                   inputId = "age",
                   label   = "Age (years)",
                   value   = 70,
                   min     = 18,
                   max     = 100,
                   step    = 1
                 ),
                 checkboxInput("axk", "ASXL1 → KRAS",   value = FALSE),
                 checkboxInput("snr", "SRSF2 → NRAS",   value = FALSE),
                 checkboxInput("nrr", "NRAS + RUNX1",   value = FALSE),
                 checkboxInput("atr", "ATRX mutation",  value = FALSE),
                 checkboxInput("jak", "JAK2 mutation",  value = FALSE),
                 br(),
                 actionButton(
                   inputId = "go",
                   label   = "Calculate",
                   icon    = icon("calculator"),
                   class   = "btn-success"
                 ),
                 width = 4
               ),
               mainPanel(
                 verbatimTextOutput("score_txt"),
                 verbatimTextOutput("risk_txt"),
                 verbatimTextOutput("med_txt"),
                 br(),
                 plotOutput("km_plot", height = "600px")
               )
             )  # /sidebarLayout
    )     # /tabPanel “IPSS-M-Evo Calculator”
    
  ) # /tabsetPanel
)   # /fluidPage

