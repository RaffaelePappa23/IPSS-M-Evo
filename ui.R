# ─────────────────────────────────────────────────────────────────────────────
# ui.R 
# ─────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  includeCSS("style.css"),
  
  # Offcanvas sidebar
  tags$nav(id = "sidebar",
           tags$ul(
             # Evolutionary Routes
             tags$li(a(href = "#",
                       onclick = "
          Shiny.setInputValue('main_section','evo',{priority:'event'});
          document.getElementById('sidebar').classList.remove('active');
          document.getElementById('menu-toggle').classList.remove('active');
        ",
                       icon("dna"), " Evolutionary Routes"
             )),
             # Risk Calculator
             tags$li(a(href = "#",
                       onclick = "
          Shiny.setInputValue('main_section','calc',{priority:'event'});
          document.getElementById('sidebar').classList.remove('active');
          document.getElementById('menu-toggle').classList.remove('active');
        ",
                       icon("calculator"), " Risk Calculator"
             )),
             # New: Support
             tags$li(a(href = "#",
                       onclick = "
          Shiny.setInputValue('main_section','support',{priority:'event'});
          document.getElementById('sidebar').classList.remove('active');
          document.getElementById('menu-toggle').classList.remove('active');
        ",
                       icon("life-ring"), " Support"
             )),
             # New: Publications
             tags$li(a(href = "#",
                       onclick = "
          Shiny.setInputValue('main_section','pubs',{priority:'event'});
          document.getElementById('sidebar').classList.remove('active');
          document.getElementById('menu-toggle').classList.remove('active');
        ",
                       icon("book"), " Publications"
             )),
             # New: Privacy Policy
             tags$li(a(href = "#",
                       onclick = "
          Shiny.setInputValue('main_section','privacy',{priority:'event'});
          document.getElementById('sidebar').classList.remove('active');
          document.getElementById('menu-toggle').classList.remove('active');
        ",
                       icon("user-secret"), " Privacy Policy"
             ))
           )
  ),
  
  # Wrapper principale
  div(class = "content-wrapper",
      # Header
      div(class = "main-header",
          tags$div(id = "menu-toggle", 
                   icon("bars", class = "fa-2x"),
                   tags$span("Menu", class = "menu-label")
          ),
          h1("IPSS-M-Evo"),
          div(class = "subtitle",
              "Evolutionary Routes Analysis & Risk Calculator"
          )
      ),
      
      # → Evolutionary Routes
      conditionalPanel(
        condition = "input.main_section == 'evo' || typeof input.main_section === 'undefined'",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
                       div(class = "sidebar-card",
                           div(class = "sidebar-title", icon("dna"), "Gene Selection"),
                           div(class = "info-box",
                               icon("info-circle", class = "info-box-icon"),
                               "Select one or more mutations detected in the patient to visualize potential evolutionary pathways."
                           ),
                           div(class = "checkbox-group",
                               # 1.1. barra di ricerca
                               textInput("gene_search", 
                                         label = NULL, 
                                         placeholder = "Search gene...", 
                                         width = "100%"),
                               # 1.2. contenitore dinamico per i checkbox filtrati
                               uiOutput("genes_checkboxes")  
                           ),
                           br(),
                           div(style = "text-align:center;",
                               fluidRow(
                                 column(6,
                                        actionButton("draw", "Generate", icon = icon("project-diagram"),
                                                     class = "btn-custom btn-primary-custom", style = "width:100%;")
                                 ),
                                 column(6,
                                        actionButton("clear_all", "Clear All", icon = icon("trash"),
                                                     class = "btn-custom btn-danger-custom", style = "width:100%;")
                                 )
                               )
                           )
                       )
          ),
          mainPanel(width = 9,
                    div(class = "content-card",
                        tabsetPanel(
                          tabPanel(
                            title = tagList(icon("share-alt"), "Force Network"),
                            br(),
                            conditionalPanel(
                              condition = "!input.draw || input.draw==0",
                              div(class="empty-state",
                                  icon("project-diagram","fa-4x"),
                                  h4("Network Visualization"),
                                  p("Select genes and click 'Generate'")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.draw>0",
                              forceNetworkOutput("net_now", height="650px")
                            )
                          ),
                          tabPanel(
                            title = tagList(icon("flow-chart"), "Sankey Diagram"),
                            br(),
                            conditionalPanel(
                              condition = "!input.draw || input.draw==0",
                              div(class="empty-state",
                                  icon("flow-chart","fa-4x"),
                                  h4("Sankey Diagram"),
                                  p("Select genes and click 'Generate'")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.draw>0",
                              sankeyNetworkOutput("net_sankey", height="550px"),
                              uiOutput("legend_routes")
                            )
                          )
                        )
                    ),
                    div(class="content-card results-section",
                        h4(icon("percentage"), " Downstream Event Probabilities"),
                        DTOutput("prob_table")
                    )
          )
        )
      ),
      
      # → Risk Calculator
      conditionalPanel(
        condition = "input.main_section == 'calc'",
        br(),
        sidebarLayout(
          sidebarPanel(width = 3,
                       div(class = "sidebar-card",
                           div(class = "sidebar-title", icon("calculator"), "Patient Parameters"),
                           div(class = "info-box",
                               icon("info-circle", class = "info-box-icon"),
                               "Enter clinical/molecular characteristics to calculate the IPSS-M-Evo risk score."
                           ),
                           h5("Clinical Parameters"),
                           numericInput("ipssm", tagList(icon("chart-line"), "IPSS-M Score"), value = NA, min=0, max=5, step=0.01, width="100%"),
                           numericInput("age", tagList(icon("birthday-cake"), "Age (years)"), value=70, min=18, max=100, step=1, width="100%"),
                           br(),
                           h5("Molecular Features"),
                           div(style="padding:10px; background:#f8f9fa; border-radius:8px;",
                               checkboxInput("axk", tagList(icon("arrow-right"), "ASXL1 → KRAS"), FALSE),
                               checkboxInput("snr", tagList(icon("arrow-right"), "SRSF2 → NRAS"), FALSE),
                               checkboxInput("nrr", tagList(icon("plus"), "NRAS + RUNX1"), FALSE),
                               checkboxInput("atr", tagList(icon("dna"), "ATRX mutation"), FALSE),
                               checkboxInput("jak", tagList(icon("dna"), "JAK2 mutation"), FALSE)
                           ),
                           br(),
                           fluidRow(
                             column(6,
                                    actionButton("go", "Calculate Risk", icon=icon("calculator"),
                                                 class="btn-custom btn-primary-custom", style="width:100%;")
                             ),
                             column(6,
                                    actionButton("reset_calc", "Reset Values", icon=icon("trash"),
                                                 class="btn-custom btn-danger-custom", style="width:100%;")
                             )
                           )
                       )
          ),
          mainPanel(width = 9,
                    div(class="content-card",
                        div(id="results_container",
                            conditionalPanel(
                              condition = "input.go == 0",
                              div(class="empty-state",
                                  icon("arrow-left","fa-2x"),
                                  h4("Enter parameters and click 'Calculate Risk'")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.go > 0",
                              fluidRow(
                                column(4,
                                       div(class="result-card",
                                           h5(icon("calculator"), " Score"),
                                           verbatimTextOutput("score_txt")
                                       )
                                ),
                                column(4,
                                       div(class="result-card", style="border-left-color:#ffc107;",
                                           h5(icon("exclamation-triangle"), " Risk Category"),
                                           verbatimTextOutput("risk_txt")
                                       )
                                ),
                                column(4,
                                       div(class="result-card", style="border-left-color:#17a2b8;",
                                           h5(icon("clock"), " Survival"),
                                           verbatimTextOutput("med_txt")
                                       )
                                )
                              ),
                              br(),
                              fluidRow(
                                column(6,
                                       h3("Leukemia-Free Survival", align="center"),
                                       plotlyOutput("km_lfs", height="550px")
                                ),
                                column(6,
                                       h3("Overall Survival", align="center"),
                                       plotlyOutput("km_os", height="550px")
                                )
                              )
                            )
                        )
                    )
          )
        )
      ),
      
      # → Support
      conditionalPanel(
        condition = "input.main_section == 'support'",
        br(),
        div(class="content-card",
            h2(icon("life-ring"), "Support"),
            p("Sezione per inserire tutte le informazioni di contatto, FAQ e moduli di supporto per gli utenti."),
        )
      ),
      
      # → Publications
      conditionalPanel(
        condition = "input.main_section == 'pubs'",
        br(),
        div(class="content-card",
            h2(icon("book"), "Publications"),
            p("Sezione per le pubblicazioni scientifiche e i materiali di approfondimento."),
        )
      ),
      
      # → Privacy Policy
      conditionalPanel(
        condition = "input.main_section == 'privacy'",
        br(),
        div(class="content-card",
            h2(icon("user-secret"), "Privacy Policy"),
            p("Sezione per la policy sulla privacy, trattamento dati, cookie e GDPR."),
        )
      )
  ),
  
  # Script menu-toggle e default section
  tags$script(HTML("
    document.getElementById('menu-toggle').onclick = function(){
      document.getElementById('sidebar').classList.toggle('active');
      this.classList.toggle('active');
    };
    document.addEventListener('DOMContentLoaded', function(){
      Shiny.setInputValue('main_section','evo',{priority:'event'});
    });
  "))
)
