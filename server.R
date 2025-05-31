# ─────────────────────────────────────────────────────────────────────────────
# server.R
# Qui mettiamo tutta la logica di Reactivity e i render dei grafici / tabelle.
# ─────────────────────────────────────────────────────────────────────────────


server <- function(input, output, session) {
  
  # ─── 3.1. REACTIVE per “Evolutionary Routes”: dati di rete (force + sankey)
  net_data <- eventReactive(input$draw, {
    # Selezione dei geni da input; se NULL, uso vettore vuoto
    selected <- if (is.null(input$mutations)) character(0) else input$mutations
    
    # Trova quali dei geni selezionati sono effettivamente parent in 'routes'
    selected_parents <- routes %>%
      filter(parent %in% selected) %>%
      pull(parent) %>%
      unique()
    
    # Prendo tutte le coppie (parent→child) per i parent selezionati
    active_links <- routes %>%
      filter(parent %in% selected_parents)
    
    # Tutti i figli potenziali (unique)
    potential_children <- unique(active_links$child)
    
    # I nodi da mostrare sono: geni selezionati + figli potenziali
    display_genes <- union(selected, potential_children)
    
    # Costruisco il tibble dei nodi
    nodes <- tibble(
      name  = display_genes,
      group = case_when(
        name %in% selected ~ "present",
        TRUE               ~ "potential"
      )
    )
    
    # Costruisco il tibble dei link con indici 0-based per D3
    links <- active_links %>%
      mutate(
        source = match(parent, nodes$name) - 1L,
        target = match(child,  nodes$name) - 1L,
        value  = 1
      )
    
    # Restituisco la lista di nodi, link e genitori validi
    list(
      nodes = nodes,
      links = links,
      selected_parents = selected_parents
    )
  })
  
  # ─── 3.2. renderForceNetwork (Force-Directed Graph)
  output$net_now <- renderForceNetwork({
    data <- net_data()
    
    # Se non ci sono genitori validi, interrompo e mostro un messaggio
    if (length(data$selected_parents) == 0) {
      validate(need(FALSE, "Select at least one Parent mutation valid for drawing the Force-Directed Graph"))
    }
    
    # Altrimenti disegno regolarmente il grafo
    forceNetwork(
      Links      = data$links,
      Nodes      = data$nodes,
      Source     = "source", 
      Target     = "target",
      NodeID     = "name",
      Group      = "group",
      Value      = "value",
      fontSize   = 14,
      opacity    = 0.9,
      zoom       = TRUE,
      legend     = TRUE
    ) %>%
      onRender(
        'function(el,x){
           // Rimuovo eventuali etichette già presenti
           d3.select(el).selectAll(".nodelabel").remove();
           // Aggiungo un <text> con il nome del nodo a ciascun nodo
           d3.select(el).selectAll("g.node")
             .append("text")
             .attr("class","nodelabel")
             .attr("dx", 10)
             .attr("dy", ".35em")
             .text(function(d){ return d.name; })
             .style("pointer-events","none")
             .style("font-family","Helvetica, Arial, sans-serif")
             .style("font-size","12px")
             .style("fill","#333");
        }'
      )
  })
  
  # ─── 3.3. renderSankeyNetwork (Sankey Diagram)
  output$net_sankey <- renderSankeyNetwork({
    data <- net_data()
    
    # Se non ci sono genitori validi, interrompo e mostro un messaggio
    if (length(data$selected_parents) == 0) {
      validate(need(FALSE, "Select at least one Parent mutation valid for drawing the Sankey Diagram"))
    }
    
    # Altrimenti disegno regolarmente il Sankey
    sankeyNetwork(
      Links      = data$links,
      Nodes      = data$nodes,
      Source     = "source",
      Target     = "target",
      Value      = "value",
      NodeID     = "name",
      NodeGroup  = "group",
      sinksRight = FALSE,
      fontSize   = 14,
      nodeWidth  = 20,
      nodePadding= 15
    )
  })
  
  # ─── 3.4. renderUI per la legenda “present” vs “potential”
  output$legend_routes <- renderUI({
    pal <- c(present = "#3182bd", potential = "#9ecae1")
    tags$div(style = "margin-top:8px;",
             tags$span(
               style = sprintf("display:inline-block;width:16px;height:16px;background:%s;", pal["present"])
             ), " present    ",
             tags$span(
               style = sprintf("display:inline-block;width:16px;height:16px;background:%s;", pal["potential"])
             ), " potential"
    )
  })
  
  # ─── 3.5. renderDT per la tabella delle probabilità downstream
  output$prob_table <- renderDT({
    data <- net_data()
    req(data)  # garantisco che net_data() esista
    df <- prob_tbl %>%
      filter(parent %in% data$selected_parents) %>%
      arrange(parent, desc(prob)) %>%
      mutate(prob = scales::percent(prob, accuracy = 0.1))
    
    if (nrow(df) == 0) {
      datatable(
        data.frame(Message = "Select at least one parent mutation ↑"),
        options = list(dom = "t"),
        rownames = FALSE
      )
    } else {
      datatable(
        df,
        colnames = c("Parent", "Child", "Probability"),
        options = list(pageLength = 10, dom = "tip"),
        rownames = FALSE
      )
    }
  })
  
  
  # ─── 3.6. PARTE “IPSS-M-Evo Calculator”: al click “Calculate”
  observeEvent(input$go, {
    # 3.6.1 Validazione degli input numerici
    validate(
      need(!is.na(input$ipssm) && input$ipssm >= 0 && input$ipssm <= 5,
           "Insert an IPSS-M score valid between 0 and 5")
    )
    validate(
      need(input$age >= 18 && input$age <= 100,
           "Insert an age between 18 and 100")
    )
    
    # 3.6.2 Calcolo dello score combinato
    score <- input$ipssm * weights$ipssm +
      input$age   * weights$age +
      ifelse(input$axk, weights$ASXL1_KRAS, 0) +
      ifelse(input$snr, weights$SRSF2_NRAS, 0) +
      ifelse(input$nrr, weights$NRAS_RUNX1,  0) +
      ifelse(input$atr, weights$ATRX,        0) +
      ifelse(input$jak, weights$JAK2,        0)
    
    # 3.6.3 Determino la categoria di rischio e recupero mediana corrispondente
    risk   <- risk_class(score)
    medrow <- surv_tbl %>% filter(class == risk)
    
    # 3.6.4 Sicurezza: accerto che medrow non sia vuoto
    validate(
      need(nrow(medrow) == 1, "Internal error: category of risk not found")
    )
    
    # 3.6.5 Render dei tre testi (score, categoria, mediane)
    output$score_txt <- renderText({
      paste0("IPSS-M-Evo score: ", round(score, 3))
    })
    output$risk_txt <- renderText({
      paste0("Risk category: ", risk)
    })
    output$med_txt <- renderText({
      paste0(
        "Median LFS = ", medrow$LFS, " mo   |   Median OS = ", medrow$OS, " mo"
      )
    })
    
    # 3.6.6 Simulo i dati di sopravvivenza (fisso seed per riproducibilità)
    set.seed(12345)
    dat_lfs <- sim_km(surv_tbl, "LFS")
    dat_os  <- sim_km(surv_tbl, "OS")
    
    # 3.6.7 Fitting Kaplan-Meier per LFS e OS
    fit_lfs <- survfit(Surv(time, status) ~ group, data = dat_lfs)
    fit_os  <- survfit(Surv(time, status) ~ group, data = dat_os)
    
    # 3.6.8 Converto i fit in data frame “tidy” e aggiungo colonna patient=TRUE/FALSE
    tidy_lfs <- surv_summary(fit_lfs, dat_lfs) %>%
      mutate(patient = (group == risk))
    tidy_os  <- surv_summary(fit_os,  dat_os)  %>%
      mutate(patient = (group == risk))
    
    # 3.6.9 Definisco tema e funzione di plotting per le curve KM
    km_theme <- theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
    
    km_plot_fun <- function(df, title) {
      ggplot(df, aes(time, surv, group = group)) +
        geom_step(color = "grey75", linewidth = 0.5) +
        geom_step(
          data   = df %>% filter(patient),
          color  = risk_cols[risk],
          linewidth = 1.4
        ) +
        scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 12)) +
        labs(title = title, x = "Months", y = "Probability") +
        km_theme
    }
    
    # 3.6.10 Render del grafico KM (LFS e OS) affiancati con patchwork
    output$km_plot <- renderPlot({
      km_plot_fun(tidy_lfs, "Leukemia-Free Survival") +
        km_plot_fun(tidy_os,  "Overall Survival") +
        plot_layout(widths = c(1, 1))
    })
  })
  
} # /server

