
# ─────────────────────────────────────────────────────────────────────────────
# server.R
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ─── 3.1. REACTIVE per “Evolutionary Routes”: dati di rete (force + sankey)
  # >>> net_data() si ricalcola solo quando l'utente preme il pulsante "draw"
  net_data <- eventReactive(input$draw, {
    
    # --- 1) Leggo le mutazioni selezionate dall'utente
    selected <- if (is.null(input$mutations)) character(0) else input$mutations
    
    # --- 2) Trovo quali dei geni selezionati sono effettivamente parent in 'routes'
    selected_parents <- routes %>%
      filter(parent %in% selected) %>%
      pull(parent) %>% unique()
    
    # --- 3) Seleziono le coppie parent→child relative a quei parent
    active_links <- routes %>%
      filter(parent %in% selected_parents)
    
    # --- 4) Individuo tutti i figli potenziali unici
    potential_children <- unique(active_links$child)
    
    # --- 5) I nodi da mostrare in grafo: i parent scelti + i loro figli
    display_genes <- union(selected_parents, potential_children)
    
    # --- 6) Costruisco tibble dei nodi, con nome, gruppo e Ensembl ID
    nodes <- tibble(
      name    = display_genes,
      group   = case_when(
        name %in% selected_parents ~ "present",   # geni già mutati
        TRUE                       ~ "potential"  # possibili step evolutivi
      ),
      ensembl = gene_map$ensembl[match(display_genes, gene_map$gene)]
    ) %>%
      mutate(
        # > Se manca Ensembl ID, lo sostituisco con il simbolo del gene
        ensembl = if_else(is.na(ensembl), name, ensembl)
      )
    
    # ─── 2) Aggiungo nodi “gruppo figli” e “gruppo orfani” per ciascun parent
    parents_with_child <- unique(active_links$parent)
    grp_nodes <- tibble(name=character(), group=character(), ensembl=character())
    for(p in parents_with_child) {
      # 2a) Nodo contenitore per tutti i figli di p
      grp_nodes <- add_row(grp_nodes,
                           name    = paste0(p, "_childrens"),
                           group   = "group",
                           ensembl = NA_character_
      )
      # 2b) Nodo per gli orfani (mutazioni scelte ma non figlie di p)
      orph <- setdiff(selected, c(p, active_links$child[active_links$parent == p]))
      if(length(orph)) {
        grp_nodes <- add_row(grp_nodes,
                             name    = paste(orph, collapse = ", "),
                             group   = "group",
                             ensembl = NA_character_
        )
      }
    }
    # Unisco nodi base + group‑nodes, evitando duplicati sul nome
    nodes <- bind_rows(nodes, grp_nodes) %>% distinct(name, .keep_all = TRUE)
    
    # ─── 3) Creo mappa name → id (0‑based) per D3.js/Sankey
    id_map <- set_names(seq_len(nrow(nodes)) - 1L, nodes$name)
    
    # ─── 4) Costruisco df links (source, target, value)
    links <- tibble(source = integer(), target = integer(), value = numeric())
    for(p in parents_with_child) {
      # 4a) COLLEGAMENTI parent → gruppo figli → ogni figlio
      kids <- active_links$child[active_links$parent == p]
      if(length(kids)) {
        grp_c <- paste0(p, "_childrens")
        # parent ➔ gruppo bambini
        links <- add_row(links,
                         source = id_map[p],
                         target = id_map[grp_c],
                         value  = length(kids))
        # gruppo bambini ➔ ciascun figlio
        for(k in kids) {
          links <- add_row(links,
                           source = id_map[grp_c],
                           target = id_map[k],
                           value  = 1)
        }
      }
      # 4b) COLLEGAMENTO parent → gruppo orfani (se esistono)
      orph <- setdiff(selected, c(p, kids))
      if(length(orph)) {
        grp_o_name <- paste(orph, collapse = ", ")
        links <- add_row(links,
                         source = id_map[p],
                         target = id_map[grp_o_name],
                         value  = length(orph))
      }
    }
    
    # ─── 5) Restituisco la lista con nodi, link e parent attivi
    list(
      nodes = nodes,
      links = links,
      selected_parents = selected_parents
    )
  })
  
  # ─── Costruzione di jsMap per collegare gene → Ensembl URL in JS
  jsMap <- toJSON(
    as.list(setNames(gene_map$ensembl, gene_map$gene)),
    auto_unbox = TRUE
  )
  
  # ─── 3.2. renderForceNetwork (Force‑Directed Graph)
  output$net_now <- renderForceNetwork({
    # >>> Mostra messaggio se non ci sono parent selezionati
    shiny::validate(
      shiny::need(length(net_data()$selected_parents) > 0,
                  "⚠️ Select at least one Parent mutation valid for drawing the Force‑Directed Graph")
    )
    
    data <- net_data()
    
    # >>> Costruisco grafo e aggiungo onRender JS per click sui nodi
    forceNetwork(
      Links    = data$links,
      Nodes    = data$nodes,
      Source   = "source", 
      Target   = "target",
      NodeID   = "name",
      Group    = "group",
      Value    = "value",
      fontSize = 14,
      opacity  = 1.0,        
      opacityNoHover = 1.0,
      zoom     = TRUE,
      legend   = TRUE
    ) %>% onRender(
      sprintf(
        'function(el, x) {
           var ensMap = %s;
           d3.select(el).selectAll("g.node")
             .filter(function(d) { return d.group !== "group"; })
             .style("cursor", "pointer")
             .on("click", function(d) {
               var id = ensMap[d.name] || d.name;
               window.open("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=" + id, "_blank");
             });
         }',
        jsMap
      )
    )
  })
  
  
  # ─── 3.3. renderSankeyNetwork (Sankey Diagram)
  output$net_sankey <- renderSankeyNetwork({
    shiny::validate(
      shiny::need(length(net_data()$selected_parents) > 0,
                  "⚠️ Select at least one Parent mutation valid for drawing the Sankey Diagram")
    )
    data <- net_data()
    # >>> Disegno il Sankey e aggiungo onRender JS per click su barre/rect
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
    ) %>% onRender(
      sprintf(
        'function(el, x) {
           var ensMap = %s;
           // Rimuovo drag sui nodi
           d3.select(el).selectAll(".node").on("mousedown.drag", null);
           d3.select(el).selectAll(".node")
             .filter(function(d) { return d.group !== "group"; })
             .select("rect")
             .style("cursor", "pointer")
             .on("click", function(d) {
               var id = ensMap[d.name] || d.name;
               window.open("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=" + id, "_blank");
             });
         }',
        jsMap
      )
    )
  })
  
  
  # ─── 3.4. renderUI per la legenda “present” vs “potential”
  output$legend_routes <- renderUI({
    # >>> Piccola legenda cromatica per distinguere i nodi
    pal <- c(present   = "#3182bd",
             potential = "#9ecae1",
             group     = "#d08000")
    tags$div(style="margin-top:8px;",
             tags$span(style=sprintf("display:inline-block;width:16px;height:16px;background:%s;", pal["present"])), " present  ",
             tags$span(style=sprintf("display:inline-block;width:16px;height:16px;background:%s;", pal["potential"])), " potential  ",
             tags$span(style=sprintf("display:inline-block;width:16px;height:16px;background:%s;", pal["group"])), " group"
    )
  })
  
  
  # ─── 3.5. renderDT per la tabella delle probabilità downstream
  output$prob_table <- renderDT({
    data <- net_data()
    req(data)  # >>> Mi assicuro che i dati esistano
    
    # Filtro e ordino la tabella di probabilità
    df <- prob_tbl %>%
      filter(parent %in% data$selected_parents) %>%
      arrange(parent, desc(prob)) %>%
      mutate(prob = scales::percent(prob, accuracy = 0.1))
    
    # Se non ci sono righe, mostro un messaggio in tabella
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
  
  # 3.6 Reset selezione mutazioni (pulsante “clear_all”)
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "mutations", selected = character(0))
  })
  
  # ─── 3.7. PARTE “IPSS-M-Evo Calculator” ────────────────────────────────────
  
  # 3.7.1 Reset calcolatore (pulsante “reset_calc”)
  observeEvent(input$reset_calc, {
    updateNumericInput(session, "ipssm", value = NA)
    updateNumericInput(session, "age",   value = 70)
    updateCheckboxInput(session, "axk",  value = FALSE)
    updateCheckboxInput(session, "snr",  value = FALSE)
    updateCheckboxInput(session, "nrr",  value = FALSE)
    updateCheckboxInput(session, "atr",  value = FALSE)
    updateCheckboxInput(session, "jak",  value = FALSE)
    # Riporto input$go a 0 per nascondere eventuali output
    session$sendInputMessage("go", list(value = 0))
  })
  
  # 3.7.2 Calcolo dello score quando si clicca “go”
  observeEvent(input$go, {
    # --- Validazione input
    if (is.na(input$ipssm) || input$ipssm < 0 || input$ipssm > 5) {
      showNotification("Insert an IPSS-M score valid between 0 and 5", type="error")
      return()
    }
    if (input$age < 18 || input$age > 100) {
      showNotification("Insert an age between 18 and 100", type="error")
      return()
    }
    
    # --- Calcolo dello score combinando ipssm, age e mutazioni
    score <- input$ipssm * weights$ipssm +
      input$age   * weights$age +
      ifelse(input$axk, weights$ASXL1_KRAS, 0) +
      ifelse(input$snr, weights$SRSF2_NRAS, 0) +
      ifelse(input$nrr, weights$NRAS_RUNX1,  0) +
      ifelse(input$atr, weights$ATRX,        0) +
      ifelse(input$jak, weights$JAK2,        0)
    
    # --- Determino categoria di rischio e mediana corrispondente
    risk   <- risk_class(score)
    medrow <- surv_tbl %>% filter(class == risk)
    
    # --- Render dei testi di output
    output$score_txt <- renderText(paste0("IPSS-M-Evo score: ", round(score, 3)))
    output$risk_txt  <- renderText(paste0("Risk category: ", risk))
    output$med_txt   <- renderText(paste0("Median LFS = ", medrow$LFS,
                                          " mo   |   Median OS = ", medrow$OS, " mo"))
    
    # --- Simulo dati di sopravvivenza per plotting (seed fisso per riproducibilità)
    set.seed(12345)
    dat_lfs <- sim_km(surv_tbl, "LFS")
    dat_os  <- sim_km(surv_tbl, "OS")
    
    # --- Fitting Kaplan‑Meier dei dati simulati
    fit_lfs <- survfit(Surv(time, status) ~ group, data = dat_lfs)
    fit_os  <- survfit(Surv(time, status) ~ group, data = dat_os)
    
    # --- Converto in formato “tidy” per ggplot e evidenzio il gruppo paziente
    tidy_lfs <- surv_summary(fit_lfs, dat_lfs) %>%
      mutate(patient = (group == risk))
    tidy_os  <- surv_summary(fit_os,  dat_os) %>%
      mutate(patient = (group == risk))
    
    # --- Definisco tema comune e funzione per il plotting delle curve KM
    km_theme <- theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 18))
    km_plot_fun <- function(df) {
      ggplot(df, aes(time, surv, group = group)) +
        geom_step(color = "grey75", linewidth = 0.5) +
        geom_step(data = df %>% filter(patient),
                  color  = risk_cols[risk],
                  linewidth = 1.4) +
        scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 12)) +
        labs(x = "Months", y = "Probability") +
        km_theme
    }
    
    # --- Render dei grafici LFS e OS come Plotly interattivi
    output$km_lfs <- renderPlotly({
      p_lfs <- km_plot_fun(tidy_lfs)
      ggplotly(p_lfs) %>%
        layout(margin = list(l = 50, r = 20, b = 40, t = 20))
    })
    output$km_os <- renderPlotly({
      p_os <- km_plot_fun(tidy_os)
      ggplotly(p_os) %>%
        layout(margin = list(l = 50, r = 20, b = 40, t = 20))
    })
  })
  
} # /server
