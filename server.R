
# ─────────────────────────────────────────────────────────────────────────────
# server.R
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # tutti i geni che appaiono come parent in routes
  parent_genes <- unique(routes$parent)
  
  # ─── 1. FILTRO PER LA BARRA DI RICERCA ────────────────────────────────────
  
  # 1.1 reactive che filtra i geni in base al testo digitato
  filtered_genes <- reactive({
    # se non ha ancora digitato nulla, ritorna tutti i geni
    txt <- str_trim(input$gene_search %||% "")
    if (txt == "") {
      genes
    } else {
      # ricerca case-insensitive
      genes[str_detect(genes, regex(txt, ignore_case = TRUE))]
    }
  })
  
  # 1.2 renderUI per creare dinamicamente il checkboxGroupInput
  output$genes_checkboxes <- renderUI({
    # geni filtrati dal testo di ricerca
    matches   <- filtered_genes()
    # geni già selezionati
    already   <- input$mutations %||% character(0)
    # unisco in modo che i selected non spariscano
    all_choices <- sort(unique(c(already, matches)))
    
    if (length(all_choices) == 0) {
      tags$em("No genes available")
    } else {
      # preparo choiceNames/choiceValues
      choice_values <- all_choices
      choice_names  <- lapply(all_choices, function(g) {
        if (g %in% parent_genes) {
          tags$span(style = "font-weight:bold; color:#3182bd;",
                    g,
                    tags$span(style = "font-size:80%; color:#666;", " (parent)"))
        } else {
          g
        }
      })
      
      checkboxGroupInput(
        inputId    = "mutations",
        label      = NULL,
        choiceNames  = choice_names,
        choiceValues = choice_values,
        selected     = already,
        width        = "100%"
      )
    }
  })
  
  # ─── REACTIVE per “Evolutionary Routes”: dati di rete (force + sankey)
  # >>> net_data() si ricalcola solo quando l'utente preme il pulsante "draw"
  net_data <- eventReactive(input$draw, {
    
    # 1) Input selezionati
    selected <- if (is.null(input$mutations)) character(0) else input$mutations
    
    # 2) Identifico i veri parent tra i selezionati
    selected_parents <- routes %>%
      filter(parent %in% selected) %>%
      pull(parent) %>% unique()
    
    # 3) Filtri solo le coppie parent→child attive
    active_links <- routes %>%
      filter(parent %in% selected_parents)
    
    # 4) Preparo tibble vuoti per nodi e link
    nodes <- tibble(name = character(), group = character(), ensembl = character())
    links <- tibble(source = integer(), target = integer(), value = numeric())
    
    # 5) Ciclo su ciascun parent per costruire la gerarchia
    for (p in selected_parents) {
      # 5.1) Nodo radice del parent
      nodes <- add_row(nodes,
                       name    = p,
                       group   = "present",
                       ensembl = gene_map$ensembl[gene_map$gene == p])
      
      # 5.2) Individuo figli e orfani
      kids  <- active_links %>% filter(parent == p) %>% pull(child)
      orphs <- setdiff(selected, union(selected_parents, kids))
      
      # 5.3) Nodo gruppo “childrens” + link p → p_childrens
      child_grp <- paste0(p, "_childrens")
      nodes <- add_row(nodes,
                       name    = child_grp,
                       group   = "group",
                       ensembl = NA_character_)
      links <- add_row(links,
                       source = NA,
                       target = NA,
                       value  = length(kids))  # 0 se nessun figlio
      
      # 5.4) Sotto‐link p_childrens → ciascun figlio
      for (k in kids) {
        nodes <- add_row(nodes,
                         name    = k,
                         group   = "potential",
                         ensembl = gene_map$ensembl[gene_map$gene == k])
        links <- add_row(links,
                         source = NA,
                         target = NA,
                         value  = 1)
      }
      
      # 5.5) Nodo gruppo “orphans” (sempre) + link p → p_orphans
      orphan_grp <- paste0(p, "_orphans")
      nodes <- add_row(nodes,
                       name    = orphan_grp,
                       group   = "group",
                       ensembl = NA_character_)
      # assicuro valore minore uguale a 1 per far comparire il ramo
      links <- add_row(links,
                       source = NA,
                       target = NA,
                       value  = ifelse(length(orphs) > 0, length(orphs), 1))
      
      # 5.6) Sotto‐link p_orphans → ciascun orphan (se presenti)
      for (o in orphs) {
        nodes <- add_row(nodes,
                         name    = o,
                         group   = "potential",
                         ensembl = gene_map$ensembl[gene_map$gene == o])
        links <- add_row(links,
                         source = NA,
                         target = NA,
                         value  = 1)
      }
    }
    
    # 6) Rimuovo duplicati e forzo l’ordine dei livelli per i colori
    nodes <- distinct(nodes, name, .keep_all = TRUE) %>%
      mutate(group = factor(group, levels = c("present", "potential", "group")))
    
    # 7) Creo la mappa name → id (0‐based)
    id_map <- set_names(seq_len(nrow(nodes)) - 1L, nodes$name)
    
    # 8) Compilo source/target nei link nell’ordine in cui sono stati aggiunti
    li <- 1L
    for (p in selected_parents) {
      kids  <- active_links %>% filter(parent == p) %>% pull(child)
      orphs <- setdiff(selected, union(selected_parents, kids))
      
      # p → p_childrens
      links$source[li] <- id_map[p]
      links$target[li] <- id_map[paste0(p, "_childrens")]
      li <- li + 1L
      
      # p_childrens → kids
      for (k in kids) {
        links$source[li] <- id_map[paste0(p, "_childrens")]
        links$target[li] <- id_map[k]
        li <- li + 1L
      }
      
      # p → p_orphans
      links$source[li] <- id_map[p]
      links$target[li] <- id_map[paste0(p, "_orphans")]
      li <- li + 1L
      
      # p_orphans → orphs (nessuno se orphs è vuoto)
      for (o in orphs) {
        links$source[li] <- id_map[paste0(p, "_orphans")]
        links$target[li] <- id_map[o]
        li <- li + 1L
      }
    }
    
    # 9) Restituisco la lista a renderForce/Sankey
    list(
      nodes            = nodes,
      links            = links,
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
      legend   = TRUE,
      colourScale = JS("
      d3.scaleOrdinal()
        .domain(['present','potential','group'])
        .range(['#3182bd','#9ecae1','#d08000'])
    ")
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
      nodePadding= 15,
      colourScale = JS("
      d3.scaleOrdinal()
        .domain(['present','potential','group'])
        .range(['#3182bd','#9ecae1','#d08000'])
    ")
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
    # 1) Svuota la search bar
    updateTextInput(session, "gene_search", value = "")
    
    # 2) Deseleziona tutte le checkbox
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
  
  # 3.7.2 Calcolo dello score quando si clicca “calculate risk”
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
