
# ─────────────────────────────────────────────────────────────────────────────
# global.R
# ─────────────────────────────────────────────────────────────────────────────

# ─── 1. LIBRARIE (package) ───────────────────────────────────────────────────

# >>> Librerie necessarie per l'interfaccia Shiny e analisi dati
library(shiny)            # Framework per applicazioni web interattive
library(tidyverse)        # Insieme di pacchetti per manipolazione e visualizzazione dati
library(networkD3)        # Per visualizzazioni network (es. sankeyNetwork)
library(htmlwidgets)      # Per integrare widgets HTML in Shiny
library(htmltools)        # Per elementi HTML personalizzati (es. tags)
library(DT)               # Tabelle interattive (DataTables)
library(survival)         # Modelli di sopravvivenza (es. survfit)
library(survminer)        # Estensione per visualizzare risultati di sopravvivenza
library(patchwork)        # Per combinare più ggplot
library(plotly)           # Grafici interattivi
library(biomaRt)          # Per accedere al database Ensembl (mapping gene → ID)
library(jsonlite)         # Per esportazione/importazione dati in JSON
library(shinyWidgets)     # Widget aggiuntivi per UI Shiny

# ─── 2. DATI DI SUPPORTO PER “Evolutionary Routes” ─────────────────────────────

# 2.1. Vie evolutive parent → child 
# >>> Elenco delle mutazioni geniche osservate in sequenza (vie evolutive)
routes <- tribble(
  ~parent, ~child,
  "ASXL1","BCOR",
  "ASXL1","KRAS",
  "ASXL1","NF1",
  "ASXL1","PHF6",
  "ASXL1","STAG2",
  "DNMT3A","BCOR",
  "EZH2","RUNX1",
  "EZH2","STAG2",
  "SF3B1","RUNX1",
  "SRSF2","CBL",
  "SRSF2","IDH1",
  "SRSF2","NRAS",
  "SRSF2","RUNX1",
  "SRSF2","STAG2",
  "TET2","MPL",
  "TET2","STAG2",
  "TET2","ZRSR2"
)

# ─── MAPPING Gene → Ensembl ID (PRE-COMPILATO) ────────────────────────────

# >>> Tabella che associa ciascun gene al suo Ensembl ID (mappatura fissa)
gene_map <- data.frame(
  gene = c("ATRX", "BCOR", "BCORL1", "CBL", "CEBPA", "CREBBP", "CSF3R", "CUX1", "DNMT3A", 
           "EP300", "ETNK1", "ETV6", "EZH2", "FLT3", "GATA2", "GNB1", "GNAS", "IDH1", 
           "IDH2", "JAK2", "KIT", "KMT2D", "KRAS", "MPL", "NF1", "NOTCH1", "NPM1", 
           "NRAS", "PHF6", "PPM1D", "PRPF40B", "PTPN11", "RAD21", "RUNX1", "SETBP1", 
           "SF3B1", "SMC1A", "SMC3", "SRSF2", "STAG2", "TET2", "TP53", "U2AF1", 
           "WT1", "ZRSR2"),
  ensembl = c("ENSG00000085224", "ENSG00000167671", "ENSG00000167674", "ENSG00000110395", 
              "ENSG00000245848", "ENSG00000005339", "ENSG00000119535", "ENSG00000106278", 
              "ENSG00000119772", "ENSG00000100393", "ENSG00000127329", "ENSG00000139083", 
              "ENSG00000106462", "ENSG00000122025", "ENSG00000179348", "ENSG00000078369", 
              "ENSG00000187498", "ENSG00000138413", "ENSG00000182054", "ENSG00000096968", 
              "ENSG00000157404", "ENSG00000167548", "ENSG00000133703", "ENSG00000117400", 
              "ENSG00000196712", "ENSG00000148400", "ENSG00000181163", "ENSG00000213281", 
              "ENSG00000122180", "ENSG00000173960", "ENSG00000166922", "ENSG00000179295", 
              "ENSG00000164754", "ENSG00000159216", "ENSG00000152284", "ENSG00000115461", 
              "ENSG00000072501", "ENSG00000108055", "ENSG00000161547", "ENSG00000101972", 
              "ENSG00000168769", "ENSG00000141510", "ENSG00000160201", "ENSG00000184937", 
              "ENSG00000169249"),
  stringsAsFactors = FALSE
)

# 2.2. Elenco alfabetico dei 47 geni
# >>> Vettore dei geni ordinati
genes <- sort(gene_map$gene)


# 2.2. Elenco completo di 47 geni (ordinato alfabeticamente)
#genes <- c(
#  "ATRX","BCOR","BCORL1","CBL","CEBPA","CREBBP","CSF3R","CUX1","DNMT3A",
#  "EP300","ETNK1","ETV6","EZH2","FLT3","GATA2","GNB1","GNAS","IDH1",
#  "IDH2","JAK2","KIT","KMT2D","KRAS","MPL","NF1","NOTCH1","NPM1",
#  "NRAS","PHF6","PPM1D","PRPF40B","PTPN11","RAD21","RUNX1","SETBP1",
#  "SF3B1","SMC1A","SMC3","SRSF2","STAG2","TET2","TP53","U2AF1",
#  "WT1","ZRSR2"
#) %>% sort()

# ─── 2. MAPPING Gene → Ensembl ID ────────────────────────────────────────────
# Usa biomaRt per scaricare gli ENS IDs dei 47 geni
# mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
# gene_map <- getBM(
#  attributes = c("hgnc_symbol", "ensembl_gene_id"),
#  filters    = "hgnc_symbol",
#  values     = genes,
#  mart       = mart
#) %>%
#  rename(gene = hgnc_symbol, ensembl = ensembl_gene_id)


# ─── 2.3. Probabilità evolutive (parent → child) ─────────────────────────────

# >>> Tabella che associa ad ogni coppia di geni una probabilità di evoluzione
prob_tbl <- tribble(
  ~parent,  ~child,   ~prob,
  "ASXL1",  "BCOR",   0.078,
  "ASXL1",  "KRAS",   0.038,
  "ASXL1",  "NF1",    0.071,
  "ASXL1",  "PHF6",   0.075,
  "ASXL1",  "STAG2",  0.250,
  "DNMT3A", "BCOR",   0.108,
  "EZH2",   "RUNX1",  0.369,
  "EZH2",   "STAG2",  0.226,
  "SF3B1",  "RUNX1",  0.086,
  "SRSF2",  "CBL",    0.058,
  "SRSF2",  "IDH1",   0.088,
  "SRSF2",  "NRAS",   0.068,
  "SRSF2",  "RUNX1",  0.332,
  "SRSF2",  "STAG2",  0.312,
  "TET2",   "MPL",    0.039,
  "TET2",   "STAG2",  0.110,
  "TET2",   "ZRSR2",  0.126
)

# ─── 3. DATI DI SUPPORTO PER “IPSS-M-Evo Calculator” ───────────────────────────

# 3.1. Classi di rischio e colori associati
# >>> Codifica colori per classi di rischio da usare nei grafici
risk_levels <- c("Very Low","Low","Moderate Low","Moderate High","High","Very High")
risk_cols   <- c(
  "Very Low"      = "#4CAF50",
  "Low"           = "#2196F3",
  "Moderate Low"  = "#cca9dd",
  "Moderate High" = "#FCD12A",
  "High"          = "#e7a13e",
  "Very High"     = "#c8513b"
)

# 3.2. Mediane di sopravvivenza per classe di rischio
# >>> Mediane (in mesi) di LFS (leukemia-free survival) e OS (overall survival)
surv_tbl <- data.frame(
  class = factor(risk_levels, levels = risk_levels),
  LFS   = c(70.2, 48.3, 28.5, 18.1, 11.7,  6.9),
  OS    = c(79.4, 56.7, 36.8, 22.5, 14.0,  8.3)
)

# 3.3. Pesi per calcolo score IPSS-M-Evo
# >>> Coefficienti per variabili nel modello di punteggio prognostico
weights <- list(
  ipssm      = 0.619021793668649,
  age        = 0.0205415570417613,
  ASXL1_KRAS = 0.555946605542339,
  SRSF2_NRAS = 0.653000345623331,
  NRAS_RUNX1 = -0.805292858388642,
  ATRX       = 0.407911813415511,
  JAK2       = 0.46186776075953
)

# ─── 4. FUNZIONI DI UTILITÀ (Utilities) ───────────────────────────────────────

# 4.1. Assegnazione della classe di rischio in base allo score
# >>> Funzione che ritorna la classe in base al valore dello score numerico
risk_class <- function(s) {
  case_when(
    s <= 0.50 ~ "Very Low",
    s <= 1.00 ~ "Low",
    s <= 1.50 ~ "Moderate Low",
    s <= 2.00 ~ "Moderate High",
    s <= 3.00 ~ "High",
    TRUE      ~ "Very High"
  )
}

# 4.2. Simulazione di dati di sopravvivenza per n pazienti per classe
# >>> Genera dati Kaplan-Meier simulati 
sim_km <- function(tbl, ep) {
  map_dfr(seq_len(nrow(tbl)), \(i) {
    lam <- log(2) / tbl[[ep]][i]  
    data.frame(
      time   = rexp(100, lam),   
      status = 1,                
      group  = factor(tbl$class[i], levels = risk_levels)
    )
  })
}