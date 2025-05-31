# ─────────────────────────────────────────────────────────────────────────────
# global.R
# ─────────────────────────────────────────────────────────────────────────────

# ─── 1. LIBRARIE (package) ───────────────────────────────────────────────────
library(shiny)
library(tidyverse)    # include dplyr, ggplot2, purrr, tibble, scales, ecc.
library(networkD3)    # per forceNetwork e sankeyNetwork
library(htmlwidgets)  # necessario per onRender()
library(htmltools)    # per tags$div, tags$span (legenda personalizzata)
library(DT)           # per tabelle interattive (DataTables)
library(survival)     # per funzioni di sopravvivenza (Surv, survfit)
library(survminer)    # per surv_summary()
library(patchwork)    # per affiancare due ggplot

# ─── 2. DATI DI SUPPORTO PER “Evolutionary Routes” ─────────────────────────────

# 2.1. Vie evolutive con coppie parent → child (validated routes)
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

# 2.2. Elenco completo di 47 geni (ordinato alfabeticamente)
genes <- c(
  "ATRX","BCOR","BCORL1","CBL","CEBPA","CREBBP","CSF3R","CUX1","DNMT3A",
  "EP300","ETNK1","ETV6","EZH2","FLT3","GATA2","GNB1","GNAS","IDH1",
  "IDH2","JAK2","KIT","KMT2D","KRAS","MPL","NF1","NOTCH1","NPM1",
  "NRAS","PHF6","PPM1D","PRPF40B","PTPN11","RAD21","RUNX1","SETBP1",
  "SF3B1","SMC1A","SMC3","SRSF2","STAG2","TET2","TP53","U2AF1",
  "WT1","ZRSR2"
) %>% sort()

# 2.3. Tabella delle probabilità hard-coded (parent → child → prob)
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

# 3.1. Livelli di rischio e colori associati
risk_levels <- c("Very Low","Low","Moderate Low","Moderate High","High","Very High")
risk_cols   <- c(
  "Very Low"      = "#4CAF50",
  "Low"           = "#2196F3",
  "Moderate Low"  = "#cca9dd",
  "Moderate High" = "#FCD12A",
  "High"          = "#e7a13e",
  "Very High"     = "#c8513b"
)

# 3.2. Mediane di LFS e OS per ciascuna classe di rischio
surv_tbl <- data.frame(
  class = factor(risk_levels, levels = risk_levels),
  LFS   = c(70.2, 48.3, 28.5, 18.1, 11.7,  6.9),
  OS    = c(79.4, 56.7, 36.8, 22.5, 14.0,  8.3)
)

# 3.3. Pesi per il calcolo dello score IPSS-M-Evo
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

# 4.1. Funzione per assegnare la categoria di rischio a partire dallo score
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

# 4.2. Funzione per simulare dati di sopravvivenza (100 pazienti per classe)
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

