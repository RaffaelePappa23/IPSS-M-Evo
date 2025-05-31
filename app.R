# ─────────────────────────────────────────────────────────────────────────────
# app.R
# Punto di ingresso: carica `global.R`, `ui.R`, `server.R` e avvia l’app.
# ─────────────────────────────────────────────────────────────────────────────

source("global.R")

source("ui.R")

source("server.R")

shinyApp(ui = ui, server = server)


