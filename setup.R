# =============================================================================
#  CLIC DCC Dashboard — one-time setup
#  Run ONCE before the first launch (locally) and once on the Posit Connect
#  server / renv. Safe to re-run: it only installs what's missing.
#      source("setup.R")
# =============================================================================

pkgs <- c("shiny","bslib","dplyr","tidyr","ggplot2","DT",
          "rmarkdown","htmltools","readxl","tinytex")

new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)

# LaTeX engine for PDF report downloads (HTML & Word need nothing extra).
# Installs a lightweight TinyTeX distribution; skipped if already present.
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}

message("Setup complete — TinyTeX present: ", tinytex::is_tinytex())
