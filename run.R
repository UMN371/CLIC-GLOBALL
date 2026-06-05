# =============================================================================
#  CLIC DCC Dashboard — launcher
#  Open this file in RStudio and click "Source", or run it in the R console.
#  (Use forward slashes in the path — R treats "\" as an escape character.)
# =============================================================================

app_dir <- "C:/Users/luxxx371/Box/Requests/Clic/Administration/Dashboard"

# --- First time only: install packages + LaTeX for PDF reports ---
# source(file.path(app_dir, "setup.R"))

# --- Launch the dashboard ---
shiny::runApp(app_dir, launch.browser = TRUE)
