# Deploying the CLIC dashboard to Posit Connect

Both files must sit in the **same project folder**:

```
clic-dashboard/
├── app.R
├── report.Rmd
├── setup.R              ← run once: installs packages + TinyTeX (for PDF)
├── clic_data.xlsx       ← the editable data source (one sheet per page)
└── www/
    ├── clic_logo.png
    └── clic_mark.png
```

## One-time setup

1. Install the publishing package:
   ```r
   install.packages("rsconnect")
   ```
2. Register your Connect server (get the URL + an API key from your Connect
   account → top-right menu → **API Keys**):
   ```r
   rsconnect::connectApiUser(
     account = "yourname",
     server  = "connect.yourinstitution.org",
     apiKey  = "PASTE_KEY"
   )
   ```

## Publish

**From RStudio:** open `app.R`, click the blue **Publish** icon (top-right of the
editor), select the Connect server, make sure both `app.R` and `report.Rmd` are
ticked, and publish.

**From the console:**
```r
rsconnect::deployApp(
  appDir   = "clic-dashboard",
  appName  = "clic-genomics-dashboard",
  account  = "yourname",
  server   = "connect.yourinstitution.org"
)
```

## Notes for a real (sensitive-data) deployment

- **Access control:** in Connect, set the content to *Specific users/groups*
  rather than *Anyone* — important for patient-linked genomics data.
- **Data source:** replace the placeholder `study_codes` block in `app.R` with a
  read from your governed source (CSV on the Connect server, a database
  connection via `pool`/`DBI`, or an API). Keep credentials in Connect
  **Environment Variables**, never in the code.
- **Scheduled report email:** because the report is plain `report.Rmd`, you can
  *also* publish it to Connect on its own and schedule an automatic email of the
  rendered HTML — no extra code needed.
- **Dependencies:** Connect reads `renv`/package info automatically at publish
  time. If you hit a missing-package error, run `rsconnect::writeManifest()` in
  the folder and redeploy.
- **Report formats:** HTML and Word downloads work with no extra setup. For the
  **PDF** option, install LaTeX once on the Connect server (or in your renv):
  `tinytex::install_tinytex()`. Without it, HTML and Word still work fine.
