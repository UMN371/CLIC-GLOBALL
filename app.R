# =============================================================================
#  CLIC DCC — Global Progress Dashboard  (Shiny + bslib)
#  Data-driven: one page per sheet. Status colours: 5 canonical (PDF) categories
#  fixed; empty/Unknown -> grey; ANY other value -> its own new category+colour.
#  Home page summarises every tab. Filters: Continent · Country · Study · Status.
#  Files: app.R · report.Rmd · setup.R · clic_data.xlsx · www/clic_logo.png · www/clic_mark.png
# =============================================================================
library(shiny); library(bslib); library(dplyr); library(tidyr)
library(ggplot2); library(DT); library(htmltools); library(readxl)

XLSX <- "clic_data.xlsx"

# Embed logos as data URIs so they display wherever the PNGs sit (folder root OR www/).
logo_uri <- function(cands){ for(f in cands) if(file.exists(f)) return(knitr::image_uri(f)); "" }
LOGO <- logo_uri(c("clic_logo.png","www/clic_logo.png","./clic_logo.png"))
MARK <- logo_uri(c("clic_mark.png","www/clic_mark.png","./clic_mark.png"))

# Columns to hide entirely from a given sheet's tracker (matched on collapsed whitespace).
HIDE_COLS <- list("Genomic Pipeline" = c(
  "Unique CLIC ID Assigned",
  "Called Genomic Data Files & Metadata Files Received",
  "Genomic Ancestry Inferred",
  "Genomic Sex Inferred",
  "Notes"))
norm_hdr <- function(x) gsub("\\s+"," ", trimws(as.character(x)))

CANON     <- c("Complete","In Process","Incomplete","N/A","Unknown")
CANON_COL <- c("Complete"="#4CAF50","In Process"="#ffcc33","Incomplete"="#E0413B","N/A"="#1F1F1F","Unknown"="#D2D2D2")
EXTRA_PAL <- c("#4E79C0","#8E5FB0","#2AA198","#C71585","#8B5A2B","#00897B","#7B8B3A","#B5651D","#5D6D7E")
LEGLAB    <- c("Complete"="Complete / Yes","In Process"="In process / Planned",
               "Incomplete"="Incomplete / No","N/A"="N/A","Unknown"="Unknown")
BLUE<-"#78B4CC"; TEAL_D<-"#3E8E78"; GOLD<-"#ffcc33"; SLATE<-"#505A5A"; INK<-"#2A4A4B"; GOLD_INK<-"#8C6D1F"

ID_EXACT <- c("Study membership","Continent","Location","Study name","Tumor type(s)",
 "Year of Study","Year CLIC Membership Started","DNA Available","Study PI 1 Name",
 "Analysis Center","Country","Type","Contributing Center","Study/Dataset","Cancer Type","Study")
is_id <- function(n){ n<-trimws(n); n %in% ID_EXACT || grepl("Number of|Extension", n) }

canon_map <- function(s){              # s = lowercased trimmed scalar
  if(is.na(s)) return("Unknown")
  if(s %in% c("complete","completed","complete/yes","yes","milestone met","done",
              "done for cases only","done only for cases","done for cases","complete*")) return("Complete")
  if(s %in% c("in process","in progress","in process/planned","in progress/planned","planned","ongoing")) return("In Process")
  if(s %in% c("incomplete","incomplete/no","no")) return("Incomplete")
  if(s %in% c("n/a","na","not applicable","not needed")) return("N/A")
  if(s %in% c("","unknown","tbd","nan")) return("Unknown")
  return(NA_character_)                # not canonical -> keep original (new category)
}
norm_status <- function(x){
  orig <- trimws(as.character(x)); s <- tolower(orig)
  mapped <- vapply(s, canon_map, character(1))
  ifelse(is.na(mapped), ifelse(is.na(orig) | orig=="", "Unknown", orig), mapped)
}

CONT <- c(Australia="Oceania","New Zealand"="Oceania",Brazil="South America",Canada="North America",
 "Costa Rica"="Central America",Guatemala="Central America",Mexico="North America",USA="North America",
 "United States"="North America",Denmark="Europe",Egypt="Africa",Finland="Europe",France="Europe",
 Germany="Europe",Greece="Europe",Italy="Europe",Spain="Europe",Sweden="Europe",
 "United Kingdom"="Europe",UK="Europe",Japan="Asia","South Korea"="Asia",Taiwan="Asia")
continent_of <- function(x){ c<-CONT[trimws(x)]; ifelse(is.na(c),"Other",unname(c)) }

COUNTRY_FIX <- c("USA"="United States","U.S.A."="United States","US"="United States",
                 "U.S."="United States","UK"="United Kingdom","U.K."="United Kingdom",
                 "Korea"="South Korea")
canon_country <- function(x){ x<-trimws(as.character(x)); f<-COUNTRY_FIX[x]; ifelse(is.na(f), x, unname(f)) }

SHEETS <- readxl::excel_sheets(XLSX)
SHORT <- c("Total Studies"="CLIC Total Studies","CoordinatingAnalysisCenter"="Coord. Center",
 "Genomic Pipeline"="GlobALL Pipeline","Phenotype Data"="Phenotype Data","Epi Data"="Epi Data",
 "IARC DCC Transfer"="IARC DCC")
short_title <- function(s) if(!is.na(SHORT[s])) unname(SHORT[s]) else s
short_title_vec <- function(x) vapply(as.character(x), short_title, character(1))
read_sheet <- function(s) as.data.frame(readxl::read_excel(XLSX, sheet=s), check.names=FALSE)
clean_names <- function(df){ nm<-names(df); keep<-!(is.na(nm)|nm==""|grepl("^\\.\\.\\.",nm)); df[keep] }

ingest_tracker <- function(sheet){
  df <- clean_names(read_sheet(sheet)); nm <- names(df)
  hide_norm <- if(is.null(HIDE_COLS[[sheet]])) character(0) else norm_hdr(HIDE_COLS[[sheet]])
  is_hidden <- function(x) norm_hdr(x) %in% hide_norm
  notes_col <- nm[tolower(trimws(nm))=="notes" & !vapply(nm,is_hidden,logical(1))]
  miles <- nm[!vapply(nm,is_id,logical(1)) & !(nm %in% notes_col) & !vapply(nm,is_hidden,logical(1))]
  if(length(miles)==0) return(NULL)
  group_col <- if("Country"%in%nm)"Country" else if("Continent"%in%nm)"Continent" else NA
  lc <- c("Study","Study/Dataset","Study name","Analysis Center","Contributing Center")
  label_col <- lc[lc%in%nm][1]; if(is.na(label_col)) label_col <- setdiff(nm,c(miles,notes_col))[1]
  inst_cands <- c("Contributing Center","Analysis Center")
  inst_col <- inst_cands[inst_cands %in% nm & inst_cands != label_col][1]
  df$.ord <- seq_len(nrow(df)); df$.label <- as.character(df[[label_col]])
  df$.group <- if(!is.na(group_col)) canon_country(df[[group_col]]) else ""
  df$.inst  <- if(!is.na(inst_col)) trimws(as.character(df[[inst_col]])) else ""
  df$.notes <- if(length(notes_col)) ifelse(is.na(df[[notes_col]]),"",as.character(df[[notes_col]])) else ""
  df[, c(".ord",".label",".group",".inst",".notes",miles)] |>
    tidyr::pivot_longer(all_of(miles), names_to="milestone", values_to="raw") |>
    dplyr::mutate(page=sheet, status=norm_status(raw), col_order=match(milestone,miles)) |>
    dplyr::transmute(page, ord=.ord, group=.group, inst=.inst, label=.label, milestone, col_order, status, notes=.notes)
}

TRACKERS <- list(); REGISTRY <- list()
for(s in SHEETS){ lt<-ingest_tracker(s); if(is.null(lt)) REGISTRY[[s]]<-clean_names(read_sheet(s)) else TRACKERS[[s]]<-lt }
clic <- dplyr::bind_rows(TRACKERS); clic$status <- as.character(clic$status)

# dynamic categories: canonical + any novel value found
extras <- sort(setdiff(unique(clic$status), CANON))
STATUS_COLORS <- c(CANON_COL, setNames(EXTRA_PAL[((seq_along(extras)-1) %% length(EXTRA_PAL))+1], extras))
status_levels <- c(CANON, extras)
clic$status    <- factor(clic$status, levels=status_levels)
clic$continent <- continent_of(clic$group)
TRK_SHEETS <- names(TRACKERS); REG_SHEETS <- names(REGISTRY)

leglab    <- function(s){ if(!is.na(LEGLAB[s])) unname(LEGLAB[s]) else s }
scolor    <- function(s){ c<-STATUS_COLORS[[as.character(s)]]; if(is.null(c)||is.na(c)) "#D2D2D2" else c }
trunc_lab <- function(s,n=26) if(nchar(s)>n) paste0(substr(s,1,n-1),"\u2026") else s

reg_country_col <- function(df) intersect(c("Country","Location"), names(df))[1]
cont_choices <- sort(unique(c(clic$continent,
  unlist(lapply(REGISTRY, function(d) if("Continent"%in%names(d)) trimws(d$Continent) else NULL)))))
country_choices <- sort(unique(c(clic$group,
  unlist(lapply(REGISTRY, function(d){ cc<-reg_country_col(d); if(is.na(cc)) NULL else canon_country(d[[cc]]) })))))
country_choices <- country_choices[country_choices!="" & !is.na(country_choices)]
study_choices <- sort(unique(clic$label))
inst_choices <- sort(unique(clic$inst)); inst_choices <- inst_choices[inst_choices!="" & !is.na(inst_choices)]
SUMMARY_PAGES <- intersect(c("Genomic Pipeline","Phenotype Data","Epi Data","IARC DCC Transfer"), TRK_SHEETS)
study_sum_choices <- sort(unique(clic$label[clic$page %in% SUMMARY_PAGES]))

# Per-sheet status-marker shape: oval (default) | dot | check
SHAPE <- c("Phenotype Data"="dot","Epi Data"="dot","CoordinatingAnalysisCenter"="check")
shape_of <- function(s){ v<-SHAPE[s]; if(is.na(v)) "oval" else unname(v) }

# One-sentence description per tab (for the Home overview)
TAB_DESC <- c(
 "Total Studies"="Master registry of every CLIC study \u2014 membership, location, tumour type and PI.",
 "Genomic Pipeline"="Genomic processing milestones, from DTUA receipt through imputation, QC and analytic completion.",
 "Phenotype Data"="Phenotype data transfer and harmonisation status for each contributing study.",
 "Epi Data"="Epidemiological data availability and transfer status for each study.",
 "IARC DCC Transfer"="Data transfer milestones between contributing centres and the IARC Data Coordinating Centre.",
 "CoordinatingAnalysisCenter"="Roles and milestones for the coordinating and analysis centres.")
tab_desc <- function(s){ d<-TAB_DESC[s]; if(is.na(d)) "" else unname(d) }

mix_div <- function(cc){ tot<-sum(cc$n); if(!tot) return(NULL)
  div(class="mix", lapply(seq_len(nrow(cc)), function(i)
    span(style=sprintf("width:%.1f%%;background:%s;", cc$n[i]/tot*100, scolor(cc$status[i])),
         title=sprintf("%s: %d", as.character(cc$status[i]), cc$n[i])))) }

# Count studies treating any "/" in a name as separate studies (e.g. AUS-ALL/CBT = 2).
split_count <- function(x){ x<-as.character(x)
  vapply(x, function(v) if(is.na(v)||trimws(v)=="") 0L else length(strsplit(v,"/",fixed=TRUE)[[1]]), integer(1)) }
reg_study_col <- function(df) intersect(c("Study name","Study","Study/Dataset","Study membership"), names(df))[1]

# ---- styles ----
app_theme <- bs_theme(version=5, bg="#F5F8F6", fg=INK, primary=TEAL_D,
  base_font=font_google("IBM Plex Sans"), heading_font=font_google("Fraunces"), "navbar-bg"="#FFFFFF")
css <- HTML(sprintf("
  .clic-tbl{border-collapse:collapse;width:100%%;font-family:'IBM Plex Sans',sans-serif;}
  .clic-tbl thead th{vertical-align:bottom;padding:8px 5px;border-bottom:2px solid %1$s;}
  .clic-tbl th.lblh{text-align:left;font-family:'IBM Plex Mono',monospace;font-size:10px;letter-spacing:.1em;text-transform:uppercase;color:%2$s;}
  .clic-tbl th.mile{font-size:9px;font-weight:600;text-transform:uppercase;color:%1$s;text-align:center;line-height:1.22;white-space:normal;min-width:78px;max-width:140px;padding:8px 6px;vertical-align:bottom;}
  .clic-tbl td{padding:0;}
  .clic-tbl td.country{font-size:12px;color:%2$s;padding:0 10px 0 2px;white-space:nowrap;}
  .clic-tbl td.inst{font-size:12px;color:%1$s;padding:0 12px 0 2px;white-space:nowrap;}
  .clic-tbl td.study{font-weight:500;font-size:12.5px;color:%1$s;padding:6px 12px 6px 0;white-space:nowrap;}
  .clic-tbl td.cell{text-align:center;height:28px;}
  .clic-tbl td.notes{font-size:11px;font-weight:600;color:%3$s;padding-left:10px;white-space:nowrap;}
  .clic-tbl tr.grp td{border-top:1px solid #E1E8E4;}
  .ov{display:inline-block;width:28px;height:14px;border-radius:8px;border:1px solid rgba(0,0,0,.12);vertical-align:middle;}
  .dot{display:inline-block;width:15px;height:15px;border-radius:50%%;border:1px solid rgba(0,0,0,.14);vertical-align:middle;}
  .chk{display:inline-block;font-size:16px;font-weight:800;line-height:1;vertical-align:middle;}
  .legend2{display:flex;gap:16px;flex-wrap:wrap;align-items:center;padding:2px;}
  .legend2 .it{display:flex;gap:7px;align-items:center;font-size:12px;color:%1$s;}
  .scrollx{overflow-x:auto;}
  .navbar-nav .nav-link.active{color:%1$s !important;box-shadow:inset 0 -3px 0 %4$s;}
  .hero{display:flex;flex-direction:column;align-items:flex-start;padding:16px 4px 22px;}
  .hero img{height:84px;margin-bottom:12px;}
  .hero h1{font-family:'Fraunces';font-weight:600;font-size:clamp(48px,9vw,92px);margin:0;line-height:.95;color:%1$s;letter-spacing:-.01em;}
  .hero h1 em{font-style:normal;color:%4$s;}
  .hero p{color:%2$s;font-size:14px;max-width:64ch;margin:8px 0 0;}
  .ovlist{display:flex;flex-direction:column;gap:14px;}
  .ovitem{border-left:3px solid %4$s;padding:2px 0 2px 14px;}
  .ovitem .h{display:flex;justify-content:space-between;gap:12px;align-items:baseline;flex-wrap:wrap;}
  .ovitem .nm{font-family:'Fraunces';font-weight:600;font-size:16px;color:%1$s;}
  .ovitem .st{font-family:'IBM Plex Mono';font-size:11px;color:%2$s;white-space:nowrap;}
  .ovitem .ds{font-size:13px;color:%2$s;margin:3px 0 7px;}
  .sumtbl{border-collapse:collapse;width:100%%;font-family:'IBM Plex Sans';font-size:13px;}
  .sumtbl th{font-family:'IBM Plex Mono';font-size:9.5px;letter-spacing:.08em;text-transform:uppercase;color:%2$s;text-align:left;padding:6px 8px;border-bottom:2px solid %1$s;}
  .sumtbl th.r,.sumtbl td.r{text-align:right;}
  .sumtbl td{padding:7px 8px;border-bottom:1px solid #E9EDEA;color:%1$s;}
  .sumtbl td.tab{font-weight:600;}
  .mix{display:flex;height:13px;width:190px;border-radius:4px;overflow:hidden;border:1px solid #E1E8E4;}
  .mix>span{display:block;height:100%%;}
  .kpigrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(118px,1fr));gap:12px;}
  .kpi{border:1px solid #E1E8E4;border-radius:12px;padding:14px;background:linear-gradient(180deg,#FFFFFF,#F2F7F4);}
  .kpi .num{font-family:'Fraunces';font-weight:600;font-size:30px;line-height:1;color:%4$s;}
  .kpi .lbl{font-size:11.5px;color:%2$s;margin-top:6px;}
", INK, SLATE, GOLD_INK, TEAL_D))

colors_json <- paste0("{", paste(sprintf('"%s":"%s"', names(STATUS_COLORS), unname(STATUS_COLORS)), collapse=","), "}")
mark_js <- paste0(
 "window.CLIC_COLORS=", colors_json, ";",
 "window.clicMark=function(shape){return function(data,type,row){",
 "if(type!=='display') return data;",
 "if(data===null||data===undefined||data==='') return '';",
 "var c=window.CLIC_COLORS[data]||'#D2D2D2';",
 "if(shape==='check'){return '<span class=\"chk\" style=\"color:'+c+'\" title=\"'+data+'\">\\u2713</span>';}",
 "var cls=(shape==='dot')?'dot':'ov';",
 "return '<span class=\"'+cls+'\" style=\"background:'+c+';display:inline-block\" title=\"'+data+'\"></span>';",
 "};};")

legend_ui <- div(class="legend2", lapply(status_levels, function(s)
  div(class="it", span(class="ov", style=paste0("background:",scolor(s),";"), title=s), trunc_lab(leglab(s)))))

short_hdr <- function(m) trimws(m)

status_mark <- function(s, shape, tip){
  col <- scolor(s)
  if(shape=="dot")   return(span(class="dot", style=paste0("background:",col,";"), title=tip))
  if(shape=="check") return(span(class="chk", style=paste0("color:",col,";"), title=tip, HTML("&#10003;")))
  span(class="ov", style=paste0("background:",col,";"), title=tip)
}

render_oval <- function(d, shape="oval"){
  if(!nrow(d)) return(div(style="padding:30px;color:#5A727C;","No rows match the filter."))
  miles <- d |> distinct(milestone,col_order) |> arrange(col_order) |> pull(milestone)
  has_group <- any(nzchar(d$group))
  has_inst  <- any(nzchar(d$inst))
  rowsdf <- d |> distinct(ord,group,inst,label,notes) |> arrange(ord)
  header <- tags$tr(
    if(has_group) tags$th(class="lblh","Country") else NULL,
    if(has_inst)  tags$th(class="lblh","Institution") else NULL,
    tags$th(class="lblh","Study / Dataset"),
    lapply(miles, function(m) tags$th(class="mile", title=m, short_hdr(m))),
    tags$th(class="lblh","Notes"))
  prev<-"__"
  body <- lapply(seq_len(nrow(rowsdf)), function(i){
    o<-rowsdf$ord[i]; lab<-rowsdf$label[i]; grp<-rowsdf$group[i]; inst<-rowsdf$inst[i]; note<-rowsdf$notes[i]
    sr <- d[d$ord==o,]; sr <- sr[match(miles,sr$milestone),]
    new <- has_group && grp!=prev; prev<<-grp
    tags$tr(class=if(new)"grp" else "",
      if(has_group) tags$td(class="country", if(new) grp else "") else NULL,
      if(has_inst)  tags$td(class="inst", inst) else NULL,
      tags$td(class="study", lab),
      lapply(seq_along(miles), function(j){ s<-as.character(sr$status[j]); if(is.na(s))s<-"Unknown"
        tags$td(class="cell", status_mark(s, shape, paste0(miles[j]," \u2014 ", leglab(s))))}),
      tags$td(class="notes", note))
  })
  div(class="scrollx", tags$table(class="clic-tbl", tags$thead(header), tags$tbody(body)))
}

render_tracker_dt <- function(d, shape="oval"){
  if(!nrow(d)) return(DT::datatable(data.frame(` `="No rows match the filter.", check.names=FALSE),
                                     rownames=FALSE, options=list(dom='t')))
  miles <- d |> distinct(milestone,col_order) |> arrange(col_order) |> pull(milestone)
  has_group <- any(nzchar(d$group)); has_inst <- any(nzchar(d$inst)); shownotes <- any(nzchar(d$notes))
  dd <- d |> mutate(status=as.character(status))
  wide <- tidyr::pivot_wider(dd, id_cols=c(ord,group,inst,label,notes),
            names_from=milestone, values_from=status, values_fn=function(x) x[1]) |> arrange(ord)
  cols <- list()
  if(has_group) cols[["Country"]] <- wide$group
  if(has_inst)  cols[["Institution"]] <- wide$inst
  cols[["Study / Dataset"]] <- wide$label
  df <- data.frame(cols, check.names=FALSE, stringsAsFactors=FALSE)
  for(m in miles){ lv <- intersect(status_levels, unique(wide[[m]])); df[[m]] <- factor(wide[[m]], levels=lv) }
  if(shownotes) df[["Notes"]] <- wide$notes
  mile_targets <- which(names(df) %in% miles) - 1L
  non_mile     <- setdiff(seq_len(ncol(df)) - 1L, mile_targets)
  DT::datatable(df, rownames=FALSE, escape=FALSE, filter="top",
    options=list(pageLength=25, scrollX=TRUE, autoWidth=FALSE, dom='ftip',
      columnDefs=list(
        list(targets=mile_targets, className="dt-center",
             render=DT::JS(paste0("window.clicMark('", shape, "')"))),
        list(targets=mile_targets, orderable=FALSE),
        list(targets=non_mile, searchable=FALSE))))
}

render_study <- function(study){
  blocks <- lapply(SUMMARY_PAGES, function(pg){
    d <- clic[clic$page==pg & clic$label==study,]; d <- d[order(d$col_order),]
    if(!nrow(d)) return(card(card_header(short_title(pg)),
      div(style="color:#5A727C;font-size:13px;","No record for this study in this tab.")))
    meta <- paste(c(if(nzchar(d$group[1])) d$group[1], if(nzchar(d$inst[1])) d$inst[1]), collapse=" \u00b7 ")
    rows <- lapply(seq_len(nrow(d)), function(i)
      tags$tr(tags$td(class="study", d$milestone[i]),
        tags$td(style="padding:5px 0;", span(class="ov", style=paste0("background:",scolor(d$status[i]),";")),
          span(style="margin-left:8px;font-size:12.5px;", leglab(as.character(d$status[i]))))))
    notes <- unique(d$notes[nzchar(d$notes)])
    card(card_header(short_title(pg)),
      if(nzchar(meta)) div(style="color:#505A5A;font-size:12px;margin-bottom:6px;", meta),
      tags$table(class="clic-tbl", tags$tbody(rows)),
      if(length(notes)) div(style="margin-top:8px;font-size:12px;color:#8C6D1F;", paste0("Notes: ", paste(notes, collapse="; "))))
  })
  do.call(tagList, blocks)
}

gg_base <- theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
  panel.grid.major.y=element_blank(),legend.position="top",legend.title=element_blank())

# ---- UI ----
make_tab <- function(s){
  if(s %in% REG_SHEETS)
    nav_panel(short_title(s), value=s, card(full_screen=TRUE,
      card_header(paste0(s," — study registry")), DTOutput(paste0("reg_",make.names(s)))))
  else
    nav_panel(short_title(s), value=s, card(full_screen=TRUE,
      card_header(paste0(s," — milestone status (use the boxes under each column header to filter)")),
      legend_ui, tags$hr(style="margin:8px 0;border-color:#E1E8E4;"),
      DTOutput(paste0("trk_",make.names(s)))))
}

home_tab <- nav_panel("Home", value="Home",
  div(class="hero", img(src=LOGO, alt="CLIC"),
    h1(HTML("Glob<em>ALL</em>"))),
  layout_columns(col_widths=c(6,6),
    card(card_header("Tabs at a glance"), uiOutput("home_overview")),
    card(card_header("Total studies by tab"), uiOutput("home_kpis"))))

summary_tab <- nav_panel("Overall Summary", value="Summary",
  layout_columns(col_widths=c(6,6),
    card(card_header("Per-tab summary"), uiOutput("home_summary")),
    card(card_header("% complete by tab"), plotOutput("home_complete", height=320)),
    card(card_header("Status mix by tab"), plotOutput("home_plot", height=320)),
    card(card_header("Status share by tab"), plotOutput("home_pies2", height=320))))

study_tab <- nav_panel("Study Summary", value="StudySummary",
  card(card_header("Per-study summary \u2014 all pipelines for one study"),
    div(style="display:flex;gap:14px;align-items:flex-end;flex-wrap:wrap;",
      div(style="min-width:300px;flex:1;",
        selectInput("sel_study","Select a study", choices=study_sum_choices,
                    selected=if(length(study_sum_choices)) study_sum_choices[1] else NULL)),
      downloadButton("study_pdf","Download PDF", class="btn-primary"),
      downloadButton("study_word","Download Word")),
    tags$hr(style="border-color:#E1E8E4;"),
    legend_ui),
  uiOutput("study_view"))

sidebar_ui <- sidebar(width=255, title="Filters",
  selectizeInput("continents","Continent", multiple=TRUE, choices=cont_choices, options=list(placeholder="All continents")),
  selectizeInput("countries","Country", multiple=TRUE, choices=country_choices, options=list(placeholder="All countries")),
  selectizeInput("insts","Institution", multiple=TRUE, choices=inst_choices, options=list(placeholder="All institutions")),
  selectizeInput("studies","Study Name", multiple=TRUE, choices=study_choices, options=list(placeholder="All studies")),
  checkboxGroupInput("statuses","Status (mix chart only)", choices=status_levels, selected=status_levels),
  hr(),
  radioButtons("fmt","Report format", c("Word (.docx)"="word","PDF"="pdf"), selected="word"),
  downloadButton("report","Download report", class="btn-primary w-100"))

MID_SHEETS  <- intersect(c("Total Studies","Genomic Pipeline","Phenotype Data","Epi Data","IARC DCC Transfer"), SHEETS)
OTHER_SHEETS<- setdiff(SHEETS, c(MID_SHEETS,"CoordinatingAnalysisCenter"))
TAIL_SHEETS <- intersect("CoordinatingAnalysisCenter", SHEETS)

ui <- do.call(page_navbar, c(
  list(id="nav",
    title=div(style="display:flex;align-items:center;gap:10px;",
      img(src=MARK, height="32", alt="CLIC"),
      span(style="font-family:'Fraunces';font-weight:600;","GlobALL")),
    theme=app_theme, window_title="GlobALL Dashboard",
    header=tags$head(tags$style(css), tags$script(HTML(mark_js)), tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@500;600&display=swap")),
    sidebar=sidebar_ui),
  list(home_tab, summary_tab, study_tab),
  lapply(c(MID_SHEETS, OTHER_SHEETS), make_tab),
  lapply(TAIL_SHEETS, make_tab)))

# ---- SERVER ----
server <- function(input, output, session){
  base <- reactive({ d<-clic
    if(length(input$continents)) d<-d[d$continent %in% input$continents,]
    if(length(input$countries))  d<-d[trimws(d$group) %in% input$countries,]
    if(length(input$insts))      d<-d[d$inst %in% input$insts,]
    if(length(input$studies))    d<-d[d$label %in% input$studies,]
    d })
  reg_filter <- function(df){
    if(length(input$continents) && "Continent"%in%names(df)) df<-df[trimws(df$Continent)%in%input$continents,]
    cc<-reg_country_col(df); if(length(input$countries) && !is.na(cc)) df<-df[canon_country(df[[cc]])%in%input$countries,]
    df }

  for(s in TRK_SHEETS) local({ sh<-s
    output[[paste0("trk_",make.names(sh))]] <- renderDT(render_tracker_dt(base()[base()$page==sh,], shape_of(sh))) })
  for(s in REG_SHEETS) local({ sh<-s
    output[[paste0("reg_",make.names(sh))]] <- renderDT(
      datatable(reg_filter(REGISTRY[[sh]]), rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE))) })

  output$home_summary <- renderUI({
    d <- base()
    s <- d |> group_by(Tab=page) |> summarise(Studies=dplyr::n_distinct(ord),
            Milestones=dplyr::n_distinct(milestone), pct=round(mean(status=="Complete")*100), .groups="drop")
    s <- s[match(intersect(SHEETS, s$Tab), s$Tab),]
    mixbar <- function(tab){
      cc <- d |> filter(page==tab) |> count(status); tot<-sum(cc$n); if(!tot) return(NULL)
      div(class="mix", lapply(seq_len(nrow(cc)), function(i)
        span(style=sprintf("width:%.1f%%;background:%s;", cc$n[i]/tot*100, scolor(cc$status[i])),
             title=sprintf("%s: %d", as.character(cc$status[i]), cc$n[i])))) }
    head <- tags$tr(tags$th("Tab"), tags$th(class="r","Studies"), tags$th(class="r","Milestones"),
      tags$th(class="r","% complete"), tags$th("Status mix"))
    reg <- lapply(REG_SHEETS, function(rs)
      tags$tr(tags$td(class="tab", short_title(rs)), tags$td(class="r", nrow(reg_filter(REGISTRY[[rs]]))),
        tags$td(class="r","\u2014"), tags$td(class="r","\u2014"),
        tags$td(tags$em(style="color:#5A727C;font-size:11px;","registry"))))
    body <- lapply(seq_len(nrow(s)), function(i) tags$tr(
      tags$td(class="tab", short_title(s$Tab[i])), tags$td(class="r", s$Studies[i]), tags$td(class="r", s$Milestones[i]),
      tags$td(class="r", sprintf("%d%%", s$pct[i])), tags$td(mixbar(s$Tab[i]))))
    tags$table(class="sumtbl", tags$thead(head), tags$tbody(reg, body))
  })

  output$home_plot <- renderPlot({
    d <- base(); if(length(input$statuses)) d <- d[as.character(d$status)%in%input$statuses,]
    validate(need(nrow(d)>0,"No data for current filters."))
    d |> count(page,status) |> mutate(page=factor(page, levels=rev(intersect(SHEETS, unique(page))))) |>
      ggplot(aes(n,page,fill=status))+geom_col(width=.72)+scale_fill_manual(values=STATUS_COLORS,drop=FALSE)+
      labs(x="Cells",y=NULL)+gg_base })

  output$home_overview <- renderUI({
    d <- base()
    items <- lapply(intersect(SHEETS, c(MID_SHEETS, OTHER_SHEETS, TAIL_SHEETS)), function(pg){
      if(pg %in% REG_SHEETS){
        n <- nrow(reg_filter(REGISTRY[[pg]]))
        return(div(class="ovitem",
          div(class="h", span(class="nm", short_title(pg)), span(class="st", paste0(n," studies \u00b7 registry"))),
          div(class="ds", tab_desc(pg))))
      }
      dp <- d[d$page==pg,]; if(!nrow(dp)) return(NULL)
      pct <- round(mean(dp$status=="Complete")*100); ns <- dplyr::n_distinct(dp$ord)
      div(class="ovitem",
        div(class="h", span(class="nm", short_title(pg)),
            span(class="st", sprintf("%d studies \u00b7 %d%% complete", ns, pct))),
        div(class="ds", tab_desc(pg)),
        mix_div(dplyr::count(dp, status)))
    })
    div(class="ovlist", items)
  })

  pies <- function(){
    d <- base(); d <- d[d$page %in% TRK_SHEETS,]
    validate(need(nrow(d)>0,"No data for current filters."))
    cc <- d |> count(page,status)
    cc$page <- factor(short_title_vec(cc$page), levels=short_title_vec(intersect(SHEETS, unique(cc$page))))
    ggplot(cc, aes(x=2,y=n,fill=status))+geom_col(width=1,color="white",linewidth=.3,position="fill")+
      coord_polar(theta="y")+facet_wrap(~page)+xlim(.4,2.5)+
      scale_fill_manual(values=STATUS_COLORS,drop=FALSE)+theme_void(base_size=11)+
      theme(legend.position="bottom",legend.title=element_blank(),strip.text=element_text(face="bold",size=10))
  }
  output$home_pies2 <- renderPlot(pies())

  output$home_kpis <- renderUI({
    d <- base()
    cards <- lapply(intersect(SHEETS, c(MID_SHEETS, OTHER_SHEETS, TAIL_SHEETS)), function(pg){
      if(pg %in% REG_SHEETS){
        df <- reg_filter(REGISTRY[[pg]]); col <- reg_study_col(df)
        n <- if(is.na(col)) nrow(df) else sum(split_count(df[[col]]))
      } else {
        labs <- d[d$page==pg,] |> distinct(ord,label) |> pull(label); n <- sum(split_count(labs))
      }
      div(class="kpi", div(class="num", n), div(class="lbl", short_title(pg)))
    })
    div(class="kpigrid", cards)
  })

  output$home_complete <- renderPlot({
    d <- base(); d <- d[d$page %in% TRK_SHEETS,]
    validate(need(nrow(d)>0,"No data for current filters."))
    s <- d |> group_by(page) |> summarise(pct=round(mean(status=="Complete")*100), .groups="drop")
    s$page <- factor(short_title_vec(s$page), levels=rev(short_title_vec(intersect(SHEETS, s$page))))
    ggplot(s, aes(pct,page))+geom_col(fill="#3E8E78",width=.66)+
      geom_text(aes(label=paste0(pct,"%")),hjust=-0.15,size=3.6,color="#2A4A4B")+
      scale_x_continuous(limits=c(0,100),expand=expansion(mult=c(0,.12)))+
      labs(x="% complete",y=NULL)+gg_base+theme(legend.position="none") })

  scope_txt <- reactive({
    parts <- c(if(length(input$continents)) paste(input$continents,collapse=", "),
               if(length(input$countries)) paste(input$countries,collapse=", "),
               if(length(input$studies)) paste(length(input$studies),"studies"))
    if(length(parts)) paste(parts,collapse=" \u00b7 ") else "All studies" })

  output$report <- downloadHandler(
    filename=function() paste0("CLIC_progress_",Sys.Date(),".",c(html="html",word="docx",pdf="pdf")[[input$fmt]]),
    content=function(file){
      fmt<-c(html="html_document",word="word_document",pdf="pdf_document")[[input$fmt]]
      tmp<-file.path(tempdir(),"report.Rmd"); file.copy("report.Rmd",tmp,overwrite=TRUE)
      dat <- base() |> transmute(page, country=group, institution=inst, study=label, milestone, status, notes)
      withProgress(message=paste0("Rendering ",toupper(input$fmt),"\u2026"), value=0.5, {
        rmarkdown::render(tmp, output_format=fmt, output_file=file,
          params=list(data=as.data.frame(dat), scope=scope_txt(), colors=STATUS_COLORS),
          envir=new.env(parent=globalenv())) }) })

  output$study_view <- renderUI({ req(input$sel_study); render_study(input$sel_study) })
  study_dl <- function(fmt) downloadHandler(
    filename=function() paste0("GlobALL_", gsub("[^A-Za-z0-9]+","_",input$sel_study), "_",
                               Sys.Date(), if(fmt=="pdf")".pdf" else ".docx"),
    content=function(file){
      of <- if(fmt=="pdf")"pdf_document" else "word_document"
      tmp<-file.path(tempdir(),"study_report.Rmd"); file.copy("study_report.Rmd",tmp,overwrite=TRUE)
      dat <- clic[clic$label==input$sel_study & clic$page %in% SUMMARY_PAGES,] |>
        transmute(page, country=group, institution=inst, milestone, col_order, status=as.character(status), notes)
      withProgress(message=paste0("Rendering ",toupper(fmt),"\u2026"), value=0.5, {
        rmarkdown::render(tmp, output_format=of, output_file=file,
          params=list(study=input$sel_study, data=as.data.frame(dat), pages=SUMMARY_PAGES),
          envir=new.env(parent=globalenv())) }) })
  output$study_pdf  <- study_dl("pdf")
  output$study_word <- study_dl("word")
}
shinyApp(ui, server)
