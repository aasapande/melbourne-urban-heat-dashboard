library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(scales)
library(htmltools)

# 1. DATA 
# Small, pre-aggregated files 
# the app loads instantly and never reprocesses the raw hundreds of thousands of rows.
heat_metrics  <- read_csv("data/microclimate_hourly_metrics.csv", show_col_types = FALSE)
pm25_daily    <- read_csv("data/pm25_daily.csv",                   show_col_types = FALSE)
sensors       <- read_csv("data/sensor_summary.csv",              show_col_types = FALSE)
sensor_season <- read_csv("data/sensor_season.csv",               show_col_types = FALSE)
master        <- read_csv("data/master_precinct.csv",             show_col_types = FALSE)
energy_type   <- read_csv("data/energy_by_type_year.csv",         show_col_types = FALSE)
pm25_stats    <- read_csv("data/pm25_season_stats.csv",           show_col_types = FALSE)
tree_density  <- read_csv("data/tree_density.csv",                show_col_types = FALSE)  # 82,010 trees, ~166 m cells
sensor_hourly <- read_csv("data/sensor_hourly_metrics.csv",       show_col_types = FALSE)  # hour x month per sensor

# Sensor ICTMicroclimate-10 began recording in Aug-2024; its part-year record
# biases its mean temperature toward winter, so it is excluded from the
# per-sensor analysis in Section 3 (consistent with the Data Exploration step).
EXCLUDE_SENSOR <- "ICTMicroclimate-10"

month_levels  <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
season_levels <- c("Summer","Autumn","Winter","Spring")
energy_years  <- c(2011, 2016, 2021, 2026)

# Southern-hemisphere meteorological seasons, used to highlight the diurnal
# profile lines that belong to the season chosen in the global filter.
month_season <- c(Dec = "Summer", Jan = "Summer", Feb = "Summer",
                  Mar = "Autumn", Apr = "Autumn", May = "Autumn",
                  Jun = "Winter", Jul = "Winter", Aug = "Winter",
                  Sep = "Spring", Oct = "Spring", Nov = "Spring")

# Energy stacked-area data, ordered residential -> mixed -> commercial, with each
# year's share of the total precomputed for the percentage view.
energy_growth <- energy_type %>%
  mutate(building_type = factor(building_type, levels = c("Residential","Mixed","Commercial"))) %>%
  group_by(year) %>% mutate(share = total_gj_m / sum(total_gj_m) * 100) %>% ungroup() %>%
  arrange(year, building_type)

# Monthly PM2.5 series for the Section-2 "change over time" view.
pm25_daily <- pm25_daily %>%
  mutate(season = factor(season, levels = season_levels),
         month_lbl = factor(month_lbl, levels = month_levels))
pm25_monthly <- pm25_daily %>%
  group_by(year, month_num, month_lbl, season) %>%
  summarise(pm25 = mean(mean_pm25, na.rm = TRUE), .groups = "drop") %>%
  mutate(t = as.Date(sprintf("%d-%02d-01", year, month_num))) %>%  # first-of-month date for ordering
  arrange(t)

precinct_energy <- master %>% filter(!is.na(pct_commercial))

# Location selector for Section 1: "All sensors" plus each sensor by friendly name.
loc_sensors <- sensors %>% filter(device_id != EXCLUDE_SENSOR)
loc_choices <- c("All sensors (pooled readings)" = "ALL",
                 setNames(loc_sensors$device_id, loc_sensors$short_name))

# 2. DESIGN TOKENS 
# Colour is used as information: the cool-blue -> warm-red temperature scale is
# reused wherever temperature appears, so the encoding is learned once. Each
# story section also has its own accent (heat=red, air=gold, trees=green, energy=blue).
PAL <- list(ink = "#16232b", paper = "#f6f4ef", cool = "#2c7fb8", warm = "#d7472b",
            green = "#2a8a5d", gold = "#e0a528", grey = "#76858d")
season_cols <- c(Summer = "#d7472b", Autumn = "#e0a528", Winter = "#2c7fb8", Spring = "#2a8a5d")

# Per-metric labels, units and 3-stop colour scales (used by the heatmap/scatter).
metric_meta <- list(
  mean_temp_c   = list(unit = "\u00b0C",    lab = "Temperature", cols = c(PAL$cool, "#f3efe6", PAL$warm)),
  mean_pm25     = list(unit = "\u00b5g/m\u00b3", lab = "PM2.5",   cols = c("#f3efe6", PAL$gold, PAL$warm)),
  mean_humidity = list(unit = "%",          lab = "Humidity",    cols = c("#f3efe6", "#9fd0c8", PAL$cool)))

# Story stages, used to build the sidebar navigation and section accents.
SECTIONS <- list(
  list(id = "sec1", num = "01", title = "Heat over time",        icon = "temperature-high", col = "#e0573f"),
  list(id = "sec2", num = "02", title = "Air quality over time", icon = "wind",             col = "#e0a528"),
  list(id = "sec3", num = "03", title = "Trees & microclimate",  icon = "tree",             col = "#3fa06f"),
  list(id = "sec4", num = "04", title = "Energy demand",         icon = "bolt",             col = "#3f97cf"),
  list(id = "sec5", num = "05", title = "Key takeaways",         icon = "star",             col = "#d8b24a"))

# Helper: give every Plotly chart a transparent background so it sits on the page.
transparent <- function(p) layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                                  font = list(family = "Inter, 'Helvetica Neue', Arial, sans-serif", color = "#2b3a42"))

# 3. UI
ui <- fluidPage(
  tags$head(
    tags$title("Urban Heat, Air Quality & Green Cover \u2014 Melbourne"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600;9..144,700&family=Inter:wght@400;500;600;700&display=swap"),
    # All styling lives in this one block: fixed sidebar, hero, section cards,
    # insight panels, takeaway cards and the mobile breakpoint. %% are escaped
    tags$style(HTML(sprintf("
      :root{--ink:%s;--paper:%s;--cool:%s;--warm:%s;--green:%s;--gold:%s;--grey:%s;}
      *{box-sizing:border-box;}
      body{margin:0;background:var(--paper);color:var(--ink);
           font-family:'Inter','Helvetica Neue',Arial,sans-serif;line-height:1.55;
           -webkit-font-smoothing:antialiased;}
      .nav h1,.hero h2,.section h3{font-family:'Fraunces',Georgia,'Times New Roman',serif;letter-spacing:-0.01em;}
      .layout{display:flex;min-height:100vh;}
      .nav{position:fixed;top:0;left:0;width:236px;height:100vh;
           background:linear-gradient(176deg,#16232b 0%%,#1b303a 58%%,#13222a 100%%);
           color:#e9eef1;padding:26px 18px;overflow-y:auto;display:flex;flex-direction:column;}
      .nav h1{font-size:15px;line-height:1.3;margin:0 0 4px;font-weight:800;letter-spacing:.01em;}
      .nav .sub{font-size:11px;color:#9fb3bd;margin-bottom:24px;}
      .navitem{display:flex;gap:9px;align-items:center;padding:11px 10px;border-radius:8px;
               cursor:pointer;color:#cdd9df;margin-bottom:3px;border-left:3px solid transparent;
               transition:background .15s,color .15s,border-color .15s;}
      .navitem:hover,.navitem:focus{background:rgba(255,255,255,.07);color:#fff;outline:none;}
      .navitem:focus-visible{box-shadow:0 0 0 2px var(--gold);}
      .navitem.active{background:rgba(255,255,255,.10);color:#fff;border-left-color:var(--nav-accent,var(--warm));}
      .navitem.active .ic{color:var(--nav-accent,var(--warm));opacity:1;}
      .navitem .n{font-size:11px;opacity:.55;font-variant-numeric:tabular-nums;min-width:16px;}
      .navitem .ic{width:17px;text-align:center;font-size:13px;opacity:.8;}
      .navitem .t{font-size:13px;font-weight:600;}
      .nav .tools{margin-top:22px;border-top:1px solid rgba(255,255,255,.12);padding-top:16px;}
      .nav .tools .btn{width:100%%;text-align:left;background:none;border:none;color:#9fb3bd;
           font-size:12px;padding:8px 10px;border-radius:6px;cursor:pointer;}
      .nav .tools .btn:hover{background:rgba(255,255,255,.08);color:#fff;}
      .nav .prog{margin-top:18px;font-size:10.5px;color:#7d929c;letter-spacing:.08em;text-transform:uppercase;}
      .nav .progbar{height:4px;background:rgba(255,255,255,.12);border-radius:3px;margin-top:6px;overflow:hidden;}
      .nav .progbar i{display:block;height:100%%;width:0;background:var(--gold);transition:width .2s;}
      .nav .datafoot{margin-top:auto;padding-top:22px;}
      .nav .datafoot .df-rule{height:1px;background:rgba(255,255,255,.12);margin-bottom:14px;}
      .nav .datafoot .df-name{font-family:'Fraunces',Georgia,serif;font-size:14px;color:#dbe6ea;margin-bottom:4px;}
      .nav .datafoot .df-meta{font-size:10.5px;color:#7d929c;line-height:1.5;}
      .main{margin-left:236px;flex:1;}
      .hero{padding:74px 60px 48px;position:relative;color:#fff;
            background:linear-gradient(115deg,rgba(16,28,35,.92) 0%%,rgba(24,40,48,.66) 52%%,rgba(44,127,184,.40) 128%%),
                       url('melbourne-hero.jpg') center 42%% / cover no-repeat, #16232b;}
      .hero .eyebrow{font-size:12px;letter-spacing:.16em;text-transform:uppercase;color:#cfe6f7;margin-bottom:14px;}
      .hero h2{font-size:37px;line-height:1.12;margin:0 0 16px;max-width:820px;font-weight:700;
               text-shadow:0 1px 18px rgba(0,0,0,.35);}
      .hero p{font-size:16px;max-width:680px;color:#eaf2f7;margin:0;text-shadow:0 1px 10px rgba(0,0,0,.3);}
      .hero .stats{display:flex;gap:14px;margin-top:28px;flex-wrap:wrap;}
      .hero .stat{background:rgba(255,255,255,.13);border:1px solid rgba(255,255,255,.2);
           border-radius:13px;padding:14px 20px;min-width:130px;backdrop-filter:blur(7px);
           -webkit-backdrop-filter:blur(7px);}
      .hero .stat b{display:block;font-family:'Fraunces',Georgia,serif;font-size:24px;font-weight:700;
           line-height:1;letter-spacing:-0.01em;}
      .hero .stat span{font-size:10.5px;letter-spacing:.07em;text-transform:uppercase;color:rgba(255,255,255,.82);}
      .hero .credit{position:absolute;bottom:7px;right:14px;font-size:10px;color:rgba(255,255,255,.55);}
      .hero .credit a{color:rgba(255,255,255,.75);text-decoration:none;}
      .filters{display:flex;gap:26px;align-items:center;flex-wrap:wrap;background:#fff;
           padding:14px 60px;border-bottom:1px solid #e4e0d7;position:sticky;top:0;z-index:60;
           box-shadow:0 1px 6px rgba(0,0,0,.05);}
      .filters .lab{font-size:10.5px;letter-spacing:.08em;text-transform:uppercase;color:var(--grey);margin-bottom:2px;}
      .filters .hint{font-size:12px;color:#46555d;max-width:330px;}
      .filters .selectize-input,.filters .form-control{min-height:34px;}
      .section{padding:52px 60px;border-bottom:1px solid #e8e4db;scroll-margin-top:84px;
               background:linear-gradient(180deg, rgba(0,0,0,0) 0%%, var(--wash, transparent) 22%%, rgba(0,0,0,0) 75%%) no-repeat;}
      .shead{display:flex;gap:15px;align-items:flex-start;margin-bottom:14px;}
      .shead .chip{flex:none;width:40px;height:40px;border-radius:11px;background:var(--accent,var(--warm));
           color:#fff;display:flex;align-items:center;justify-content:center;font-size:18px;margin-top:3px;
           box-shadow:0 3px 9px rgba(0,0,0,.16);}
      .shead .stext{min-width:0;}
      .sectnum{font-size:12px;letter-spacing:.12em;color:var(--accent,var(--warm));font-weight:700;text-transform:uppercase;}
      .section h3{font-size:25px;margin:4px 0 4px;font-weight:800;}
      .section .lede{font-size:15px;color:#46555d;max-width:700px;margin:0 0 20px;}
      .insight{background:#fff;border-left:4px solid var(--green);padding:13px 18px;margin-top:18px;
           font-size:14px;border-radius:0 8px 8px 0;box-shadow:0 1px 3px rgba(0,0,0,.05);}
      .insight b{color:var(--ink);}
      .twocol{display:grid;grid-template-columns:1fr 1fr;gap:24px;align-items:start;}
      .panel{background:#fff;border:1px solid #e8e4db;border-radius:12px;padding:14px;
             box-shadow:0 1px 4px rgba(20,35,43,.05);}
      .panel .ptitle{font-size:13px;font-weight:700;margin:2px 4px 8px;display:flex;
           justify-content:space-between;align-items:center;gap:8px;}
      .toggle-row{margin:0 2px 6px;}
      .toggle-row .radio-inline,.toggle-row label{font-size:12.5px;}
      .takeaways{display:grid;grid-template-columns:repeat(4,1fr);gap:16px;margin-top:8px;}
      .tk{background:#fff;border:1px solid #e8e4db;border-radius:12px;padding:18px;border-top:3px solid var(--accent,var(--warm));box-shadow:0 1px 4px rgba(20,35,43,.05);}
      .tk .tkhead{display:flex;align-items:center;gap:10px;}
      .tk .tkic{flex:none;width:30px;height:30px;border-radius:8px;background:var(--accent,var(--warm));
           color:#fff;display:flex;align-items:center;justify-content:center;font-size:14px;}
      .tk .big{font-size:25px;font-weight:800;color:var(--accent,var(--warm));}
      .tk .lab{font-size:12.5px;color:#46555d;margin-top:7px;line-height:1.42;}
      .detailcard{background:#fbfaf7;border:1px solid #e8e4db;border-radius:9px;padding:12px 14px;font-size:13px;}
      .detailcard h4{margin:0 0 8px;font-size:14px;}
      .detailcard .row{display:flex;justify-content:space-between;padding:4px 0;border-bottom:1px dashed #eee;}
      .detailcard .row b{font-variant-numeric:tabular-nums;}
      .cmpwrap{display:flex;gap:12px;margin-top:12px;flex-wrap:wrap;}
      .cmpwrap .detailcard{flex:1;min-width:150px;}
      .hintpill{font-size:11px;color:var(--grey);font-weight:500;}
      .foot{padding:28px 60px 56px;font-size:12px;color:var(--grey);}
      .foot a{color:var(--cool);}
      @media (prefers-reduced-motion: reduce){*{scroll-behavior:auto !important;}}
      @media (max-width:900px){
        .nav{position:static;width:100%%;height:auto;}
        .main{margin-left:0;}
        .twocol,.takeaways{grid-template-columns:1fr;}
        .hero,.section,.filters,.foot{padding-left:22px;padding-right:22px;}
      }
    ", PAL$ink, PAL$paper, PAL$cool, PAL$warm, PAL$green, PAL$gold, PAL$grey))),
    
    # Client-side scroll helpers: smooth-scroll to a section, highlight the active
    # nav item as the reader scrolls (scroll-spy), advance the progress bar, and
    # make nav items keyboard-activatable with Enter/Space.
    tags$script(HTML("
      function goTo(id){var el=document.getElementById(id);
        if(el){el.scrollIntoView({behavior:'smooth',block:'start'});}}
      document.addEventListener('DOMContentLoaded',function(){
        var secs=['sec1','sec2','sec3','sec4','sec5'];
        function upd(){
          var cur=secs[0];
          secs.forEach(function(s){var el=document.getElementById(s);
            if(el && el.getBoundingClientRect().top<170){cur=s;}});
          document.querySelectorAll('.navitem').forEach(function(n){
            n.classList.toggle('active', n.getAttribute('data-target')===cur);});
          var h=document.body.scrollHeight-window.innerHeight;
          var pct=h>0?Math.min(100,Math.max(0,window.scrollY/h*100)):0;
          var bar=document.getElementById('progfill'); if(bar){bar.style.width=pct+'%';}
        }
        window.addEventListener('scroll',upd); upd();
        document.querySelectorAll('.navitem').forEach(function(n){
          n.addEventListener('keydown',function(e){
            if(e.key==='Enter'||e.key===' '){e.preventDefault();goTo(n.getAttribute('data-target'));}});
        });
      });
    "))
  ),
  div(class = "layout",
      #Sidebar: navigation, tools, progress, identity ---------------
      tags$nav(class = "nav",
               tags$h1("Urban Heat, Air Quality & Green Cover"),
               div(class = "sub", "Energy demand across Melbourne \u00b7 a guided data story"),
               # One nav item per story stage, built from SECTIONS.
               lapply(SECTIONS, function(s){
                 div(class = "navitem", `data-target` = s$id, tabindex = "0",
                     role = "button", `aria-label` = paste("Go to", s$title),
                     style = sprintf("--nav-accent:%s;", s$col),
                     onclick = sprintf("goTo('%s')", s$id),
                     span(class = "n", s$num),
                     span(class = "ic", icon(s$icon)),
                     span(class = "t", s$title))
               }),
               # Re-openable help and data-source dialogs.
               div(class = "tools",
                   tags$button(class = "btn", onclick = "Shiny.setInputValue('show_how', Math.random())",
                               HTML("&#9432;&nbsp; How to use this")),
                   tags$button(class = "btn", onclick = "Shiny.setInputValue('show_about', Math.random())",
                               HTML("&#9783;&nbsp; About the data"))),
               div(class = "prog", "Your progress",
                   div(class = "progbar", tags$i(id = "progfill"))),
               div(class = "datafoot",
                   div(class = "df-rule"),
                   div(class = "df-name", "Aasavari Pande"),
                   div(class = "df-meta", "Semester 1, 2026 \u00b7 City of Melbourne Open Data"))
      ),
      div(class = "main",
          #HERO: title, framing question and dataset summary cards
          div(class = "hero",
              tags$h2("Do greener parts of Melbourne stay cooler, cleaner \u2014 and use less energy?"),
              tags$p(paste("A guided tour through three real datasets, moving from city-wide patterns over",
                           "time to local sensor environments and finally to precinct-level energy demand.")),
              div(class = "stats",
                  div(class = "stat", tags$b("589,630"), tags$span("sensor readings")),
                  div(class = "stat", tags$b("82,010"),  tags$span("urban trees")),
                  div(class = "stat", tags$b("13,587"),  tags$span("energy properties")),
                  div(class = "stat", tags$b("20"),      tags$span("precincts compared"))),
              div(class = "credit",
                  HTML("Photo: Robert Stokoe / <a href='https://www.pexels.com' target='_blank' rel='noopener'>Pexels</a>"))),
          
          # STICKY GLOBAL CONTROLS: season filter + reset
          # The season filter is the one "global" control; it drives Sections 2 and 3.
          div(class = "filters",
              div(div(class = "lab", "Season filter"),
                  selectInput("season", NULL,
                              choices = c("All seasons", season_levels),
                              selected = "All seasons", width = "165px")),
              div(class = "hint", HTML("&#8592; one control, many views: <b>season</b> updates the
                  air-quality chart (Section 2) and the sensor map + scatter (Section 3) together.")),
              div(style = "margin-left:auto;",
                  actionButton("reset_all", "Reset all", icon = icon("rotate-left"),
                               class = "btn", style = "font-size:12px;"))),
          
          # SECTION 1: HEAT 
          # Hour x month heatmap (left) linked to a 24-hour profile (right).
          div(id = "sec1", class = "section", style = "--accent:#d7472b;--wash:rgba(215,71,43,.26);",
              div(class = "shead",
                  span(class = "chip", icon("temperature-high")),
                  div(class = "stext",
                      div(class = "sectnum", "01 \u2014 When is the city hottest?"),
                      tags$h3("Heat over time"))),
              p(
                class = "lede",
                HTML(paste(
                  "Every hour of the day across the calendar year. The grid shows mean",
                  "air temperature; <b>pick a location</b> to compare individual sensors",
                  "against the city average, then <b>click any month</b> to trace its 24-hour rhythm against the",
                  "yearly average. The season filter highlights that season's months."
                ))
              ),
              div(style = "display:flex;gap:26px;align-items:flex-end;flex-wrap:wrap;",
                  div(div(class = "lab", style = "font-size:10.5px;letter-spacing:.08em;text-transform:uppercase;color:#76858d;margin-bottom:2px;", "Location"),
                      selectInput("loc", NULL, choices = loc_choices, selected = "ALL", width = "220px"))),
              div(class = "twocol",
                  div(class = "panel",
                      div(class = "ptitle", span("Hour \u00d7 month grid \u2014 click a month to drill in")),
                      plotlyOutput("heatmap", height = "430px"),
                      uiOutput("heat_coverage")),
                  div(class = "panel",
                      div(class = "ptitle", span(textOutput("diurnal_title", inline = TRUE)),
                          actionButton("clear_month", "Clear", class = "btn",
                                       style = "font-size:11px;padding:2px 8px;border:1px solid #e0ddd4;border-radius:6px;")),
                      plotlyOutput("diurnal", height = "388px"),
                      uiOutput("month_readout"))),
              div(class = "insight", uiOutput("heat_insight"))),
          
          # SECTION 2: AIR QUALITY
          # Toggle between the seasonal PM2.5 distribution and the monthly trend.
          div(id = "sec2", class = "section", style = "--accent:#e0a528;--wash:rgba(224,165,40,.30);",
              div(class = "shead",
                  span(class = "chip", icon("wind")),
                  div(class = "stext",
                      div(class = "sectnum", "02 \u2014 When is the air worst?"),
                      tags$h3("Air quality over time"))),
              p(class = "lede", HTML(paste("PM2.5 fine particles. <b>Switch the view</b> between the seasonal",
                                           "distribution and the month-by-month trend. The season filter above focuses whichever season",
                                           "you choose."))),
              div(class = "panel",
                  div(class = "ptitle",
                      span(textOutput("air_title", inline = TRUE)),
                      div(class = "toggle-row", style = "margin:0;",
                          radioButtons("pm_view", NULL,
                                       choices = c("Seasonal distribution" = "box", "Change over time" = "line"),
                                       selected = "box", inline = TRUE))),
                  plotlyOutput("air", height = "400px")),
              div(class = "insight", uiOutput("air_insight"))),
          
          # SECTION 3: TREES & MICROCLIMATE (LINKED VIEWS) 
          # The story's core: a Leaflet map and a Plotly scatter linked in both
          # directions, plus a temperature/PM2.5 outcome toggle.
          div(id = "sec3", class = "section", style = "--accent:#2a8a5d;--wash:rgba(42,138,93,.26);",
              div(class = "shead",
                  span(class = "chip", icon("tree")),
                  div(class = "stext",
                      div(class = "sectnum", "03 \u2014 Do greener locations tend to be cooler?"),
                      tags$h3("Trees & microclimate"))),
              p(class = "lede", HTML(paste("The heart of the story, and a genuinely linked pair of views.",
                                           "<b>Click a sensor on the map</b> to highlight it in the scatter \u2014 or click a scatter point",
                                           "to highlight it back on the map and zoom to it. Marker colour is mean temperature; marker",
                                           "size is the count of trees recorded near each sensor (a proxy for local green cover), and the",
                                           "<b>green shading</b> underneath is the binned density of Melbourne's 82,010 recorded street and",
                                           "park trees. Use the <b>y-axis toggle</b> to test trees against either temperature or air quality."))),
              div(class = "twocol",
                  div(class = "panel",
                      div(class = "ptitle", span("Sensor map \u2014 size = nearby trees, colour = mean temp"),
                          div(style = "display:flex;gap:12px;align-items:center;",
                              div(class = "toggle-row", style = "margin:0;",
                                  checkboxInput("show_trees", "Tree density", value = TRUE)),
                              actionButton("reset_sel", "Clear", class = "btn",
                                           style = "font-size:11px;padding:2px 8px;border:1px solid #e0ddd4;border-radius:6px;"))),
                      leafletOutput("map", height = "440px"),
                      uiOutput("sensor_card")),
                  div(class = "panel",
                      div(class = "ptitle", span("Nearby tree count vs\u2026"),
                          div(class = "toggle-row", style = "margin:0;",
                              radioButtons("scatter_y", NULL,
                                           choices = c("Temperature" = "mean_temp_c", "PM2.5" = "mean_pm25"),
                                           selected = "mean_temp_c", inline = TRUE))),
                      plotlyOutput("scatter", height = "430px"))),
              div(class = "insight", uiOutput("scatter_insight"))),
          
          # SECTION 4: ENERGY 
          # Left: stacked energy by building type with a clickable year + KPI cards.
          # Right: driver scatter (fixed to 2026) with a precinct comparison.
          div(id = "sec4", class = "section", style = "--accent:#2c7fb8;--wash:rgba(44,127,184,.26);",
              div(class = "shead",
                  span(class = "chip", icon("bolt")),
                  div(class = "stext",
                      div(class = "sectnum", "04 \u2014 What is energy demand associated with?"),
                      tags$h3("Energy demand"))),
              p(class = "lede", HTML(paste(
                "Explore four <b>modelled snapshots</b> of energy demand. On the left, switch between total",
                "energy and percentage share, then <b>select a year or click the chart</b> to update the KPI cards.",
                "On the right, the driver analysis remains fixed to <b>2026</b>: toggle commercial building share, tree",
                "count or mean temperature, then <b>click precincts</b> to compare up to three locations."
              ))),
              div(class = "twocol",
                  div(class = "panel",
                      div(class = "ptitle", span(textOutput("area_title", inline = TRUE)),
                          div(class = "toggle-row", style = "margin:0;",
                              radioButtons("area_view", NULL,
                                           choices = c("Total (GJ)" = "total", "Share (%)" = "share"),
                                           selected = "total", inline = TRUE))),
                      div(class = "toggle-row",
                          radioButtons("energy_focus_year", "Explore modelled year",
                                       choices = energy_years, selected = 2026, inline = TRUE)),
                      plotlyOutput("area", height = "340px"),
                      uiOutput("energy_year_cards"),
                      div(class = "hintpill", style = "margin:10px 3px 2px;",
                          "Modelled snapshots, not annual meter readings. KPI totals use the 2026-active property cohort.")),
                  div(class = "panel",
                      div(class = "ptitle", span(textOutput("driver_title", inline = TRUE)),
                          div(class = "toggle-row", style = "margin:0;display:flex;gap:14px;align-items:center;",
                              radioButtons("driver_x", NULL,
                                           choices = c("Commercial building share" = "pct_commercial",
                                                       "Tree count" = "n_trees",
                                                       "Mean temperature" = "mean_temp_c"),
                                           selected = "pct_commercial", inline = TRUE))),
                      plotlyOutput("driver", height = "300px"))),
              # Precinct comparison: click points above (or type) to add up to three.
              div(style = "margin-top:14px;",
                  div(style = "display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;",
                      div(class = "hintpill",
                          "Click a precinct to add it; selected precincts appear dark. Click it again to remove it. Compare up to 3."),
                      actionButton("clear_cmp", "Clear comparison", icon = icon("xmark"),
                                   class = "btn",
                                   style = "font-size:11px;padding:4px 9px;border:1px solid #e0ddd4;border-radius:6px;")),
                  selectizeInput("cmp", NULL, choices = sort(precinct_energy$precinct),
                                 multiple = TRUE, width = "100%",
                                 options = list(maxItems = 3, placeholder = "Add precincts to compare\u2026")),
                  uiOutput("compare_cards")),
              div(class = "insight", uiOutput("energy_insight"))),
          
          # SECTION 5: KEY TAKEAWAYS 
          # Four data-computed summary cards plus the closing statement.
          div(id = "sec5", class = "section", style = "--accent:#16232b;--wash:rgba(22,35,43,.14);",
              div(class = "shead",
                  span(class = "chip", icon("star")),
                  div(class = "stext",
                      div(class = "sectnum", "05 \u2014 The story in four numbers"),
                      tags$h3("Key takeaways"))),
              p(class = "lede", HTML("Each of the project's questions in one number, computed from the data.
                  The tree\u2013temperature card updates with the season filter; the selected-year energy KPIs appear in Section 4.")),
              uiOutput("takeaways"),
              div(class = "insight", style = "border-left-color:var(--warm);",
                  HTML("<b>Bottom line.</b> Sensor locations with more nearby trees tended to be cooler.
                  At precinct level, commercial building composition was much more strongly associated with
                  modelled energy demand than tree count. Across properties with non-zero modelled energy in 2026,
                  total modelled consumption rose by about 47% from 2011 to 2026."))),
          
          # FOOTER: DATA ATTRIBUTION
          div(class = "foot",
              HTML(paste0("Data: City of Melbourne Open Data Portal \u2014 ",
                          "<a href='https://discover.data.vic.gov.au/dataset/microclimate-sensors-data' target='_blank' rel='noopener'>Microclimate Sensors</a>, ",
                          "<a href='https://data.melbourne.vic.gov.au/explore/dataset/trees-with-species-and-dimensions-urban-forest/export/' target='_blank' rel='noopener'>Urban Forest</a>, ",
                          "<a href='https://data.melbourne.vic.gov.au/explore/dataset/property-level-energy-consumption-modelled-on-building-attributes-baseline-2011-/export/' target='_blank' rel='noopener'>Property-Level Energy (Modelled)</a>. ",
                          "Built with R Shiny, Leaflet and Plotly. FIT5147 DVP \u00b7 Aasavari Pande \u00b7 35595353.")))
      )
  )
)

# 4. SERVER 
server <- function(input, output, session) {
  
  sel <- reactiveVal(NULL)   # currently selected sensor (short_name) shared by the map + scatter
  
  
  # First-load overlay introducing the project, the three questions, the datasets
  # and the key interactions. Re-openable from the sidebar "How to use" button.
  welcome <- function() modalDialog(
    title = NULL, easyClose = TRUE, size = "l",
    footer = modalButton("Start exploring \u2192"),
    HTML("<div style='position:relative;margin:-15px;border-radius:6px;overflow:hidden;min-height:540px;'>
      <video autoplay muted loop playsinline poster='melbourne-hero-poster.jpg'
             onloadedmetadata='this.muted=true;var p=this.play();if(p)p.catch(function(){});'
             style='position:absolute;top:0;left:0;width:100%;height:100%;object-fit:cover;z-index:0;'>
        <source src='melbourne-hero.mp4' type='video/mp4'></video>
      <div style='position:absolute;top:0;left:0;width:100%;height:100%;z-index:1;
           background:linear-gradient(158deg,rgba(11,20,26,.72) 0%,rgba(11,20,26,.88) 70%);'></div>
      <div style='position:relative;z-index:2;padding:30px 34px;line-height:1.6;color:#dbe3e7;'>
        <h3 style='margin:6px 0 2px;font-size:23px;color:#fff;'>Urban Heat, Air Quality &amp; Green Cover in Melbourne</h3>
        <p style='margin:0 0 14px;color:#cdd8dd;'>A guided data story for Melbourne residents and urban-planning
          stakeholders, asking whether greener parts of the city stay cooler, cleaner, and use less energy.</p>
        <p style='margin:0 0 4px;color:#fff;'><b>Three questions guide the story:</b></p>
        <ol style='margin:0 0 14px;padding-left:20px;'>
          <li>How do temperature and air quality vary across locations and over time?</li>
          <li>Does urban green cover relate to local microclimate and air quality?</li>
          <li>Do green cover and microclimate relate to building energy demand?</li></ol>
        <p style='margin:0 0 4px;color:#fff;'><b>Built from three real datasets:</b>
          <span style='color:#cdd8dd;'>12 microclimate sensors (589,630 readings), 82,010 street and park
          trees, and modelled energy for 13,587 properties.</span></p>
        <hr style='border:none;border-top:1px solid rgba(255,255,255,.2);margin:14px 0;'>
        <p style='margin:0 0 4px;color:#fff;'><b>How to explore:</b></p>
        <ul style='margin:0;padding-left:20px;'>
          <li>Move through the five steps with the <b>sidebar</b>, or just scroll.</li>
          <li>The <b>season filter</b> (top) updates the air-quality chart and the sensor map + scatter together.</li>
          <li><b>Section 1</b> \u2014 pick a location and click any month to trace its 24-hour temperature rhythm.</li>
          <li><b>Section 3</b> \u2014 click a sensor on the map to highlight it in the scatter, and vice-versa.</li>
          <li><b>Section 4</b> \u2014 select or click a modelled year for live KPIs, toggle the 2026 driver,
              and click precincts to compare up to three.</li></ul>
        <div style='font-size:10px;color:#9fb0b7;margin-top:16px;text-align:right;'>Melbourne, Southbank \u00b7 author's own footage</div>
      </div>
    </div>"))
  showModal(welcome())
  observeEvent(input$show_how, { showModal(welcome()) })
  # "About the data" dialog: sources and the main cleaning/coverage caveats.
  observeEvent(input$show_about, {
    showModal(modalDialog(title = "About the data", easyClose = TRUE, footer = modalButton("Close"),
                          HTML("<p style='margin-top:0;'>Three datasets from the
        <a href='https://data.melbourne.vic.gov.au' target='_blank' rel='noopener'>City of Melbourne Open
        Data Portal</a>, cleaned and integrated in R.</p>
        <ul style='line-height:1.7;padding-left:18px;'>
          <li><b>Microclimate Sensors</b> \u2014 589,630 cleaned 15-minute readings, 12 fixed sensors
              (May 2024\u2013Mar 2026). Times converted to Melbourne local; PM2.5 spikes above 500 \u00b5g/m\u00b3
              removed as sensor malfunction.</li>
          <li><b>Urban Forest</b> \u2014 82,010 tree records; tree counts recorded near each sensor (within roughly 500 m).</li>
          <li><b>Property-Level Energy (Modelled)</b> \u2014 13,587 properties at 2011, 2016, 2021, 2026,
              aggregated to 20 precincts.</li></ul>
        <p>Sensor <i>ICTMicroclimate-10</i> is held out of the Section 3 analysis (part-year record).
        Relationships shown are correlational, not causal.</p>")))
  })
  
  # Season-aware sensor table feeding the map + scatter: the all-seasons summary
  # when no season is chosen, otherwise the per-season table. Drops the part-year sensor.
  sens_view <- reactive({
    d <- if (input$season == "All seasons") sensors else dplyr::filter(sensor_season, season == input$season)
    d %>% filter(device_id != EXCLUDE_SENSOR, !is.na(mean_temp_c))
  })
  
  # Reset controls
  # "Reset all" returns every control to its default and recentres the map.
  observeEvent(input$reset_all, {
    updateSelectInput(session, "season", selected = "All seasons")
    updateSelectInput(session, "loc", selected = "ALL")
    updateRadioButtons(session, "pm_view", selected = "box")
    updateRadioButtons(session, "scatter_y", selected = "mean_temp_c")
    updateRadioButtons(session, "driver_x", selected = "pct_commercial")
    updateRadioButtons(session, "energy_focus_year", selected = 2026)
    updateSelectizeInput(session, "cmp", selected = character(0))
    sel(NULL)
    sel_month(NULL)
    leafletProxy("map") %>%
      clearGroup("hl") %>%
      setView(lng = 144.962, lat = -37.812, zoom = 13)
  })
  # "Clear" (Section 3) just drops the current sensor selection and recentres.
  observeEvent(input$reset_sel, {
    sel(NULL)
    leafletProxy("map") %>%
      clearGroup("hl") %>%
      setView(lng = 144.962, lat = -37.812, zoom = 13)
  })
  
  # SECTION 1: hour x month heatmap LINKED to a 24-hour profile 
  sel_month <- reactiveVal(NULL)                 # month selected by clicking the grid or a line
  observeEvent(input$clear_month, { sel_month(NULL) })
  
  # Section 1 data source: the pooled all-sensor grid, or one sensor's record.
  # For a single sensor, months with too few hours to be representative (sensor
  # outages) are dropped so a lone hour-cell doesn't masquerade as a whole month;
  # they then appear as honest blank rows.
  MIN_HOURS_PER_MONTH <- 6
  heat_data <- reactive({
    if (is.null(input$loc) || input$loc == "ALL") return(heat_metrics)
    d <- sensor_hourly %>% filter(device_id == input$loc) %>%
      select(hour, month_num, month_lbl, mean_temp_c, mean_pm25, mean_humidity)
    keep <- d %>% count(month_lbl) %>% filter(n >= MIN_HOURS_PER_MONTH) %>% pull(month_lbl)
    d %>% filter(month_lbl %in% keep)
  })
  loc_label <- reactive({
    if (is.null(input$loc) || input$loc == "ALL") "all sensors" else names(loc_choices)[loc_choices == input$loc]
  })
  
  # Heatmap: complete() fills the full 24x12 grid so missing cells stay blank
  # rather than collapsing the layout; y-axis reversed to put January at the top.
  output$heatmap <- renderPlotly({
    metric <- "mean_temp_c"; md <- metric_meta[[metric]]
    z <- heat_data() %>% mutate(val = .data[[metric]]) %>%
      select(hour, month_lbl, val) %>%
      tidyr::complete(hour = 0:23, month_lbl = month_levels) %>%
      tidyr::pivot_wider(names_from = hour, values_from = val) %>%
      arrange(match(month_lbl, month_levels))
    zmat <- as.matrix(z[, as.character(0:23)])
    cs <- list(list(0, md$cols[1]), list(0.5, md$cols[2]), list(1, md$cols[3]))
    plot_ly(x = 0:23, y = month_levels, z = zmat, type = "heatmap",
            colorscale = cs, xgap = 1, ygap = 1, source = "heat",
            colorbar = list(title = md$unit, len = 0.92),
            hovertemplate = paste0("%{y}, %{x}:00<br>%{z:.1f} ", md$unit, "<extra></extra>")) %>%
      layout(xaxis = list(title = "Hour of day", tickvals = seq(0, 23, 3),
                          ticktext = paste0(seq(0, 23, 3), ":00"), zeroline = FALSE),
             yaxis = list(title = "", autorange = "reversed")) %>%
      transparent() %>% config(displayModeBar = FALSE)
  })
  
  output$diurnal_title <- renderText({
    md <- metric_meta[["mean_temp_c"]]
    if (!is.null(sel_month())) sprintf("%s \u2014 24-hour profile (%s)", sel_month(), md$lab)
    else sprintf("24-hour profile by month (%s)", md$lab)
  })
  
  # Diurnal profile: one faint line per month, with emphasis applied by context
  # (the clicked month, or the months of the filtered season). The dotted line
  # is the pooled annual mean for reference.
  output$diurnal <- renderPlotly({
    metric <- "mean_temp_c"; md <- metric_meta[[metric]]
    d   <- heat_data() %>% mutate(val = .data[[metric]]) %>% filter(!is.na(val))
    # Guard: if a location has no readings at all, show a framed empty plot
    # instead of letting Plotly error on missing x/y.
    if (nrow(d) == 0)
      return(plot_ly(source = "diurnal") %>%
               layout(xaxis = list(title = "Hour of day", range = c(0, 23)),
                      yaxis = list(title = sprintf("%s (%s)", md$lab, md$unit)),
                      annotations = list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                         text = "No readings for this location.",
                                         showarrow = FALSE, font = list(color = PAL$grey))) %>%
               transparent() %>% config(displayModeBar = FALSE))
    ann <- d %>% group_by(hour) %>% summarise(val = mean(val, na.rm = TRUE), .groups = "drop")
    ssel <- input$season
    p <- plot_ly(source = "diurnal")
    for (mn in month_levels) {
      dm <- d %>% filter(month_lbl == mn)
      if (nrow(dm) == 0) next                       # skip months this location never recorded
      is_sel    <- !is.null(sel_month()) && sel_month() == mn
      in_season <- ssel != "All seasons" && month_season[[mn]] == ssel
      # Emphasis priority: clicked month > season-filtered months > neutral.
      if (is_sel)                         { col <- PAL$warm;            w <- 3;   op <- 1 }
      else if (!is.null(sel_month()))     { col <- "#cbd4d9";          w <- 1;   op <- 0.45 }
      else if (ssel != "All seasons" && in_season)  { col <- season_cols[[ssel]]; w <- 2.4; op <- 1 }
      else if (ssel != "All seasons")     { col <- "#d6dde1";          w <- 1;   op <- 0.4 }
      else                                { col <- "#aab6bd";          w <- 1;   op <- 0.6 }
      p <- add_lines(p, x = dm$hour, y = dm$val, name = mn, key = mn,
                     line = list(color = col, width = w), opacity = op, hoverinfo = "text",
                     text = sprintf("%s \u00b7 %02d:00<br>%.1f %s", mn, dm$hour, dm$val, md$unit),
                     showlegend = FALSE)
    }
    p %>% add_lines(x = ann$hour, y = ann$val, name = "Annual mean",
                    line = list(color = PAL$ink, width = 2, dash = "dot"), hoverinfo = "text",
                    text = sprintf("Annual mean \u00b7 %02d:00<br>%.1f %s", ann$hour, ann$val, md$unit)) %>%
      layout(xaxis = list(title = "Hour of day", tickvals = seq(0, 23, 3),
                          ticktext = paste0(seq(0, 23, 3), ":00")),
             yaxis = list(title = sprintf("%s (%s)", md$lab, md$unit)),
             legend = list(orientation = "h", x = 0, y = 1.12)) %>%
      transparent() %>% config(displayModeBar = FALSE)
  })
  
  # Month selection is bidirectional: clicking a heatmap cell or a profile line
  # both set sel_month().
  observeEvent(event_data("plotly_click", source = "heat"), {
    ev <- event_data("plotly_click", source = "heat")
    if (!is.null(ev$y)) sel_month(as.character(ev$y))
  })
  observeEvent(event_data("plotly_click", source = "diurnal"), {
    ev <- event_data("plotly_click", source = "diurnal")
    if (!is.null(ev$key)) sel_month(as.character(ev$key))
  })
  
  # Detail card for the selected month: peak, low and overnight temperature.
  output$month_readout <- renderUI({
    md <- metric_meta[["mean_temp_c"]]
    if (is.null(sel_month()))
      return(div(class = "detailcard", style = "color:#76858d;margin-top:10px;",
                 "Click a month on the grid (or a line) to read its peak hour and overnight value."))
    d  <- heat_data() %>% filter(month_lbl == sel_month()) %>% mutate(val = .data[["mean_temp_c"]]) %>% filter(!is.na(val))
    if (nrow(d) == 0)
      return(div(class = "detailcard", style = "color:#76858d;margin-top:10px;",
                 sprintf("%s has no readings for %s at this location.", sel_month(), md$lab)))
    pk <- d %>% slice_max(val, n = 1, with_ties = FALSE)
    tr <- d %>% slice_min(val, n = 1, with_ties = FALSE)
    midv <- d %>% filter(hour == 0) %>% pull(val)
    div(class = "detailcard", style = "margin-top:10px;",
        tags$h4(sprintf("%s \u2014 %s \u00b7 %s", sel_month(), md$lab, loc_label())),
        div(class = "row", span("Daily peak"), tags$b(sprintf("%.1f %s at %02d:00", pk$val, md$unit, pk$hour))),
        div(class = "row", span("Daily low"),  tags$b(sprintf("%.1f %s at %02d:00", tr$val, md$unit, tr$hour))),
        div(class = "row", style = "border:none;", span("Overnight (00:00)"),
            tags$b(if (length(midv) == 0) "\u2013" else sprintf("%.1f %s", midv, md$unit))))
  })
  
  # warn when a single sensor recorded fewer than 12 months.
  output$heat_coverage <- renderUI({
    if (is.null(input$loc) || input$loc == "ALL") return(NULL)
    present <- sensor_hourly %>% filter(device_id == input$loc) %>%
      count(month_lbl) %>% filter(n >= MIN_HOURS_PER_MONTH) %>% nrow()
    if (present >= 12) return(NULL)
    div(style = "font-size:11.5px;color:#76858d;margin:8px 4px 0;",
        sprintf("%s recorded data for %d of 12 months. Blank rows are periods this sensor was offline \u2014 no values are estimated.",
                loc_label(), present))
  })
  
  output$heat_insight <- renderUI({
    HTML("<b>Insight.</b> Summer afternoons (15:00\u201316:00) are hottest \u2014 January peaks at
      25.6\u00a0\u00b0C \u2014 while winter mornings are coldest (June dawn ~9\u00a0\u00b0C). Summer nights stay warm
      (January midnight ~19.6\u00a0\u00b0C, vs 10.0\u00a0\u00b0C in June), showing that seasonal temperature differences continue overnight.")
  })
  
  # SECTION 2: PM2.5 distribution OR trend
  output$air_title <- renderText({
    if (input$pm_view == "box") "PM2.5 by season (\u00b5g/m\u00b3, extreme values cropped)"
    else "Monthly mean PM2.5 over time (\u00b5g/m\u00b3)"
  })
  # Two views share the panel: a per-season boxplot, or a monthly trend with
  # season-coloured markers. The global season filter dims the non-selected seasons.
  output$air <- renderPlotly({
    foc <- input$season
    if (input$pm_view == "box") {
      p <- plot_ly()
      for (s in season_levels) {
        ds <- pm25_daily %>% filter(season == s)
        op <- if (foc == "All seasons" || foc == s) 1 else 0.28
        p <- p %>% add_trace(y = ds$mean_pm25, name = s, type = "box", boxpoints = FALSE,
                             opacity = op, line = list(color = season_cols[[s]]),
                             fillcolor = season_cols[[s]], marker = list(color = season_cols[[s]]))
      }
      # y capped at 30 to keep the boxes readable; the title notes values are cropped.
      p %>% layout(showlegend = FALSE, yaxis = list(title = "PM2.5 (\u00b5g/m\u00b3)", range = c(0, 30)),
                   xaxis = list(title = "")) %>% transparent() %>% config(displayModeBar = FALSE)
    } else {
      m <- pm25_monthly                              # already ordered by time
      p <- plot_ly() %>%
        add_lines(x = m$t, y = m$pm25, line = list(color = PAL$grey, width = 1.5),
                  hoverinfo = "skip", showlegend = FALSE)
      for (s in season_levels) {                     # season-coloured markers on top of the grey line
        ms <- m %>% filter(season == s)
        op <- if (foc == "All seasons" || foc == s) 1 else 0.3
        sz <- if (foc != "All seasons" && foc == s) 12 else 8
        p <- add_markers(p, x = ms$t, y = ms$pm25, name = s, opacity = op,
                         marker = list(size = sz, color = season_cols[[s]],
                                       line = list(color = "white", width = 1)),
                         hoverinfo = "text",
                         text = sprintf("%s %d \u00b7 %s<br>%.1f \u00b5g/m\u00b3", ms$month_lbl, ms$year, s, ms$pm25),
                         showlegend = TRUE)
      }
      p %>% layout(
        yaxis = list(title = "PM2.5 (\u00b5g/m\u00b3)", rangemode = "tozero"),
        xaxis = list(title = ""),
        legend = list(orientation = "h", x = 0, y = 1.12)
      ) %>% transparent() %>% config(displayModeBar = FALSE)
    }
  })
  output$air_insight <- renderUI({
    HTML("<b>Insight.</b> PM2.5 was highest and most variable in <b>winter</b> and autumn. At the seasonal level, Melbourne\u2019s highest average temperatures and highest PM2.5 concentrations occurred at different times of the year.")
  })
  
  # SECTION 3: linked Leaflet map <-> Plotly scatter 
  # Base map drawn once: the tree-density wash sits underneath as soft context.
  output$map <- renderLeaflet({
    tg <- colorNumeric(c("#dcefdc", "#9bd0ad", "#2a8a5d"), domain = sqrt(tree_density$n_trees))
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.962, lat = -37.812, zoom = 13) %>%
      addCircleMarkers(data = tree_density, lng = ~lon, lat = ~lat, group = "trees",
                       radius = ~rescale(sqrt(n_trees), to = c(1.5, 6.5)), stroke = FALSE,
                       fillColor = ~tg(sqrt(n_trees)), fillOpacity = 0.3,
                       label = ~lapply(sprintf("%s trees nearby", format(n_trees, big.mark = ",")), HTML))
  })
  # Sensor markers are redrawn via a proxy whenever the season changes, so the
  # base map and tree layer are not rebuilt. Colour = temperature, size = trees.
  observe({
    d <- sens_view(); if (nrow(d) == 0) return()
    pal <- colorNumeric(c(PAL$cool, "#f0d28a", PAL$warm), domain = d$mean_temp_c)
    leafletProxy("map", data = d) %>%
      clearGroup("sensors") %>% removeControl("tlegend") %>%
      addCircleMarkers(lng = ~sensor_lon, lat = ~sensor_lat, layerId = ~short_name, group = "sensors",
                       radius = ~rescale(sqrt(tree_count_500m), to = c(8, 17)),
                       fillColor = ~pal(mean_temp_c), fillOpacity = 0.95, color = "#16232b", weight = 1.5,
                       label = ~lapply(sprintf("<b>%s</b><br>Nearby trees: %s<br>Mean temp: %.1f \u00b0C",
                                               short_name, format(tree_count_500m, big.mark = ","), mean_temp_c), HTML)) %>%
      addLegend("bottomright", layerId = "tlegend", pal = pal, values = ~mean_temp_c,
                title = "Mean \u00b0C", opacity = 0.9)
  })
  observeEvent(input$show_trees, {            # show/hide the green-cover layer
    proxy <- leafletProxy("map")
    if (isTRUE(input$show_trees)) showGroup(proxy, "trees") else hideGroup(proxy, "trees")
  }, ignoreInit = TRUE)
  
  # Scatter: tree count vs the chosen outcome, with a subordinate linear trend
  # guide and a live Spearman annotation. The selected sensor is enlarged/recoloured.
  output$scatter <- renderPlotly({
    yf <- input$scatter_y; md <- metric_meta[[yf]]
    d <- sens_view() %>% filter(!is.na(.data[[yf]]))
    d$yval <- d[[yf]]
    if (nrow(d) < 3) return(plotly_empty())
    d$is_sel <- if (is.null(sel())) FALSE else d$short_name == sel()
    fit <- lm(yval ~ tree_count_500m, data = d)
    xr  <- range(d$tree_count_500m)
    yline <- predict(fit, newdata = data.frame(tree_count_500m = xr))
    sp  <- suppressWarnings(cor.test(d$tree_count_500m, d$yval, method = "spearman"))
    scatter_p_txt <- if (is.na(sp$p.value)) {
      "p unavailable"
    } else if (sp$p.value < 0.001) {
      "p < 0.001"
    } else {
      sprintf("p = %.2f", sp$p.value)
    }
    plot_ly(source = "scatter") %>%
      add_lines(x = xr, y = yline, line = list(color = PAL$grey, dash = "dash", width = 1),
                hoverinfo = "skip", showlegend = FALSE) %>%
      add_markers(data = d, x = ~tree_count_500m, y = ~yval, key = ~short_name,
                  marker = list(size = ~ifelse(is_sel, 20, 12),
                                color = ~ifelse(is_sel, PAL$warm, PAL$green),
                                line = list(color = "white", width = 1.5)),
                  hoverinfo = "text",
                  text = ~sprintf("%s<br>%s nearby trees<br>%.1f %s",
                                  short_name, format(tree_count_500m, big.mark = ","), yval, md$unit),
                  showlegend = FALSE) %>%
      layout(xaxis = list(title = "Nearby tree count (proxy for green cover)"),
             yaxis = list(title = sprintf("Mean %s (%s)", tolower(md$lab), md$unit)),
             annotations = list(x = max(d$tree_count_500m), y = max(d$yval),
                                text = sprintf("Spearman r = %.2f  \u00b7  %s",
                                               unname(sp$estimate), scatter_p_txt),
                                showarrow = FALSE, xanchor = "right",
                                font = list(color = PAL$grey, size = 11))) %>%
      transparent() %>% config(displayModeBar = FALSE)
  })
  
  output$scatter_insight <- renderUI({
    yf <- input$scatter_y
    d <- sens_view() %>% filter(!is.na(.data[[yf]]))
    if (nrow(d) < 3) return(HTML("Insufficient data in this season to estimate a relationship."))
    r <- suppressWarnings(cor(d$tree_count_500m, d[[yf]], method = "spearman"))
    s <- if (input$season == "All seasons") "across the full record" else paste("in", tolower(input$season))
    metric <- if (yf == "mean_temp_c") "lower temperatures" else "lower PM2.5"
    HTML(sprintf("<b>Insight.</b> Sensors with more surrounding trees tend to record %s %s
      (Spearman r = %.2f). The dashed line shows the overall direction of the relationship.
      This is an observational association and does not establish causation.", metric, s, r))
  })
  
  # Bidirectional map <-> scatter linking
  # A map click or a scatter click both update the shared selection (sel());
  # the observer below then highlights the sensor on the map and flies to it.
  observeEvent(input$map_marker_click, { sel(input$map_marker_click$id) })
  observeEvent(event_data("plotly_click", source = "scatter"), {
    ev <- event_data("plotly_click", source = "scatter")
    if (!is.null(ev$key)) sel(ev$key)
  })
  observeEvent(sel(), {
    d <- sens_view(); s <- d %>% filter(short_name == sel())
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("hl")
    if (nrow(s) == 1) {
      proxy %>% addCircleMarkers(lng = s$sensor_lon, lat = s$sensor_lat, group = "hl",
                                 radius = rescale(sqrt(s$tree_count_500m), to = c(8, 17),
                                                  from = range(sqrt(d$tree_count_500m))) + 7,
                                 fillColor = "transparent", color = PAL$warm, weight = 3) %>%
        flyTo(lng = s$sensor_lon, lat = s$sensor_lat, zoom = 15)
    }
  }, ignoreNULL = TRUE)
  
  # Detail card for the selected sensor (PM2.5 reads "no sensor" for Royal Park).
  output$sensor_card <- renderUI({
    if (is.null(sel())) return(div(class = "detailcard", style = "color:#76858d;margin-top:10px;",
                                   "Click a sensor (map) or a point (scatter) to see its local profile."))
    s <- sens_view() %>% filter(short_name == sel())
    if (nrow(s) != 1) return(NULL)
    div(class = "detailcard", style = "margin-top:10px;",
        tags$h4(s$short_name),
        div(class = "row", span("Nearby trees"), tags$b(format(s$tree_count_500m, big.mark = ","))),
        div(class = "row", span("Mean temperature"), tags$b(sprintf("%.1f \u00b0C", s$mean_temp_c))),
        div(class = "row", style = "border:none;", span("Mean PM2.5"),
            tags$b(if (is.na(s$mean_pm25)) "no sensor" else sprintf("%.1f \u00b5g/m\u00b3", s$mean_pm25))))
  })
  
  # SECTION 4: energy
  output$area_title <- renderText({
    if (input$area_view == "share") "Composition by building type (% of total)"
    else "Total energy by building type (million GJ)"
  })
  # Stacked area by building type across the four modelled years. A dotted
  # vertical line marks the year currently selected for the KPI cards.
  output$area <- renderPlotly({
    share <- input$area_view == "share"
    selected_year <- as.numeric(input$energy_focus_year)
    d <- energy_growth
    
    d$txt <- if (share) {
      sprintf("%s \u2014 %d<br>%.1f%% of total<br>%.2f million GJ",
              d$building_type, d$year, d$share, d$total_gj_m)
    } else {
      sprintf("%s \u2014 %d<br>%.2f million GJ",
              d$building_type, d$year, d$total_gj_m)
    }
    
    fill_cols <- c(Residential = PAL$cool, Mixed = PAL$gold, Commercial = PAL$warm)
    
    plot_ly(d, x = ~year, y = ~total_gj_m, color = ~building_type, colors = fill_cols,
            type = "scatter", mode = "lines+markers", stackgroup = "one",
            groupnorm = if (share) "percent" else "", source = "area", key = ~year,
            hoverinfo = "text", text = ~txt,
            marker = list(size = 8), line = list(width = 1.5)) %>%
      layout(
        xaxis = list(title = "Modelled year \u2014 click a point to explore",
                     tickvals = energy_years),
        yaxis = list(title = if (share) "Share of total (%)" else "Total modelled energy (million GJ)"),
        legend = list(orientation = "h", x = 0, y = 1.16),
        hovermode = "x unified",
        shapes = list(list(
          type = "line", x0 = selected_year, x1 = selected_year,
          yref = "paper", y0 = 0, y1 = 1,
          line = list(color = PAL$ink, dash = "dot", width = 2)
        )),
        annotations = list(list(
          x = selected_year, y = 1, yref = "paper",
          text = paste("Selected:", selected_year),
          showarrow = FALSE, yshift = 10,
          font = list(color = PAL$ink, size = 11)
        ))
      ) %>%
      transparent() %>% config(displayModeBar = FALSE)
  })
  
  # Clicking the area chart snaps the selection to the nearest modelled year.
  observeEvent(event_data("plotly_click", source = "area"), {
    ev <- event_data("plotly_click", source = "area")
    if (!is.null(ev$x)) {
      selected_year <- energy_years[which.min(abs(energy_years - as.numeric(ev$x)))]
      updateRadioButtons(session, "energy_focus_year", selected = selected_year)
    }
  })
  
  # KPI cards for the selected year: total energy, commercial share, and growth
  # since 2011 (all derived from energy_growth so they stay in sync with the chart).
  output$energy_year_cards <- renderUI({
    selected_year <- as.numeric(input$energy_focus_year)
    selected_data <- energy_growth %>% filter(year == selected_year)
    
    total_energy <- sum(selected_data$total_gj_m, na.rm = TRUE)
    commercial_share <- selected_data %>%
      filter(as.character(building_type) == "Commercial") %>%
      pull(share)
    commercial_share <- if (length(commercial_share) == 0) NA_real_ else commercial_share[[1]]
    
    energy_2011 <- energy_growth %>%
      filter(year == 2011) %>%
      summarise(total = sum(total_gj_m, na.rm = TRUE)) %>%
      pull(total)
    
    growth_since_2011 <- (total_energy / energy_2011 - 1) * 100
    
    div(class = "cmpwrap",
        div(class = "detailcard",
            tags$h4(as.character(selected_year)),
            div(class = "row", style = "border:none;",
                span("Total modelled energy"),
                tags$b(sprintf("%.2f M GJ", total_energy)))),
        div(class = "detailcard",
            tags$h4("Composition"),
            div(class = "row", style = "border:none;",
                span("Commercial energy share"),
                tags$b(if (is.na(commercial_share)) "\u2013" else sprintf("%.1f%%", commercial_share)))),
        div(class = "detailcard",
            tags$h4("Change"),
            div(class = "row", style = "border:none;",
                span("Growth since 2011"),
                tags$b(sprintf("%+.1f%%", growth_since_2011)))))
  })
  
  output$driver_title <- renderText({
    lab <- switch(input$driver_x, pct_commercial = "Commercial building share",
                  n_trees = "Tree count", mean_temp_c = "Mean temperature")
    sprintf("%s vs energy per property (2026)", lab)
  })
  # Driver scatter (fixed to 2026): energy per property against the chosen driver,
  # with a live Spearman annotation. Temperature is only available for the
  # sensor-covered precincts, which the axis label flags.
  output$driver <- renderPlotly({
    xf <- input$driver_x
    ycol <- "mean_energy_2026"
    xmeta <- switch(xf,
                    pct_commercial = list(lab = "Commercial building share (%)", fmt = function(v) sprintf("%.0f%% commercial", v)),
                    n_trees        = list(lab = "Tree count in precinct",        fmt = function(v) paste0(format(round(v), big.mark = ","), " trees")),
                    mean_temp_c    = list(lab = "Mean temperature (\u00b0C)",     fmt = function(v) sprintf("%.1f \u00b0C", v)))
    d <- precinct_energy %>% filter(!is.na(.data[[ycol]]), !is.na(.data[[xf]]))
    d$xval <- d[[xf]]; d$yval <- d[[ycol]]; d$is_sel <- d$precinct %in% input$cmp
    sp <- suppressWarnings(cor.test(d$xval, d$yval, method = "spearman"))
    p_txt <- if (is.na(sp$p.value)) {
      "p unavailable"
    } else if (sp$p.value < 0.001) {
      "p &lt; 0.001"
    } else {
      sprintf("p = %.3f", sp$p.value)
    }
    note <- sprintf("Spearman r = %.2f, %s", unname(sp$estimate), p_txt)
    sub  <- if (xf == "mean_temp_c") "  (only precincts with sensor coverage)" else ""
    xaxis_cfg <- list(title = paste0(xmeta$lab, sub))
    # Anchor count/share axes at 0 so the spread isn't visually exaggerated.
    if (xf %in% c("pct_commercial", "n_trees")) {
      xmax <- max(d$xval, na.rm = TRUE)
      xaxis_cfg$range <- c(0, xmax * 1.08)
    }
    plot_ly(d, x = ~xval, y = ~yval, type = "scatter", mode = "markers",
            source = "driver", key = ~precinct,
            marker = list(size = ~ifelse(is_sel, 17, 11),
                          color = ~ifelse(is_sel, PAL$ink, PAL$warm),
                          line = list(color = "white", width = 1)),
            hoverinfo = "text",
            text = ~sprintf("%s<br>%s<br>%.0f GJ/property", precinct, vapply(xval, xmeta$fmt, character(1)), yval)) %>%
      layout(xaxis = xaxis_cfg, yaxis = list(title = "Energy per property (GJ)"),
             annotations = list(x = 0.02, y = 0.98, xref = "paper", yref = "paper",
                                text = note, showarrow = FALSE, xanchor = "left",
                                font = list(color = PAL$grey, size = 11))) %>%
      transparent() %>% config(displayModeBar = FALSE)
  })
  
  # Clicking a driver point toggles that precinct in the comparison set (max 3):
  # click once to add, click again to remove.
  observeEvent(event_data("plotly_click", source = "driver"), {
    ev <- event_data("plotly_click", source = "driver")
    if (!is.null(ev$key)) {
      cur <- if (is.null(input$cmp)) character(0) else input$cmp
      if (ev$key %in% cur) {
        cur <- setdiff(cur, ev$key)
      } else if (length(cur) < 3) {
        cur <- c(cur, ev$key)
      }
      updateSelectizeInput(session, "cmp", selected = cur)
    }
  })
  
  observeEvent(input$clear_cmp, {
    updateSelectizeInput(session, "cmp", selected = character(0))
  })
  
  # One comparison card per selected precinct.
  output$compare_cards <- renderUI({
    if (length(input$cmp) == 0) return(NULL)
    cards <- lapply(input$cmp, function(pc) {
      p <- master %>% filter(precinct == pc)
      if (nrow(p) != 1) return(NULL)
      div(class = "detailcard",
          tags$h4(p$precinct),
          div(class = "row", span("Energy/property (2026)"), tags$b(sprintf("%.0f GJ", p$mean_energy_2026))),
          div(class = "row", span("Commercial building share"), tags$b(sprintf("%.0f%%", p$pct_commercial))),
          div(class = "row", span("Trees in precinct"), tags$b(format(p$n_trees, big.mark = ","))),
          div(class = "row", style = "border:none;", span("Growth 2011\u219226"),
              tags$b(sprintf("%+.0f%%", p$energy_growth_pct))))
    })
    div(class = "cmpwrap", cards)
  })
  
  # Insight text switches with the chosen driver.
  output$energy_insight <- renderUI({
    msg <- switch(input$driver_x,
                  pct_commercial = "<b>Insight.</b> Across the 20 precincts, commercial building share is the strongest
        correlate of energy demand (Spearman r = 0.77, p &lt; 0.001). Melbourne CBD (84% commercial) uses
        ~2,595 GJ per property; leafy Parkville (10% commercial, 28,563 trees) uses just 88 GJ.",
                  n_trees = "<b>Insight.</b> Tree count shows no meaningful association with energy demand
        (Spearman r = 0.10) \u2014 the points scatter without a trend. Greenery relates to the local
        microclimate, not to how much energy a precinct uses.",
                  mean_temp_c = "<b>Insight.</b> Mean temperature is only available for precincts with sensor
        coverage. This view shows an observed association and should not be interpreted as causal.")
    HTML(paste0(msg, " Overall, <b>commercial building composition</b> is far more strongly associated with
      energy demand than tree count in this dataset."))
  })
  
  # SECTION 5: takeaways computed from the data 
  # Four headline numbers, one per question. Cards 1-2 are fixed facts; card 3
  # (trees vs temperature) recomputes with the season filter; card 4 ties to Section 4.
  output$takeaways <- renderUI({
    pk <- heat_metrics %>% slice_max(mean_temp_c, n = 1, with_ties = FALSE)             # hottest hour x month cell
    worst <- pm25_stats %>% slice_max(pct_over20, n = 1, with_ties = FALSE)             # worst-air season
    best  <- pm25_stats %>% slice_min(pct_over20, n = 1, with_ties = FALSE)             # cleanest-air season
    d3 <- sens_view() %>% filter(!is.na(mean_temp_c))                                   # trees vs temp (current season)
    r3 <- if (nrow(d3) > 2) suppressWarnings(cor(d3$tree_count_500m, d3$mean_temp_c, method = "spearman")) else NA
    s3 <- if (input$season == "All seasons") "the full record" else tolower(input$season)
    ycol <- "mean_energy_2026"                                                          # commercial share vs energy
    d4 <- precinct_energy %>% filter(!is.na(.data[[ycol]]))
    ct4 <- suppressWarnings(cor.test(d4$pct_commercial, d4[[ycol]], method = "spearman"))
    p4txt <- if (ct4$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", ct4$p.value)
    
    div(class = "takeaways",
        div(class = "tk", style = "--accent:#d7472b;",
            div(class = "tkhead", span(class = "tkic", icon("temperature-high")),
                span(class = "big", sprintf("%.1f\u00b0C", pk$mean_temp_c))),
            div(class = "lab", sprintf("Hottest hourly mean \u2014 %s around %02d:00, and summer nights stay warm.",
                                       pk$month_lbl, pk$hour))),
        div(class = "tk", style = "--accent:#e0a528;",
            div(class = "tkhead", span(class = "tkic", icon("wind")),
                span(class = "big", worst$season)),
            div(class = "lab", sprintf("Highest share of elevated PM2.5 readings: %.1f%% of %s readings exceeded 20 \u00b5g/m\u00b3, vs %.1f%% in %s.",
                                       worst$pct_over20, tolower(worst$season), best$pct_over20, tolower(best$season)))),
        div(class = "tk", style = "--accent:#2a8a5d;",
            div(class = "tkhead", span(class = "tkic", icon("tree")),
                span(class = "big", if (is.na(r3)) "\u2013" else sprintf("r = %.2f", r3))),
            div(class = "lab", sprintf("Trees vs temperature across %s. \u2191 updates with the season filter.",
                                       s3))),
        div(class = "tk", style = "--accent:#2c7fb8;",
            div(class = "tkhead", span(class = "tkic", icon("bolt")),
                span(class = "big", sprintf("r = %.2f", unname(ct4$estimate)))),
            div(class = "lab", sprintf("Commercial building share vs energy, 2026 (%s); tree count is not significant (r = 0.10).",
                                       p4txt))))
  })
}

shinyApp(ui, server)