FIT5147 Data Visualisation Project (DVP) Part 2
Urban Heat, Air Quality & Green Cover: Relationships with Energy Demand in Melbourne
Aasavari Pande | 35595353 | Applied Session 09
================================================================================

WHAT THIS IS
An interactive narrative visualisation (R Shiny) implementing FDS Sheet 5: a
guided, scrolling data story with a story-navigation sidebar, a global season
filter that drives several views at once, and a bidirectionally linked Leaflet
map + Plotly scatter at its centre.

HOW TO RUN
1. Install R (tested on R 4.3.x) and RStudio.
2. Install packages (once). Tested with recent CRAN versions:
     install.packages(c("shiny","leaflet","plotly","dplyr","readr",
                        "tidyr","stringr","scales","htmltools"))
3. Open app.R IN ITS OWN FOLDER (the project root, with data/ and www/ beside
   it) and click "Run App"  (or, from that folder:  shiny::runApp() ).
   Do not move app.R out of the folder; it reads ./data and ./www by relative
   path.
4. Tested on a second machine by copying the unzipped folder and running it
   without changing any paths.

INTERNET DEPENDENCIES (the app still runs offline, with graceful fallbacks)
- The Leaflet basemap tiles (CartoDB) need internet; without it the map markers
  still work but the background map may not load.
- Headings/body use Google Fonts; if offline, the app falls back to system fonts
  and remains fully readable.
- The welcome video is bundled locally (www/) and needs no internet.

FOLDER STRUCTURE
  app.R            The Shiny application (UI + server). Reads only ./data and ./www.
  data/            Pre-aggregated files the app reads (all nine are required):
                     microclimate_hourly_metrics.csv  all-sensor hour x month grid
                     sensor_hourly_metrics.csv         per-sensor hour x month (drill-down)
                     sensor_summary.csv                all-seasons per-sensor (map + scatter)
                     sensor_season.csv                 per-season per-sensor
                     pm25_daily.csv                    daily PM2.5 (Section 2)
                     pm25_season_stats.csv             per-season PM2.5 stats (live KPI)
                     energy_by_type_year.csv           stacked area (Section 4)
                     master_precinct.csv               20 precincts (Section 4)
                     tree_density.csv                  binned tree-density grid (map layer)
  www/             Media used in the page chrome (not behind any chart):
                     melbourne-hero.jpg                hero photo (Robert Stokoe / Pexels)
                     melbourne-hero-poster.jpg         welcome-video poster frame
                     melbourne-hero.mp4                welcome-screen video (author's own)

DATA PREPARATION
All data cleaning and wrangling were completed during the Data Exploration
Project. The CSVs in ./data are the already-prepared, app-ready files (further
aggregated for this visualisation), so the app runs directly from them with no
cleaning step at load time. The wrangling code is not required to run the app
and is not included; the preparation steps are described in Section 3.1 of the
report.

DATA NOTES (described more fully in the report)
- The three sources are City of Melbourne Open Data: Microclimate Sensor Readings,
  Urban Forest (trees), and Property-Level Energy (modelled, baseline 2011).
- Energy figures are MODELLED snapshots for 2011, 2016, 2021 and 2026, not annual
  meter readings.
- The energy growth figure (+47%, 2011->2026) is computed on properties with
  non-zero modelled energy in 2026; this cohort is used consistently.
- Tree count near each sensor is a PROXY for local green cover (counted within
  roughly 500 m); it is not canopy area.
- Precincts are assigned by nearest-centroid (approximate), not official polygons.
- Sensor ICTMicroclimate-10 is excluded (part-year record); other sensors have
  unequal coverage, so single-sensor views show a coverage caption and the pooled
  "All sensors" view is averaged across sensors.
- Royal Park (aws5-0999) has no PM2.5 instrument, so it drops out when the
  Section 3 scatter is toggled to PM2.5.

TROUBLESHOOTING
- Run from the project root; confirm data/ and www/ sit beside app.R.
- If a package is missing, re-run the install.packages line above.
- Ensure no data files were renamed with "(1)"/"(2)"/"(3)" suffixes; filenames
  must match the list above exactly.