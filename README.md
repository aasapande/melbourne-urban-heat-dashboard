# Urban Heat, Air Quality & Green Cover — Melbourne

An interactive R Shiny narrative visualisation exploring how urban heat, air quality, and green cover relate to building energy demand across Melbourne, built on three City of Melbourne open datasets: ~590K microclimate sensor readings, 82K trees, and 13K+ property records.

The app is a guided, scrolling data story with a story-navigation sidebar, a global season/year/precinct filter that drives several views at once, and a bidirectionally linked Leaflet map + Plotly scatter at its centre (click or hover a point in either view to highlight it in the other).

**Key finding:** the intuitive hypothesis, that more green cover reduces energy demand, didn't hold. Tree density correlates with cooler temperatures (r = −0.49), but commercial land use, not vegetation, is what actually drives energy demand (r = 0.77, p < 0.001).

## Screenshots

*(Add 1–3 screenshots or a short GIF of the app here, e.g. the heatmap section and the linked map+scatter section.)*

## How to run

1. Install R (tested on R 4.3.x) and RStudio.
2. Install the required packages (once):
   ```r
   install.packages(c("shiny", "leaflet", "plotly", "dplyr", "readr",
                       "tidyr", "stringr", "scales", "htmltools"))
   ```
3. Open `app.R` **in its own folder** (with `data/` and `www/` beside it) and click **Run App**, or from that folder run:
   ```r
   shiny::runApp()
   ```
   Don't move `app.R` out of the folder; it reads `./data` and `./www` by relative path.

## Internet dependencies

The app runs fully offline, with graceful fallbacks:
- Leaflet basemap tiles (CartoDB) need internet; without it, map markers still render but the background tiles may not load.
- Fonts load from Google Fonts; offline, the app falls back to system fonts and stays fully readable.
- The welcome video is bundled locally in `www/` and needs no internet connection.

## Folder structure

```
app.R                              Shiny application (UI + server). Reads only ./data and ./www.
data/
  microclimate_hourly_metrics.csv  All-sensor hour x month grid
  sensor_hourly_metrics.csv        Per-sensor hour x month (drill-down)
  sensor_summary.csv               All-seasons per-sensor (map + scatter)
  sensor_season.csv                Per-season per-sensor
  pm25_daily.csv                   Daily PM2.5 (Section 2)
  pm25_season_stats.csv            Per-season PM2.5 stats (live KPI)
  energy_by_type_year.csv          Stacked area chart (Section 4)
  master_precinct.csv              20 precincts (Section 4)
  tree_density.csv                 Binned tree-density grid (map layer)
www/
  melbourne-hero.jpg               Hero photo
  melbourne-hero-poster.jpg        Welcome-video poster frame
  melbourne-hero.mp4               Welcome-screen video
```

## Data preparation

All cleaning and wrangling (timezone conversion, faulty-sensor filtering, spatial joins, aggregation) were completed upstream and are not part of this repo. The CSVs in `data/` are the already-prepared, app-ready files, so the app runs directly from them with no cleaning step at load time.

## Data notes

- Sources: City of Melbourne Open Data — Microclimate Sensor Readings, Urban Forest (trees), and Property-Level Energy (modelled, baseline 2011).
- Energy figures are **modelled** snapshots for 2011, 2016, 2021, and 2026, not annual meter readings.
- Tree count near each sensor is a **proxy** for local green cover (counted within ~500 m), not canopy area.
- Precincts are assigned by nearest-centroid (approximate), not official polygons.
- Sensor `ICTMicroclimate-10` is excluded (part-year record); other sensors have unequal coverage, so single-sensor views show a coverage caption.
- `Royal Park (aws5-0999)` has no PM2.5 instrument, so it drops out when the Section 3 scatter is toggled to PM2.5.

## Tech stack

R, Shiny, Leaflet, Plotly, dplyr

## Troubleshooting

- Run from the project root; confirm `data/` and `www/` sit beside `app.R`.
- If a package is missing, re-run the `install.packages` line above.
- Make sure no data files were renamed with `(1)`/`(2)`/`(3)` suffixes; filenames must match the list above exactly.
