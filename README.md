# Let's Follow the Sockeye (CSS FTT / PitPro verification dashboard)

Author: Macus Ong, Okanagan Nation Alliance
Current version: see `VERSION` (also shown top-right in the app)

Shiny dashboard for the Okanagan fry and smolt outmigration study. It reads
PTAGIS "PitPro Tagging" and "PitPro Interrogation Detail" exports and shows
release, detection, travel time, speed, survival and lambda by release group.

## Run

1. Open `App_Dev/CSS_FTT_Dashboard.Rproj` in RStudio.
2. In the console: `shiny::runApp()`
3. First time only: `install.packages(c("shiny","shinyjs","dplyr","ggplot2","lubridate","readxl","leaflet","DT","viridisLite"))`

## Folders

| Folder | Contents |
|---|---|
| `App_Dev/` | `app.R`, the RStudio project file |
| `data/` | PTAGIS exports plus `InterrogationSiteList_2026.csv` and `MRRSiteList.csv` |
| `outputs/` | Saved plots and tables |
| `archive/` | Old scripts, including the manual `PitPro_Verify_ONAF_SKAOSO_Combined.R` |
| `R/` | Helper scripts |

## Data files

- Files must contain `PitPro` and either `Tagging` or `Interrogation Detail`, plus the download date, for example
  `2026 SKATAL_PitPro Tagging_August26 2026.csv`.
- Every dataset (text before `PitPro`) is loaded, using its newest export date. Overlapping exports are de-duplicated by tag ID and by tag + site + time.
- CSV and XLSX both work. Multi-year and single-year files can be mixed.
- Detections at or before the release time are dropped.
- Species code 4 (sockeye) only.
- Files that do not contain `PitPro` in the name (for example `ONAF tags_2023-2025.xlsx`) are ignored.
- Loaded files and counts are printed in the R console when the app starts.

## Populations and release sites

| Shown in the app | Release site code | Notes |
|---|---|---|
| Skaha Lake | SKATAL | |
| Osoyoos Lake | OSOYOL | |
| Okanagan Lake (screw trap) | OKANR | Caught in a rotary screw trap; independent of the hatchery fish |
| ONA Fish Hatchery - Equesis Creek | EQUESC | Fry from brood collection |
| ONA Fish Hatchery - Mission Creek | MISS3C | Position read from MRRSiteList.csv |
| ONA Fish Hatchery - Shingle Creek | SHINGC | |

No acronyms are shown in the app; codes appear here only for reference. Hatchery release sites are never pooled.

## Checkpoints (upstream to downstream)

Penticton (OKD, OKP), Okanagan Channel (OKC), Zosel Dam (ZSL), Rocky Reach Dam (RRJ, RRF),
McNary Dam, John Day Dam, Bonneville Dam (B2J, BCC and other Bonneville codes), Columbia Estuary.
Detection rates count only fish released upstream of each checkpoint.

## Survival tab (ported from the manual PitPro verify script)

- Travel time: arithmetic mean with SD, harmonic mean with delta-method SE, positive times only.
- Survival: single-release Cormack-Jolly-Seber. Reach survivals, detection probabilities and lambda (final-reach survival x terminal detection; only the product is estimable).
- Chains: Skaha Lake / Osoyoos Lake / Okanagan Lake = Release, Rocky Reach Dam, Bonneville Dam. Hatchery sites = Release, Penticton, Okanagan Channel, Rocky Reach Dam, Bonneville Dam.
- Bonneville includes the estuary arrays for detection; only B2J and BCC give travel time.
- Estimates near 0 or 1, or with a large SE, are flagged unstable (too few detections).
- Checked: Skaha Lake 2025 matches the manual script's PitPro comparison (n, arithmetic and harmonic means).

## Recaptures tab
Any csv or xlsx in `data\` with "Recapture" in its name (for example `2026 Recapture Year Detail.csv`) is read automatically. To add another year, drop that year's file into `data\`; all recapture files are stacked and duplicates removed. If a csv and an xlsx share the same name, the csv is used; an xlsx without a `Tag Code` column (a pivot summary) is ignored.

- Shows fish caught again after release: where and when they were released, where and when they were recaptured, and days at large.
- Okanagan populations are kept separate; every other release site (Caribou Creek, Burton Creek) is grouped as "Columbia (Arrow Lakes)" and appears under All populations. Their map positions are approximate because those sites are not in the site lists.
- Display only: recaptures do not change detection rates, travel time or survival.
- Follows the population and release-year selection when Go is pressed. Days at large uses the recorded release date, so a few fish recaptured before that date show a negative number.
- Coordinates come from `MRRSiteList.csv` (Mission Creek now included).

## Exports

Plot PNGs and table CSVs are named `<name>_v<version>_<mmddyyyy>_<hhmmss>` with the timestamp in GMT-7. PNGs are always saved in light mode.

## Versioning

MAJOR.MINOR.PATCH. New feature or tab = minor. Bug fix or small change = patch. Every app.R change updates `APP_VERSION` and adds an entry to `CHANGELOG.md`.

## Changelog

See `CHANGELOG.md` (the version tracker). Check it matches the app with `Rscript tools/check_version.R`.
