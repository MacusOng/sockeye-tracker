# Version tracker - Let's Follow the Sockeye

Every change to `app.R` gets a version here. Newest first. Times are GMT-7.
MAJOR.MINOR.PATCH: new feature or tab = minor, fix or small change = patch, breaking redesign = major.

How to record a change (in this order):
1. Set `APP_VERSION` in `app.R` (top of file).
2. Add an entry at the top of this file (date, type, what changed and why).
3. Run `Rscript tools/check_version.R` from the project folder; it confirms the two match.

Entry types: Added, Changed, Fixed, Removed.

## 0.5.2 - 2026-09-29
- Changed: exported plots carry a visible footer with population, release years, version and export date/time.

## 0.5.1 - 2026-09-29
- Removed: Map tab (maps remain on Migration and Recaptures).
- Changed: app renamed "Let's Follow the Sockeye" with subtitle "Okanagan fry and smolt outmigration tracker".

## 0.5.0 - 2026-09-29
- Added: Recaptures tab (summary, map, table, CSV export). Reads every file with "Recapture" in its name in `data\`, stacks and de-duplicates, so add-on years just work.
- Added: non-Okanagan release sites grouped as "Columbia (Arrow Lakes)"; display only.
- Changed: Mission Creek position now read from the updated `MRRSiteList.csv`.

## 0.4.2
- Removed: Data tab from the interface; notes moved to the README.

## 0.4.1
- Fixed: dark-mode table striping contrast.

## 0.4.0
- Added: Survival tab (arithmetic and harmonic mean travel time, reach survival, lambda; Cormack-Jolly-Seber, checked against the manual verify script).

## 0.3.0
- Added: multi-page layout, Go button, separate release sites (Equesis, Mission, Shingle Creek), full names instead of acronyms, maps, migration playback, speed tab.

## 0.2.0
- Changed: loads every dataset found; run-code filter removed (fixed empty ONA Fish Hatchery results).

## 0.1.3
- Fixed: dark/light mode; plot export.

## 0.1.2
- Fixed: data folder now found from `App_Dev`.

## 0.1.1
- Fixed: dashboard page structure error.

## 0.1.0
- First version.

Dates before 0.5.0 were not recorded.
