# Version tracker - Let's Follow the Sockeye

Every change to `app.R` gets a version here. Newest first. Times are UTC (GMT-7 before 0.18.0).
MAJOR.MINOR.PATCH: new feature or tab = minor, fix or small change = patch, breaking redesign = major.

How to record a change (in this order):
1. Set `APP_VERSION` in `app.R` (top of file).
2. Add an entry at the top of this file (date, type, what changed and why).
3. Run `Rscript tools/check_version.R` from the project folder; it confirms the two match.

Entry types: Added, Changed, Fixed, Removed.

## 0.18.0 - 2026-11-06
- Added: screensaver. When the page sits idle (1, 2, 5, 10 or 15 minutes, set in Settings), it shows a large UTC clock and/or a slideshow of the photo library (new photo every 8 seconds, with captions). Modes: Clock + photos (default), Clock only, Photos only, Off, plus a Preview button. Any mouse move, click, key or touch returns to the dashboard. Runs in the browser only, so it does not keep the shinyapps.io session awake.
- Changed: all app times are now UTC instead of GMT-7 (version box, export file names, plot footers and the Follow-the-sun sunrise/sunset times), because local time-zone rules are unsettled. Times in the PTAGIS data were already handled as UTC.

## 0.17.0 - 2026-11-05
- Added: Settings pane in the left panel (Filters / Settings switch at the top). Eight colour themes (Classic, Light, Dark, Midnight, Graphite, Forest, Sand, High Contrast), "Follow the sun" (Auto switches between Light and Dark at sunrise and sunset in Penticton; choosing a theme by hand switches it off) and an interface font picker using system typefaces only, with a live preview. Settings are saved in the viewer's browser.
- Removed: the Dark mode / Light mode button at the top of the left panel (replaced by the themes). Charts and the map still switch to dark styling with any dark theme.
- Fixed: stray space before the full stop in the "May still be migrating" warning.

## 0.16.0 - 2026-11-02
- Added: "may still be migrating" check on the p-value cards (Survival and Speed tabs). For each release site and year, the dashboard compares the days between release and the end of the data (latest detection in that export) with how long that site's fish usually take to reach Rocky Reach Dam (90th percentile from earlier release years; calendar fallback when there is no history: hatchery fry complete after July 31 of the next year, lake and screw-trap fish after August 31 of the release year). Flagged groups get a warning and are left out of the p-value tests by default; a tick box puts them back in.
- Added: print layout. Printing or saving the page as PDF no longer draws the left pane over the content, and download buttons are hidden.

## 0.15.0 - 2026-11-02
- Added: survival p-values on the Survival tab, under the "Survival and lambda" table. Release groups that share the same survival estimate (for example "Release to Rocky Reach Dam" for the lake populations) are compared pair by pair with a two-sided z-test on the Cormack-Jolly-Seber estimates and their standard errors, Holm-adjusted when there are several pairs. Each comparison has a plain-language summary and a table. Detection probability and lambda are not compared, because they are not survival; unstable estimates are left out and listed.

## 0.14.0 - 2026-11-02
- Added: population comparisons with p-values. The Survival tab has a new card, "Do the selected populations differ? (p-values)", that compares the populations picked in the left pane on three measures: share of tagged fish detected at Rocky Reach Dam or farther downstream (Fisher's exact test for two populations, chi-squared for three or more), days from release to Rocky Reach Dam, and pace in km/day (Wilcoxon rank-sum for two populations, Kruskal-Wallis for three or more). With three or more populations a pairwise table (Holm-adjusted) shows which pairs differ. Each result has a plain-language summary of what the p-value means for that selection, plus an "Export p-values (CSV)" button.
- Added: the pace p-value also shows on the Speed tab under "Median pace by release location".
- Populations with fewer than 3 fish for a measure are left out of that test and listed. Release years in the selection are pooled. With one population selected, the card asks for two or more.

## 0.13.0 - 2026-10-01
- Added: the Population picker in the left pane is now multi-select. Pick any combination of release sites, or "All Populations". Choosing "All Populations" clears the individual sites; ticking all six sites switches back to "All Populations". Search box, Select All / Deselect All buttons and a count of sites selected. Needs the shinyWidgets package.
- Fixed: CHANGELOG.md entry for 0.13.0 had been added above the file header with garbled characters; moved here in the standard format.

## 0.12.1 - 2026-09-30
- Fixed: photo library address now uses the correct GitHub account name (`MacusOng/sockeye-tracker-photos`).

## 0.12.0 - 2026-09-29
- Added: Photos tab. It reads every image (jpg, jpeg, png, gif, webp) from the public GitHub repository `MacusOng/sockeye-tracker-photos`, so photos are uploaded there instead of into the dashboard. Folders become albums, captions come from the file names, with album filter, search, "Show more" paging and a large-view pop-up. Refreshes on start, on Go and with the Refresh photos button.
- Added: `GITHUB_SETUP.md` (creating both repositories, pushing the dashboard, uploading photos).

## 0.11.2 - 2026-09-29
- Added: small key under the spotlight fish slider: red dot = fish position, hollow gold circle = site where it will be detected, solid gold circle = site where it has been detected.

## 0.11.1 - 2026-09-29
- Changed: exported plot footer now says "Generated" instead of "Exported".

## 0.11.0 - 2026-09-29
- Fixed: the Okanagan Lake screw trap is now placed at Penticton (Okanagan River channel). PTAGIS lists the OKANR code near Tonasket, WA, which put it in the wrong area. Its fish now start at Penticton, so their Penticton, Okanagan Channel and Zosel Dam detections are counted (previously hidden). Map, spotlight route and detection rates for this population change.
- Added: checkpoints that already had detections in the data but were not shown: Lower Okanogan River, Wells Dam, Rock Island Dam, Priest Rapids Dam, The Dalles Dam. Survival chains and survival results are unchanged.
- Changed: Migration tiles and map labels say where each checkpoint is (for example Penticton = Okanagan Lake dam and channel array at Penticton, BC; Okanagan Channel = Okanagan River channel between Oliver and Osoyoos Lake, BC).
- Changed: the river route gains the new checkpoints. An existing `RiverPath.csv` needs the new `cp:` keys or it is ignored.

## 0.10.0 - 2026-09-29
- Added: "Release dates" range in the left pane, under Release year. It is a free date range (not limited to the dates in the data) applied to release date when Go is pressed, on top of the year selection. It starts at the span of the data.
- Added: spotlight fish "Release site" and "Final destination" dropdowns (final destination = farthest checkpoint the fish was detected at, with fish counts). Changing either draws a matching fish; "Another fish" draws another match. Replaces the "Another from this location" button.

## 0.9.0 - 2026-09-29
- Changed: the spotlight fish now moves along the river (Okanagan lakes and river, Okanogan River, Columbia River) instead of straight lines between stations. Stations where it was detected show as gold circles (hollow until the fish reaches them, solid after, with the date on hover) and a single dot moves along the water with no trail line.
- Changed: the Migration map draws the river as its base line instead of a straight dashed line between checkpoints.
- Added: optional `data\RiverPath.csv` (columns key, lat, lon, upstream to downstream, with a `cp:` key for every checkpoint) to replace the built-in river route with a more exact centreline.
- Note: the built-in river route is hand-placed and approximate.

## 0.8.1 - 2026-09-29
- Fixed: the spotlight fish no longer plays along with the whole season. It now has its own date slider and play button in the card that run from the fish's release date to its last detection, then stop. The season date slider above only drives the counts. Removed the "Start at this fish's release" button (no longer needed).

## 0.8.0 - 2026-09-29
- Added: spotlight fish now travels the Migration map with the date slider. A gold marker moves from its release site through each checkpoint it was detected at, leaving a trail, with the full route shown faintly.
- Changed: spotlight card says where the fish is on the slider date (not released yet, travelling between two places, or last seen), and has "Start at this fish's release" and "Another fish" buttons. Only fish detected at two or more checkpoints are chosen. Choosing another fish moves the slider to its release date.

## 0.7.1 - 2026-09-29
- Removed: the "Preview a fish" picker, auto-jump to Journeys and mini timeline (0.6.1 to 0.7.0); unreliable after the first fish.
- Added: static "This selection" summary in the left pane (fish released, detected downstream, farthest checkpoint, median days to Bonneville Dam, recaptured). Updates when Go is pressed.
- Changed: Journeys path card follows the clicked row again (still shown above the table).

## 0.7.0 - 2026-09-29
- Added: choosing a fish in the "Preview a fish" picker opens the Journeys tab, jumps the table to that fish, and shows its full path card above the table. Fish with no downstream detections stay on the current tab.
- Added: mini timeline (days since release at each checkpoint) in the left-pane preview.
- Changed: clicking an Explore row fills the picker without leaving Explore. The Journeys path card now follows the picker.

## 0.6.1 - 2026-09-29
- Changed: the left-pane preview now has a "Preview a fish" picker. Type or paste any PIT tag in the current selection (detected fish are listed first) and its release, checkpoint dates, pace and any recapture show on every tab. Clicking a row in Explore or Journeys fills the picker too. Speed and Survival summaries show when no fish is picked.

## 0.6.0 - 2026-09-29
- Added: preview panel at the bottom of the left pane that follows the active tab. Explore and Journeys (click a fish row), Speed (fastest or slowest fish) and Survival (lambda by release group). It only appears when there is real data to show.
- Changed: Dark/Light mode button moved to the top of the left pane. Explore rows are now single-select. Selected table rows stay readable in dark mode.

## 0.5.3 - 2026-09-29
- Fixed: Explore table (and other tables) kept widening the page. The page layout let wide tables stretch it; tables now stay inside their card and column widths are recalculated when a tab opens.
- Changed: Migration tiles now say what they count ("tagged fish released by this date", "fish detected by this date").

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

