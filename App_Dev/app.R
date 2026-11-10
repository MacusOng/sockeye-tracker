# =============================================================================
#  Let's Follow the Sockeye  (Okanagan fry and smolt outmigration tracker)
#  Okanagan fry / smolt outmigration - Okanagan Nation Alliance (ONA)
#
#  Version: 0.4.2  (patch: Data tab removed from the interface; notes moved to README)
#  Prior:   0.4.1  (patch: dark-mode table striping contrast)
#  Prior:   0.4.0  (new Survival tab: travel time arithmetic/harmonic means, survival, lambda)
#  Versioning: MAJOR.MINOR.PATCH - new feature/tab = MINOR, bug fix = PATCH
#  Author : Macus Ong, Okanagan Nation Alliance
#
#  Packages: shiny, shinyjs, dplyr, ggplot2, lubridate, readxl, leaflet, DT,
#            viridisLite
#    install.packages(c("shiny","shinyjs","dplyr","ggplot2","lubridate",
#                       "readxl","leaflet","DT","viridisLite"))
#
#  Folder layout (this file lives in App_Dev/):
#    ../data/     PTAGIS "PitPro Tagging" + "PitPro Interrogation Detail" exports
#    ../outputs/  (exports are downloaded through the browser)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(readxl)
  library(leaflet)
  library(DT)
  library(viridisLite)
})

# =============================================================================
# 0. VERSION, PATHS, HELPERS
# =============================================================================

APP_VERSION      <- "0.23.0"
APP_TIMEZONE     <- "UTC"                             # all app times are UTC (local time-zone rules unsettled)
tz_now           <- function(fmt) format(Sys.time(), fmt, tz = APP_TIMEZONE)
APP_VERSION_DATE <- tz_now("%m %d %Y %H %M %S")

nz       <- function(a, b) if (is.null(a)) b else a
fmt_int  <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_date <- function(x) format(x, "%b %d, %Y")

find_project_dir <- function(sub) {
  for (cand in c(sub, file.path("..", sub))) {
    if (dir.exists(cand)) return(normalizePath(cand, winslash = "/"))
  }
  NA_character_
}
DATA_DIR <- find_project_dir("data")

# =============================================================================
# 1. POPULATIONS, RELEASE SITES, DETECTION CHECKPOINTS
#    (full names only on screen - no site codes)
# =============================================================================

# Release sites. ONA Fish Hatchery fish are fry from brood collection, released
# at three separate tributary sites. Okanagan Lake fish are caught in a rotary
# screw trap in the Okanagan River channel at Penticton (PTAGIS lists the OKANR code near
# Tonasket, WA, so its position is set to the Penticton site). start_cp = first checkpoint a fish can pass.
RELEASE <- data.frame(
  code      = c("EQUESC", "MISS3C", "SHINGC", "SKATAL", "OSOYOL", "OKANR"),
  label     = c("Equesis Creek", "Mission Creek", "Shingle Creek",
                "Skaha Lake", "Osoyoos Lake", "Okanagan Lake (screw trap)"),
  full      = c("ONA Fish Hatchery - Equesis Creek",
                "ONA Fish Hatchery - Mission Creek",
                "ONA Fish Hatchery - Shingle Creek",
                "Skaha Lake", "Osoyoos Lake", "Okanagan Lake (screw trap)"),
  lat       = c(50.371865, 49.860294, 49.503792, 49.344503, 49.018503, 49.500853),
  lon       = c(-119.492405, -119.443545, -119.740968, -119.580079, -119.458285, -119.613364),
  start_cp  = c("PEN", "PEN", "OKC", "OKC", "ZSL", "PEN"),
  stringsAsFactors = FALSE
)

pal_m <- magma(7,   begin = 0.30, end = 0.85)
pal_i <- inferno(7, begin = 0.30, end = 0.85)
RELEASE$color <- c(pal_m[1], pal_m[3], pal_m[5], pal_i[2], pal_i[4], pal_i[6])
POP_FILL <- setNames(RELEASE$color, RELEASE$full)
POP_FILL <- c(POP_FILL, "Other release site" = "#8a8a8a")

# Define individual sites for multi-select population picker
INDIVIDUAL_SITES <- c("EQUESC", "MISS3C", "SHINGC", "SKATAL", "OSOYOL", "OKANR")

POP_CHOICES <- list(
  "All Populations (convenience)" = "all",
  "ONA Fish Hatchery" = c("Equesis Creek" = "EQUESC", "Mission Creek" = "MISS3C",
                          "Shingle Creek" = "SHINGC"),
  "Lakes & Screw Trap" = c("Skaha Lake" = "SKATAL", "Osoyoos Lake" = "OSOYOL",
                           "Okanagan Lake (screw trap)" = "OKANR")
)

# Downstream checkpoints, upstream -> downstream (rkm = river km from the
# Columbia River mouth, from the PTAGIS interrogation site list)
CHECKPOINTS <- data.frame(
  cp    = c("PEN", "OKC", "ZSL", "OKL", "WEL", "RRJ", "RIS", "PRD", "MCJ", "JDJ", "TDA", "BON", "EST"),
  label = c("Penticton", "Okanagan Channel", "Zosel Dam", "Lower Okanogan River", "Wells Dam",
            "Rocky Reach Dam", "Rock Island Dam", "Priest Rapids Dam", "McNary Dam", "John Day Dam",
            "The Dalles Dam", "Bonneville Dam", "Columbia Estuary"),
  rep   = c("OKD", "OKC", "ZSL", "OKL", "WEA", "RRJ", "RIA", "PRA", "MCJ", "JDJ", "TD1", "B2J", "PD7"),
  rkm   = c(1054, 1007, 990, 883, 830, 763, 730, 639, 470, 347, 308, 234, 70),
  lat   = c(49.5009, 49.1141, 48.9335, 48.2687, 47.9473, 47.5320, 47.3433, 46.6443, 45.9323, 45.7100, 45.6191, 45.6368, 46.1467),
  lon   = c(-119.6134, -119.5658, -119.4198, -119.7281, -119.8645, -120.2996, -120.0933, -119.9104, -119.2999, -120.6926, -121.1197, -121.9653, -123.3799),
  desc  = c("Okanagan Lake dam and channel array at Penticton, BC",
            "Okanagan River channel, between Oliver and Osoyoos Lake, BC",
            "Zosel Dam on the Okanogan River, Oroville, WA",
            "Okanogan River instream array near Malott, WA",
            "Wells Dam on the Columbia River, WA",
            "Rocky Reach Dam on the Columbia River, WA",
            "Rock Island Dam on the Columbia River, WA",
            "Priest Rapids Dam on the Columbia River, WA",
            "McNary Dam on the Columbia River, WA and OR",
            "John Day Dam on the Columbia River, WA and OR",
            "The Dalles Dam on the Columbia River, WA and OR",
            "Bonneville Dam on the Columbia River, WA and OR",
            "Lower Columbia River estuary arrays"),
  stringsAsFactors = FALSE
)
RELEASE$start_idx <- match(RELEASE$start_cp, CHECKPOINTS$cp)

SITE_TO_CP <- c(
  OKD = "PEN", OKP = "PEN", OKC = "OKC", ZSL = "ZSL", OKL = "OKL",
  WEA = "WEL", WEJ = "WEL", WEH = "WEL", RRJ = "RRJ", RRF = "RRJ", RIA = "RIS", PRA = "PRD", PRH = "PRD",
  TD1 = "TDA", TD2 = "TDA",
  MCJ = "MCJ", MC1 = "MCJ", MC2 = "MCJ", JDJ = "JDJ", JO1 = "JDJ", JO2 = "JDJ",
  B2J = "BON", BCC = "BON", BO1 = "BON", BO2 = "BON", BO3 = "BON", BO4 = "BON",
  PD5 = "EST", PD6 = "EST", PD7 = "EST", PD8 = "EST", PDW = "EST", PDO = "EST",
  PDC = "EST", TWX = "EST"
)

# Optionally refresh coordinates / river km from the site lists in data/
refresh_from_site_lists <- function(data_dir) {
  if (is.na(data_dir)) return(invisible())
  f_int <- file.path(data_dir, "InterrogationSiteList_2026.csv")
  if (file.exists(f_int)) try({
    s <- read.csv(f_int, fileEncoding = "latin1", check.names = FALSE, stringsAsFactors = FALSE)
    i <- match(CHECKPOINTS$rep, s[["Int Site Code"]])
    ok <- !is.na(i)
    lat <- suppressWarnings(as.numeric(s[["Lat"]][i]))
    lon <- suppressWarnings(as.numeric(s[["Long"]][i]))
    rk  <- suppressWarnings(as.numeric(s[["Int Site RKM Total"]][i]))
    CHECKPOINTS$lat[ok & !is.na(lat)] <<- lat[ok & !is.na(lat)]
    CHECKPOINTS$lon[ok & !is.na(lon)] <<- lon[ok & !is.na(lon)]
    CHECKPOINTS$rkm[ok & !is.na(rk)]  <<- rk[ok & !is.na(rk)]
  }, silent = TRUE)
  f_mrr <- file.path(data_dir, "MRRSiteList.csv")
  if (file.exists(f_mrr)) try({
    m <- read.csv(f_mrr, fileEncoding = "latin1", check.names = FALSE, stringsAsFactors = FALSE)
    i <- match(RELEASE$code, m[["MRR Site Info Code"]])
    ok <- !is.na(i)
    lat <- suppressWarnings(as.numeric(m[["MRR Site Latitude Value"]][i]))
    lon <- suppressWarnings(as.numeric(m[["MRR Site Longitude Value"]][i]))
    RELEASE$lat[ok & !is.na(lat)] <<- lat[ok & !is.na(lat)]
    RELEASE$lon[ok & !is.na(lon)] <<- lon[ok & !is.na(lon)]
  }, silent = TRUE)
  # the screw trap sits in the Okanagan River channel at Penticton, not at the PTAGIS OKANR position
  i <- match("OKANR", RELEASE$code); j <- match("PEN", CHECKPOINTS$cp)
  RELEASE$lat[i] <<- CHECKPOINTS$lat[j]; RELEASE$lon[i] <<- CHECKPOINTS$lon[j]
  invisible()
}
suppressWarnings(refresh_from_site_lists(DATA_DIR))

# =============================================================================
# 2. DATA DISCOVERY AND LOADING
# =============================================================================

# Pair tagging / interrogation files by dataset name and keep the newest export
# date of each dataset (e.g. "ALL POP", "ONAF", "SKATAL", "OSOYOL").
find_all_datasets <- function(data_dir = DATA_DIR) {
  if (is.na(data_dir) || !dir.exists(data_dir)) return(data.frame())
  files <- list.files(data_dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  files <- files[grepl("pitpro", basename(files), ignore.case = TRUE) &
                   !grepl("^~\\$", basename(files))]
  if (length(files) == 0) return(data.frame())

  months <- c(january = 1, february = 2, march = 3, april = 4, may = 5, june = 6,
              july = 7, august = 8, september = 9, october = 10, november = 11, december = 12)
  pat <- sprintf("(%s)\\s*([0-9]{1,2})\\s+(20[0-9]{2})", paste(names(months), collapse = "|"))

  parse_one <- function(f) {
    bn <- tolower(tools::file_path_sans_ext(basename(f)))
    z  <- regmatches(bn, regexec(pat, bn))[[1]]
    d  <- if (length(z) == 4)
      as.Date(sprintf("%04d-%02d-%02d", as.integer(z[4]), months[[z[2]]], as.integer(z[3])))
    else as.Date(file.info(f)$mtime)
    type <- if (grepl("interrog", bn)) "obs" else if (grepl("tagging", bn)) "tag" else NA_character_
    key  <- trimws(gsub("[_ ]+", " ", gsub("pitpro.*$", "", bn)))
    data.frame(file = f, type = type, key = key, date = d, stringsAsFactors = FALSE)
  }
  info <- do.call(rbind, lapply(files, parse_one))
  info <- info[!is.na(info$type), , drop = FALSE]

  out <- list()
  for (k in unique(info$key)) {
    sub <- info[info$key == k, ]
    tg <- sub[sub$type == "tag", ]
    ob <- sub[sub$type == "obs", ]
    if (nrow(tg) == 0 || nrow(ob) == 0) next
    common <- intersect(as.character(tg$date), as.character(ob$date))
    dt <- if (length(common)) max(as.Date(common)) else min(max(tg$date), max(ob$date))
    pick <- function(x) x$file[order(x$date == dt, x$date, decreasing = TRUE)][1]
    out[[k]] <- data.frame(dataset = toupper(k), tag_file = pick(tg), obs_file = pick(ob),
                           date = dt, stringsAsFactors = FALSE)
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

# Read a PTAGIS export (csv or xlsx); the header row is located automatically
read_ptagis <- function(path) {
  if (tolower(tools::file_ext(path)) == "xlsx") {
    probe <- readxl::read_excel(path, col_names = FALSE, col_types = "text", n_max = 15)
    hdr <- which(probe[[1]] == "Tag ID Code")[1]
    if (is.na(hdr)) hdr <- 4
    df <- as.data.frame(readxl::read_excel(path, skip = hdr - 1, col_names = TRUE,
                                           col_types = "text"), stringsAsFactors = FALSE)
    x <- df[[1]]
    if (length(x) && !is.na(x[1])) df[[1]] <- x[cummax(seq_along(x) * !is.na(x))]  # fill-down
    return(df)
  }
  top <- readLines(path, n = 15, warn = FALSE)
  hdr <- which(grepl("Tag ID Code", top, fixed = TRUE))[1]
  if (is.na(hdr)) hdr <- 1
  read.csv(path, skip = hdr - 1, header = TRUE, check.names = FALSE,
           colClasses = "character", stringsAsFactors = FALSE, fileEncoding = "latin1")
}

load_pit_pair <- function(tag_path, obs_path, species = "4") {
  tag <- read_ptagis(tag_path)[, 1:6]
  obs <- read_ptagis(obs_path)[, 1:4]
  names(tag) <- c("pit", "reldt", "relsite", "species", "run", "rear")
  names(obs) <- c("pit", "site", "obsdt", "antenna")
  tag[] <- lapply(tag, trimws)
  obs[] <- lapply(obs, trimws)

  tag <- tag[!is.na(tag$pit) & tag$pit != "" & tag$species == species, , drop = FALSE]
  tag <- tag[!duplicated(tag$pit), , drop = FALSE]
  tag$reldt_posix <- lubridate::ymd_hms(tag$reldt, tz = "UTC", quiet = TRUE)
  obs$obsdt_posix <- lubridate::ymd_hms(obs$obsdt, tz = "UTC", quiet = TRUE)

  obs <- obs[obs$pit %in% tag$pit, , drop = FALSE]
  jr  <- setNames(as.numeric(tag$reldt_posix), tag$pit)
  jo  <- as.numeric(obs$obsdt_posix)
  bad <- !is.na(jo) & !is.na(jr[obs$pit]) & jo <= jr[obs$pit]      # pre-release reads
  list(tag = tag, obs = obs[!bad, , drop = FALSE])
}

.cache <- new.env()

# Load every dataset, combine, de-duplicate and pre-compute per-fish checkpoints
get_bundle <- function(data_dir = DATA_DIR) {
  ds <- find_all_datasets(data_dir)
  if (nrow(ds) == 0) return(NULL)
  key <- paste(ds$tag_file, ds$obs_file,
               file.info(ds$tag_file)$mtime, file.info(ds$obs_file)$mtime, collapse = "|")
  if (identical(.cache$key, key)) return(.cache$bundle)

  tags <- list(); obss <- list(); log <- character()
  for (i in seq_len(nrow(ds))) {
    ld <- tryCatch(suppressMessages(load_pit_pair(ds$tag_file[i], ds$obs_file[i])),
                   error = function(e) { log <<- c(log, sprintf("%s: FAILED (%s)", ds$dataset[i], e$message)); NULL })
    if (is.null(ld)) next
    ld$tag$dataset <- ds$dataset[i]
    tags[[length(tags) + 1]] <- ld$tag
    obss[[length(obss) + 1]] <- ld$obs
    log <- c(log, sprintf("%-26s export %s   tags %s   detections %s", ds$dataset[i],
                          format(ds$date[i]), fmt_int(nrow(ld$tag)), fmt_int(nrow(ld$obs))))
  }
  if (length(tags) == 0) return(NULL)

  tag <- do.call(rbind, tags)
  tag <- tag[!duplicated(tag$pit), , drop = FALSE]
  obs <- do.call(rbind, obss)
  obs <- obs[!duplicated(obs[, c("pit", "site", "obsdt")]), , drop = FALSE]
  obs <- obs[obs$pit %in% tag$pit, , drop = FALSE]

  k <- match(tag$relsite, RELEASE$code)
  tag$year       <- lubridate::year(tag$reldt_posix)
  tag$site_label <- ifelse(is.na(k), "Other release site", RELEASE$label[k])
  tag$site_full  <- ifelse(is.na(k), "Other release site", RELEASE$full[k])
  tag$start_idx  <- ifelse(is.na(k), 1, RELEASE$start_idx[k])

  obs$cp <- unname(SITE_TO_CP[obs$site])
  cpobs  <- obs[!is.na(obs$cp) & !is.na(obs$obsdt_posix), , drop = FALSE]
  cp <- cpobs %>%
    group_by(pit, cp) %>%
    summarise(first_det = min(obsdt_posix), last_det = max(obsdt_posix), n = n(), .groups = "drop") %>%
    mutate(idx = match(cp, CHECKPOINTS$cp)) %>%
    as.data.frame()
  site_first <- obs[!is.na(obs$obsdt_posix), c("pit", "site", "obsdt_posix")] %>%
    group_by(pit, site) %>% summarise(first_det = min(obsdt_posix), .groups = "drop") %>% as.data.frame()
  obs_n <- as.data.frame(table(pit = obs$pit), stringsAsFactors = FALSE)
  names(obs_n)[2] <- "n_obs"

  b <- list(tag = tag, cp = cp, site_first = site_first, obs_n = obs_n, datasets = ds, log = log,
            export_date = max(ds$date), n_obs = nrow(obs))
  message("Data loaded:\n  ", paste(log, collapse = "\n  "))
  .cache$key <- key
  .cache$bundle <- b
  b
}


# -----------------------------------------------------------------------------
# 2b. RECAPTURE FILES ("<year> Recapture Year Detail" exports from PTAGIS)
# Every csv / xlsx in data/ whose name contains "Recapture" is read, stacked and
# de-duplicated, so adding another year is just dropping in another file.
# -----------------------------------------------------------------------------
RECAP_SITES <- data.frame(
  code  = c("RREBYP", "RIS", "RI2BYP", "SKATAL", "OSOYOL"),
  label = c("Rocky Reach Dam (bypass)", "Rock Island Dam", "Rock Island Dam (bypass)",
            "Skaha Lake (dam tailrace)", "Osoyoos Lake"),
  lat   = c(47.531237, 47.343278, 47.343278, 49.344503, 49.018503),
  lon   = c(-120.299184, -120.0933, -120.0933, -119.580079, -119.458285),
  stringsAsFactors = FALSE)
# Columbia (Arrow Lakes) release sites are not in the site lists; positions approximate
COLUMBIA_SITES <- data.frame(
  code  = c("CARIBC", "BURTC"),
  label = c("Caribou Creek (Arrow Lakes)", "Burton Creek (Arrow Lakes)"),
  lat   = c(49.33, 49.99), lon = c(-117.75, -117.87), stringsAsFactors = FALSE)
COLUMBIA_LABEL <- "Columbia (Arrow Lakes)"
COLUMBIA_COLOR <- "#3b6fb6"

refresh_recap_coords <- function(data_dir) {
  f <- if (is.na(data_dir)) character() else file.path(data_dir, "MRRSiteList.csv")
  if (length(f) && file.exists(f)) try({
    m <- read.csv(f, fileEncoding = "latin1", check.names = FALSE, stringsAsFactors = FALSE)
    for (nm in c("RECAP_SITES", "COLUMBIA_SITES")) {
      d <- get(nm); i <- match(d$code, m[["MRR Site Info Code"]])
      la <- suppressWarnings(as.numeric(m[["MRR Site Latitude Value"]][i]))
      lo <- suppressWarnings(as.numeric(m[["MRR Site Longitude Value"]][i]))
      ok <- !is.na(la) & !is.na(lo) & !(d$code %in% c("RREBYP"))   # keep the precise bypass location
      d$lat[ok] <- la[ok]; d$lon[ok] <- lo[ok]
      assign(nm, d, envir = globalenv())
    }
  }, silent = TRUE)
}
suppressWarnings(refresh_recap_coords(DATA_DIR))

parse_md <- function(x) {
  if (inherits(x, c("POSIXct", "Date"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- trimws(as.character(x))
  d <- as.Date(x, format = "%m/%d/%Y")
  d2 <- as.Date(x, format = "%Y-%m-%d"); d[is.na(d)] <- d2[is.na(d)]
  d
}
site_code <- function(x) trimws(sub(" - .*$", "", as.character(x)))

find_recapture_files <- function(data_dir = DATA_DIR) {
  if (is.na(data_dir) || !dir.exists(data_dir)) return(character())
  f <- list.files(data_dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  f <- f[grepl("recapture", basename(f), ignore.case = TRUE) & !grepl("^~\\$", basename(f))]
  stem <- tools::file_path_sans_ext(basename(f))
  f[!(grepl("\\.xlsx$", f, ignore.case = TRUE) & stem %in% stem[grepl("\\.csv$", f, ignore.case = TRUE)])]
}

read_recapture_file <- function(f) {
  d <- tryCatch({
    if (grepl("\\.xlsx$", f, ignore.case = TRUE)) as.data.frame(read_excel(f, sheet = 1, col_types = "text"))
    else read.csv(f, fileEncoding = "latin1", check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  }, error = function(e) NULL)
  if (is.null(d) || !all(c("Tag Code", "Recap Site Name") %in% names(d))) return(NULL)
  g <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA_character_, nrow(d))
  rel <- g("Release Date MMDDYYYY"); rc <- g("Recap Date MMDDYYYY")
  if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {   # text cells may hold Excel serial numbers
    fix <- function(z) ifelse(grepl("^[0-9.]+$", z), as.character(as.Date(as.numeric(z), origin = "1899-12-30"), "%Y-%m-%d"), z)
    rel <- fix(rel); rc <- fix(rc)
  }
  data.frame(pit = trimws(d[["Tag Code"]]), rel_date = parse_md(rel), recap_date = parse_md(rc),
             rel_code = site_code(g("Release Site Name")), recap_code = site_code(g("Recap Site Name")),
             recap_file = g("Recap File Name"), source = basename(f), stringsAsFactors = FALSE)
}

load_recaptures <- function(data_dir = DATA_DIR) {
  fs <- find_recapture_files(data_dir)
  if (!length(fs)) return(NULL)
  r <- do.call(rbind, lapply(fs, read_recapture_file))
  if (is.null(r) || !nrow(r)) return(NULL)
  r <- r[!is.na(r$recap_date) & nzchar(r$pit), , drop = FALSE]
  r <- r[!duplicated(r[, c("pit", "recap_date", "recap_code", "recap_file")]), , drop = FALSE]
  k <- match(r$rel_code, RELEASE$code); kc <- match(r$rel_code, COLUMBIA_SITES$code)
  r$group <- ifelse(!is.na(k), r$rel_code, "COLUMBIA")
  r$pop_label <- ifelse(!is.na(k), RELEASE$full[k], COLUMBIA_LABEL)
  r$rel_label <- ifelse(!is.na(k), RELEASE$full[k],
                        ifelse(!is.na(kc), COLUMBIA_SITES$label[kc], r$rel_code))
  r$rel_lat <- ifelse(!is.na(k), RELEASE$lat[k], COLUMBIA_SITES$lat[kc])
  r$rel_lon <- ifelse(!is.na(k), RELEASE$lon[k], COLUMBIA_SITES$lon[kc])
  r$color   <- ifelse(!is.na(k), RELEASE$color[k], COLUMBIA_COLOR)
  j <- match(r$recap_code, RECAP_SITES$code)
  r$recap_label <- ifelse(is.na(j), r$recap_code, RECAP_SITES$label[j])
  r$recap_lat <- RECAP_SITES$lat[j]; r$recap_lon <- RECAP_SITES$lon[j]
  r$year <- as.integer(format(r$rel_date, "%Y"))
  r$days <- as.numeric(r$recap_date - r$rel_date)
  r[order(r$recap_date, r$pit), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# 1b. RIVER ROUTE  (used to move the spotlight fish along the water, not over land)
# Hand-placed vertices along the Okanagan lakes and river, the Okanogan River and
# the Columbia River. Vertices keyed "cp:XXX" / "rel:XXX" take their coordinates
# from the site lists. To use a more exact centreline, put a file named
# RiverPath.csv (columns key, lat, lon; upstream to downstream, same keys) in data/.
# -----------------------------------------------------------------------------
RIVER_MASTER_DEF <- "
key,lat,lon
lake_N,50.255,-119.365
,50.160,-119.510
,50.030,-119.480
lake_K,49.890,-119.500
,49.800,-119.620
,49.740,-119.720
,49.660,-119.665
,49.580,-119.630
rel:OKANR,49.500,-119.613
cp:PEN,49.500,-119.613
,49.420,-119.580
rel:SKATAL,49.345,-119.580
,49.290,-119.545
,49.200,-119.550
cp:OKC,49.114,-119.566
,49.060,-119.520
rel:OSOYOL,49.019,-119.458
,48.990,-119.440
cp:ZSL,48.934,-119.420
,48.700,-119.440
,48.550,-119.500
,48.410,-119.520
,48.360,-119.580
,48.280,-119.700
cp:OKL,48.269,-119.728
,48.100,-119.780
,48.050,-119.900
cp:WEL,47.947,-119.865
,47.850,-119.950
,47.800,-119.990
,47.750,-120.100
,47.670,-120.220
,47.600,-120.280
cp:RRJ,47.532,-120.300
,47.430,-120.320
cp:RIS,47.343,-120.093
,47.150,-120.030
,46.950,-119.990
cp:PRD,46.644,-119.910
,46.640,-119.730
,46.650,-119.500
,46.550,-119.400
,46.470,-119.260
,46.280,-119.270
,46.230,-119.090
,46.080,-118.920
,46.000,-119.020
,45.950,-119.200
cp:MCJ,45.932,-119.300
,45.920,-119.350
,45.900,-119.500
,45.840,-119.700
,45.720,-120.200
,45.720,-120.350
,45.710,-120.450
cp:JDJ,45.710,-120.693
,45.670,-120.830
,45.640,-121.070
cp:TDA,45.619,-121.120
,45.610,-121.180
,45.680,-121.400
,45.710,-121.510
,45.690,-121.880
cp:BON,45.637,-121.965
,45.580,-122.120
,45.540,-122.250
,45.580,-122.400
,45.620,-122.650
,45.660,-122.770
,45.860,-122.800
,46.020,-122.850
,46.140,-122.960
,46.170,-123.100
,46.160,-123.250
cp:EST,46.147,-123.380
"
RIVER_BRANCH_DEF <- list(
  EQUESC = data.frame(lat = c(50.3719, 50.310), lon = c(-119.4924, -119.420), join = "lake_N"),
  MISS3C = data.frame(lat = c(49.860, 49.872), lon = c(-119.4435, -119.470), join = "lake_K"),
  SHINGC = data.frame(lat = c(49.5038, 49.500, 49.492), lon = c(-119.7410, -119.700, -119.640), join = "cp:PEN"))

build_river <- function(data_dir = DATA_DIR) {
  txt <- RIVER_MASTER_DEF
  f <- if (!is.na(data_dir)) file.path(data_dir, "RiverPath.csv") else ""
  if (file.exists(f)) {
    ext <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    need <- paste0("cp:", CHECKPOINTS$cp)
    if (!is.null(ext) && all(c("key", "lat", "lon") %in% names(ext)) && all(need %in% ext$key)) m <- ext
    else { warning("RiverPath.csv ignored: needs key, lat, lon and every cp: key"); m <- read.csv(text = txt, stringsAsFactors = FALSE) }
  } else m <- read.csv(text = txt, stringsAsFactors = FALSE)
  m$key[is.na(m$key)] <- ""
  # exact coordinates for checkpoints and release sites
  for (i in seq_len(nrow(m))) {
    k <- m$key[i]
    if (startsWith(k, "cp:"))  { j <- match(sub("cp:", "", k), CHECKPOINTS$cp); if (!is.na(j)) { m$lat[i] <- CHECKPOINTS$lat[j]; m$lon[i] <- CHECKPOINTS$lon[j] } }
    if (startsWith(k, "rel:")) { j <- match(sub("rel:", "", k), RELEASE$code);  if (!is.na(j)) { m$lat[i] <- RELEASE$lat[j];     m$lon[i] <- RELEASE$lon[j] } }
  }
  km <- function(la, lo) { n <- length(la); if (n < 2) return(0)
  dy <- diff(la) * 111.2; dx <- diff(lo) * 111.2 * cos(mean(la) * pi / 180); c(0, cumsum(sqrt(dx^2 + dy^2))) }
  m$km <- km(m$lat, m$lon)
  list(master = m)
}
RIVER <- build_river(DATA_DIR)

# route from a release site down to a given checkpoint (or the last one), with distance in km
river_route <- function(relcode, to_cp = "EST") {
  m <- RIVER$master
  b <- RIVER_BRANCH_DEF[[relcode]]
  if (!is.null(b)) { j <- match(b$join[1], m$key); start <- j
  pre <- data.frame(key = "", lat = b$lat, lon = b$lon, stringsAsFactors = FALSE)
  } else { start <- match(paste0("rel:", relcode), m$key)
  pre <- NULL }
  end <- match(paste0("cp:", to_cp), m$key)
  if (is.na(start) || is.na(end) || end < start) return(NULL)
  r <- rbind(pre, m[start:end, c("key", "lat", "lon")])
  r$key[1] <- if (is.null(pre)) r$key[1] else "start"
  dy <- diff(r$lat) * 111.2; dx <- diff(r$lon) * 111.2 * cos(mean(r$lat) * pi / 180)
  r$km <- c(0, cumsum(sqrt(dx^2 + dy^2)))
  r
}


# -----------------------------------------------------------------------------
# 2c. PHOTO LIBRARY (read live from a public GitHub repository)
# Photos are uploaded to the repo (browser or phone); the Photos tab lists every
# image in it. Folders in the repo become albums. Change PHOTO_REPO to point
# somewhere else, or set the environment variable PHOTO_REPO.
# -----------------------------------------------------------------------------
PHOTO_REPO <- Sys.getenv("PHOTO_REPO", "MacusOng/sockeye-tracker-photos")
PHOTO_EXT  <- "\\.(jpe?g|png|gif|webp)$"

photo_url <- function(repo, path) {
  enc <- vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLencode, "", reserved = TRUE)
  sprintf("https://raw.githubusercontent.com/%s/HEAD/%s", repo, paste(enc, collapse = "/"))
}
photo_caption <- function(path) {
  b <- tools::file_path_sans_ext(basename(path))
  trimws(gsub("\\s+", " ", gsub("[_-]+", " ", b)))
}
# turn a vector of repo file paths into the photo table
photo_table <- function(paths, repo = PHOTO_REPO) {
  paths <- paths[grepl(PHOTO_EXT, paths, ignore.case = TRUE)]
  if (!length(paths)) return(data.frame(path = character(), album = character(), caption = character(), url = character(), stringsAsFactors = FALSE))
  d <- dirname(paths)
  d <- data.frame(path = paths,
                  album = ifelse(d == ".", "Photos", trimws(gsub("\\s+", " ", gsub("[_-]+", " ", gsub("/", " / ", d))))),
                  caption = vapply(paths, photo_caption, ""),
                  url = vapply(paths, photo_url, "", repo = repo), stringsAsFactors = FALSE)
  d <- d[order(d$album, d$path), , drop = FALSE]; rownames(d) <- NULL; d
}
# list image files in the repo: GitHub tree API first, jsDelivr file list as a backup
fetch_photo_index <- function(repo = PHOTO_REPO) {
  paths <- tryCatch({
    tr <- jsonlite::fromJSON(sprintf("https://api.github.com/repos/%s/git/trees/HEAD?recursive=1", repo))
    tr$tree$path[tr$tree$type == "blob"]
  }, error = function(e) NULL)
  if (is.null(paths)) for (br in c("main", "master")) {
    paths <- tryCatch({
      j <- jsonlite::fromJSON(sprintf("https://data.jsdelivr.com/v1/packages/gh/%s@%s?structure=flat", repo, br))
      sub("^/", "", j$files$name)
    }, error = function(e) NULL)
    if (!is.null(paths)) break
  }
  if (is.null(paths)) stop("could not reach the photo library")
  photo_table(paths, repo)
}

# =============================================================================
# 3. ANALYSIS HELPERS
# =============================================================================

# Detection rate per checkpoint x release site (only fish that can reach it)
rate_by_site <- function(tg, cp) {
  rows <- lapply(seq_len(nrow(CHECKPOINTS)), function(i) {
    ti <- tg[tg$start_idx <= i, , drop = FALSE]
    if (nrow(ti) == 0) return(NULL)
    n_app <- table(ti$site_full)
    di <- cp[cp$idx == i & cp$pit %in% ti$pit, , drop = FALSE]
    n_det <- table(factor(ti$site_full[match(di$pit, ti$pit)], levels = names(n_app)))
    data.frame(cp_idx = i, checkpoint = CHECKPOINTS$label[i], site_full = names(n_app),
               n_applicable = as.integer(n_app), n_detected = as.integer(n_det),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame())
  out$rate <- round(100 * out$n_detected / out$n_applicable, 1)
  out$checkpoint <- factor(out$checkpoint, levels = CHECKPOINTS$label)
  out
}

funnel_from <- function(rate_df) {
  if (nrow(rate_df) == 0) return(data.frame())
  a <- aggregate(cbind(n_applicable, n_detected) ~ cp_idx + checkpoint, data = rate_df, FUN = sum)
  a <- a[order(a$cp_idx), ]
  a$pct <- round(100 * a$n_detected / a$n_applicable, 1)
  a
}

# Per-fish summary: first / farthest checkpoint reached
fish_summary <- function(tg, cp, obs_n) {
  agg <- cp %>%
    group_by(pit) %>%
    summarise(n_cp = n(), first_idx = min(idx), last_idx = max(idx),
              t_first = first_det[which.min(idx)], t_last = first_det[which.max(idx)],
              .groups = "drop")
  out <- tg %>%
    left_join(agg, by = "pit") %>%
    left_join(obs_n, by = "pit")
  out$n_obs[is.na(out$n_obs)] <- 0L
  out$n_cp[is.na(out$n_cp)]   <- 0L
  out$days_to_last <- as.numeric(difftime(out$t_last, out$reldt_posix, units = "days"))
  out$farthest <- ifelse(is.na(out$last_idx), NA_character_, CHECKPOINTS$label[out$last_idx])
  as.data.frame(out)
}

speed_from <- function(cp, tg) {
  a <- cp %>%
    group_by(pit) %>%
    filter(n() >= 2) %>%
    summarise(i1 = min(idx), i2 = max(idx),
              t1 = first_det[which.min(idx)], t2 = first_det[which.max(idx)], .groups = "drop") %>%
    filter(i2 > i1)
  if (nrow(a) == 0) return(data.frame())
  a$km   <- CHECKPOINTS$rkm[a$i1] - CHECKPOINTS$rkm[a$i2]
  a$days <- as.numeric(difftime(a$t2, a$t1, units = "days"))
  a <- a[a$days >= 1 / 24, , drop = FALSE]
  a$pace <- a$km / a$days
  a <- left_join(a, tg[, c("pit", "site_full")], by = "pit")
  a$from <- CHECKPOINTS$label[a$i1]
  a$to   <- CHECKPOINTS$label[a$i2]
  as.data.frame(a[order(-a$pace), ])
}


# -----------------------------------------------------------------------------
# 3a. POPULATION COMPARISONS (p-values)
#     Compares the populations picked in the left pane on three measures:
#       1. share of tagged fish detected at Rocky Reach Dam or farther downstream
#          (Fisher's exact test for 2 populations, chi-squared for 3+)
#       2. days from release to the first Rocky Reach Dam detection
#       3. pace (km/day) between first and last checkpoint
#          (2 and 3: Wilcoxon rank-sum for 2 populations, Kruskal-Wallis for 3+)
#     With 3+ populations, pairwise tests (Holm-adjusted) show which pairs differ.
#     Populations with fewer than MIN_FISH_TEST fish are left out of a test.
# -----------------------------------------------------------------------------
MIN_FISH_TEST <- 3
P_ALPHA       <- 0.05

fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("-")
  if (p < 0.001) "p < 0.001" else if (p < 0.01) sprintf("p = %.3f", p) else sprintf("p = %.2f", p)
}
p_chance <- function(p) {
  if (p < 0.001) "less than 1 time in 1,000"
  else if (p < 0.01) { k <- max(1, round(p * 1000)); sprintf("about %d time%s in 1,000", k, if (k == 1) "" else "s") }
  else if (p > 0.95) "almost every time"
  else { k <- max(1, round(p * 100)); sprintf("about %d time%s in 100", k, if (k == 1) "" else "s") }
}

# Pairwise p-value matrix (lower triangle) -> long data frame
pairs_long <- function(m) {
  if (is.null(m)) return(NULL)
  out <- list()
  for (i in rownames(m)) for (j in colnames(m))
    if (!is.na(m[i, j])) out[[length(out) + 1]] <- data.frame(a = j, b = i, p = m[i, j], stringsAsFactors = FALSE)
  if (length(out)) do.call(rbind, out) else NULL
}

# Numeric measure (days, km/day) compared across populations
test_numeric <- function(x, g) {
  ok <- is.finite(x) & !is.na(g); x <- x[ok]; g <- as.character(g[ok])
  n <- table(g); keep <- names(n)[n >= MIN_FISH_TEST]; dropped <- setdiff(names(n), keep)
  x <- x[g %in% keep]; g <- factor(g[g %in% keep])
  grp <- if (length(x)) data.frame(group = levels(g), n = as.integer(table(g)),
                                   median = as.numeric(tapply(x, g, median)), stringsAsFactors = FALSE)
         else data.frame(group = character(), n = integer(), median = numeric())
  res <- list(kind = "numeric", p = NA_real_, method = NA_character_, groups = grp, dropped = dropped, pairs = NULL)
  if (nlevels(g) < 2) return(res)
  if (nlevels(g) == 2) {
    res$p <- suppressWarnings(wilcox.test(x ~ g, exact = FALSE)$p.value); res$method <- "Wilcoxon rank-sum test"
  } else {
    res$p <- kruskal.test(x, g)$p.value; res$method <- "Kruskal-Wallis test"
    res$pairs <- pairs_long(suppressWarnings(
      pairwise.wilcox.test(x, g, p.adjust.method = "holm", exact = FALSE)$p.value))
  }
  if (!is.finite(res$p)) res$p <- NA_real_
  res
}

# Yes/no measure (detected downstream) compared across populations
test_prop <- function(hit, g) {
  ok <- !is.na(g); hit <- hit[ok]; g <- as.character(g[ok])
  n <- table(g); keep <- names(n)[n >= MIN_FISH_TEST]; dropped <- setdiff(names(n), keep)
  hit <- hit[g %in% keep]; g <- factor(g[g %in% keep])
  grp <- if (length(hit)) data.frame(group = levels(g), n = as.integer(table(g)),
                                     hits = as.integer(tapply(hit, g, sum)), stringsAsFactors = FALSE)
         else data.frame(group = character(), n = integer(), hits = integer())
  grp$pct <- if (nrow(grp)) 100 * grp$hits / grp$n else numeric()
  res <- list(kind = "prop", p = NA_real_, method = NA_character_, groups = grp, dropped = dropped, pairs = NULL)
  if (nlevels(g) < 2) return(res)
  if (all(grp$hits == 0) || all(grp$hits == grp$n)) {        # no variation: nothing to test
    res$p <- 1; res$method <- "no test needed (all populations identical)"; return(res)
  }
  tab <- table(g, factor(hit, levels = c(TRUE, FALSE)))
  if (nlevels(g) == 2) {
    res$p <- fisher.test(tab)$p.value; res$method <- "Fisher's exact test"
  } else {
    ex <- suppressWarnings(chisq.test(tab)$expected)
    if (any(ex < 5)) {
      set.seed(2026)
      res$p <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)$p.value
      res$method <- "chi-squared test (simulated, small counts)"
    } else {
      res$p <- chisq.test(tab)$p.value; res$method <- "chi-squared test"
    }
    res$pairs <- pairs_long(suppressWarnings(
      pairwise.prop.test(grp$hits, grp$n, p.adjust.method = "holm")$p.value))
    if (!is.null(res$pairs)) {   # pairwise.prop.test labels by position; map back to names
      res$pairs$a <- grp$group[as.integer(res$pairs$a)]; res$pairs$b <- grp$group[as.integer(res$pairs$b)]
    }
  }
  if (!is.finite(res$p)) res$p <- NA_real_
  res
}

# All three comparisons for the current selection
compare_pops <- function(tg, cp, speed) {
  rr <- match("RRJ", CHECKPOINTS$cp)
  hit <- tg$pit %in% cp$pit[cp$idx >= rr]
  r1 <- test_prop(hit, tg$site_full)
  r1$title <- "Detected at Rocky Reach Dam or farther downstream"

  a <- cp[cp$cp == "RRJ", c("pit", "first_det"), drop = FALSE]
  a <- left_join(a, tg[, c("pit", "reldt_posix", "site_full")], by = "pit")
  days <- as.numeric(difftime(a$first_det, a$reldt_posix, units = "days"))
  r2 <- test_numeric(days, a$site_full)
  r2$title <- "Days from release to Rocky Reach Dam"; r2$what <- "travel time to Rocky Reach Dam"; r2$unit <- "days"

  r3 <- if (nrow(speed)) test_numeric(speed$pace, speed$site_full) else test_numeric(numeric(), character())
  r3$title <- "Pace between first and last checkpoint"; r3$what <- "pace"; r3$unit <- "km/day"
  list(detect = r1, days = r2, pace = r3)
}

# Plain-language summary of one comparison
pop_blurb <- function(r) {
  g <- r$groups
  if (identical(substr(nz(r$method, ""), 1, 7), "no test"))
    return("Every population had the same result here, so there is no difference to test.")
  if (is.na(r$p)) {
    return(if (nrow(g) < 2) sprintf("Not enough fish to compare: a population needs at least %d fish with this measurement.", MIN_FISH_TEST)
           else "This comparison could not be calculated for the current selection.")
  }
  if (r$kind == "prop") {
    desc <- paste0(sprintf("%.1f%% of %s fish (%s of %s)", g$pct, g$group, fmt_int(g$hits), fmt_int(g$n)), collapse = "; ")
    lead <- sprintf("Share of tagged fish detected at Rocky Reach Dam or farther downstream: %s (%s).", desc, fmt_p(r$p))
  } else if (nrow(g) == 2) {
    lead <- if (r$unit == "days")
      sprintf("Fish from %s took a median of %.1f days to reach Rocky Reach Dam (n = %s), compared with %.1f days for %s (n = %s) (%s).",
              g$group[1], g$median[1], fmt_int(g$n[1]), g$median[2], g$group[2], fmt_int(g$n[2]), fmt_p(r$p))
    else
      sprintf("Fish from %s moved at a median pace of %.1f km/day (n = %s), compared with %.1f km/day for %s (n = %s) (%s).",
              g$group[1], g$median[1], fmt_int(g$n[1]), g$median[2], g$group[2], fmt_int(g$n[2]), fmt_p(r$p))
  } else {
    lo <- which.min(g$median); hi <- which.max(g$median)
    lead <- sprintf("Median %s ranged from %.1f %s (%s) to %.1f %s (%s) across %d populations (%s).",
                    r$what, g$median[lo], r$unit, g$group[lo], g$median[hi], r$unit, g$group[hi], nrow(g), fmt_p(r$p))
  }
  meaning <- if (r$p < P_ALPHA)
    sprintf("If these populations really did not differ, a gap this large would turn up by chance alone %s. That is rare, so the difference is probably real.", p_chance(r$p))
  else
    sprintf("If these populations really did not differ, chance alone would produce a gap this large %s. That is common, so these data cannot show a real difference. The populations may still differ; there may be too few fish to tell.", p_chance(r$p))
  if (nrow(g) > 2 && r$p < P_ALPHA && !is.null(r$pairs))
    meaning <- paste(meaning, "The pairwise table shows which pairs differ.")
  paste(lead, meaning)
}


# Survival comparisons between release groups (Cormack-Jolly-Seber estimates).
# Only the same survival parameter is compared (e.g. "Release to Rocky Reach Dam"),
# using a two-sided z-test on estimate / standard error (Wald test), Holm-adjusted
# across all pairs of that parameter. Detection probabilities and lambda are not
# compared: they are not survival. Unstable estimates are left out.
compare_survival <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(list())
  d <- d[d$type == "Survival", , drop = FALSE]
  out <- list()
  for (par in unique(d$parameter)) {
    x <- d[d$parameter == par, , drop = FALSE]
    left_out <- x$group_label[x$unstable | !is.finite(x$se) | x$se <= 0]
    x <- x[!(x$group_label %in% left_out), , drop = FALSE]
    if (nrow(x) < 2) { if (nrow(x) + length(left_out) >= 2)
      out[[par]] <- list(parameter = par, groups = x, pairs = NULL, dropped = left_out); next }
    cmb <- utils::combn(nrow(x), 2)
    pr <- data.frame(a = x$group_label[cmb[1, ]], b = x$group_label[cmb[2, ]],
                     ea = x$estimate[cmb[1, ]], eb = x$estimate[cmb[2, ]], stringsAsFactors = FALSE)
    z <- (pr$ea - pr$eb) / sqrt(x$se[cmb[1, ]]^2 + x$se[cmb[2, ]]^2)
    pr$p <- p.adjust(2 * pnorm(-abs(z)), method = "holm")
    out[[par]] <- list(parameter = par, groups = x, pairs = pr, dropped = left_out)
  }
  out
}

surv_blurb <- function(r) {
  pr <- r$pairs
  if (is.null(pr) || nrow(pr) == 0)
    return("Not enough release groups with a stable estimate to compare.")
  reach <- paste("from", sub("^Release", "release", sub("^Survival: ", "", r$parameter)))
  bits <- vapply(seq_len(nrow(pr)), function(i) {
    hi <- if (pr$ea[i] >= pr$eb[i]) 1 else 2
    w  <- c(pr$a[i], pr$b[i]); e <- c(pr$ea[i], pr$eb[i])
    ratio <- if (min(e) > 0) sprintf(" (about %.1f times as many)", max(e) / min(e)) else ""
    if (pr$p[i] < P_ALPHA)
      sprintf("%s survived %s at %.0f%%, compared with %.0f%% for %s%s (%s). A gap this large would turn up by chance alone %s if survival were really the same, so the difference is probably real.",
              w[hi], reach, 100 * e[hi], 100 * e[3 - hi], w[3 - hi], ratio, fmt_p(pr$p[i]), p_chance(pr$p[i]))
    else
      sprintf("%s (%.0f%%) and %s (%.0f%%) survived %s at rates these data cannot tell apart (%s): chance alone would give a gap this size %s.",
              pr$a[i], 100 * pr$ea[i], pr$b[i], 100 * pr$eb[i], reach, fmt_p(pr$p[i]), p_chance(pr$p[i]))
  }, "")
  paste(bits, collapse = " ")
}

# Release groups that may still be migrating when the data were exported.
# Threshold per release site: 90th percentile of days from release to the first
# Rocky Reach Dam detection, from that site's earlier release years (needs at least
# MIN_HIST_FISH fish). Without enough history, a calendar fallback is used:
# ONA Fish Hatchery fry rear in a lake for a year, so they are treated as complete
# after July 31 of the year after release; lake and screw-trap fish after August 31
# of the release year. A group is flagged when fewer days have passed between its
# median release date and the data cut-off than its threshold. The cut-off is the
# latest detection in the export(s) the group's fish came from (falls back to the
# export date), so an interrogation file that ends early is accounted for.
MIN_HIST_FISH <- 5
HATCHERY_CODES <- c("EQUESC", "MISS3C", "SHINGC")
migration_flags <- function(all_tag, all_cp, sel_tg, export_date) {
  if (nrow(sel_tg) == 0) return(data.frame())
  rr  <- all_cp[all_cp$cp == "RRJ", c("pit", "first_det")]
  hist <- merge(all_tag[, c("pit", "relsite", "year", "reldt_posix")], rr, by = "pit")
  hist$days <- as.numeric(difftime(hist$first_det, hist$reldt_posix, units = "days"))
  grp <- unique(sel_tg[!is.na(sel_tg$year), c("relsite", "site_full", "year")])
  ld <- merge(all_cp[, c("pit", "last_det")], all_tag[, c("pit", "dataset")], by = "pit")
  ds_cut <- if (nrow(ld)) tapply(as.numeric(ld$last_det), ld$dataset, max, na.rm = TRUE) else numeric()
  out <- lapply(seq_len(nrow(grp)), function(i) {
    g <- grp[i, ]
    rel <- as.Date(median(sel_tg$reldt_posix[sel_tg$relsite == g$relsite & sel_tg$year == g$year], na.rm = TRUE))
    h <- hist$days[hist$relsite == g$relsite & hist$year < g$year & is.finite(hist$days)]
    if (length(h) >= MIN_HIST_FISH) {
      thr <- as.numeric(quantile(h, 0.9)); basis <- sprintf("%d earlier fish from this site", length(h))
    } else {
      end <- if (g$relsite %in% HATCHERY_CODES) as.Date(sprintf("%d-07-31", g$year + 1)) else as.Date(sprintf("%d-08-31", g$year))
      thr <- as.numeric(end - rel); basis <- "calendar rule (not enough earlier years)"
    }
    ds <- unique(sel_tg$dataset[sel_tg$relsite == g$relsite & sel_tg$year == g$year])
    cut <- suppressWarnings(max(ds_cut[names(ds_cut) %in% ds]))
    cutoff <- if (is.finite(cut)) as.Date(as.POSIXct(cut, origin = "1970-01-01", tz = "UTC")) else as.Date(export_date)
    elapsed <- as.numeric(cutoff - rel)
    data.frame(relsite = g$relsite, site_full = g$site_full, year = g$year, released = rel, cutoff = cutoff,
               elapsed = elapsed, threshold = thr, basis = basis,
               flagged = is.finite(elapsed) && is.finite(thr) && elapsed < thr, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# -----------------------------------------------------------------------------
# 3b. TRAVEL-TIME STATISTICS AND SINGLE-RELEASE SURVIVAL (Cormack-Jolly-Seber)
#     Ported from the manual PitPro verification script. Modeled chains:
#       Skaha Lake / Osoyoos Lake / Okanagan Lake : release -> Rocky Reach -> Bonneville
#       ONA Fish Hatchery sites : release -> Penticton -> Okanagan Channel -> Rocky Reach -> Bonneville
#     Bonneville folds in the estuary arrays for detection (survival) but only the
#     dam bypass antennas carry a travel time - the same convention as PitPro.
# -----------------------------------------------------------------------------
CH_RRH <- list(name = "Rocky Reach Dam", codes = "RRJ", tt = "RRJ")
CH_BON <- list(name = "Bonneville Dam", codes = c("B2J", "BCC", "TWX", "PDO", "PDW", "PDC", "PD5", "PD7", "PD8"),
               tt = c("B2J", "BCC"))
CHAINS <- list(
  lake  = list(CH_RRH, CH_BON),
  hatch = list(list(name = "Penticton", codes = c("OKD", "OKP"), tt = c("OKD", "OKP")),
               list(name = "Okanagan Channel", codes = "OKC", tt = "OKC"), CH_RRH, CH_BON)
)
chain_for <- function(relsite) if (relsite %in% c("EQUESC", "MISS3C", "SHINGC")) "hatch" else "lake"

# Harmonic mean with delta-method SE (matches PitPro's harmonic SE)
harm_stats <- function(x) {
  x <- x[is.finite(x) & x > 0]; n <- length(x)
  if (n == 0) return(c(hm = NA_real_, se = NA_real_, n = 0))
  inv <- 1 / x; hm <- n / sum(inv)
  c(hm = hm, se = hm^2 * sd(inv) / sqrt(n), n = n)
}
# Arithmetic mean with SD (PitPro prints the SD in brackets) and SE
arith_stats <- function(x) {
  x <- x[is.finite(x) & x > 0]; n <- length(x)
  if (n == 0) return(c(mean = NA_real_, sd = NA_real_, se = NA_real_, n = 0))
  c(mean = mean(x), sd = sd(x), se = sd(x) / sqrt(n), n = n)
}

# Single-release CJS. H = fish x downstream-site 0/1 detection matrix.
# Parameters: reach survivals phi_1..phi_{K-1}, detections p_1..p_{K-1}, and
# lambda = terminal survival x terminal detection (only their product is estimable).
fit_cjs <- function(H, site_names) {
  Kd <- ncol(H); if (Kd == 0) return(NULL)
  key <- apply(H, 1, paste, collapse = "")
  tabH <- table(key)
  hist_mat <- do.call(rbind, lapply(names(tabH), function(k) as.integer(strsplit(k, "")[[1]])))
  cnt <- as.integer(tabH)
  last_occ <- apply(hist_mat, 1, function(h) { w <- which(h == 1); if (length(w)) max(w) else 0L })
  npar <- if (Kd == 1) 1 else 2 * (Kd - 1) + 1

  negll <- function(theta) {
    if (Kd == 1) { phi <- numeric(0); p <- numeric(0); lambda <- plogis(theta[1]) }
    else {
      phi <- plogis(theta[1:(Kd - 1)]); p <- plogis(theta[Kd:(2 * (Kd - 1))]); lambda <- plogis(theta[npar])
    }
    chi <- numeric(Kd + 1); chi[Kd + 1] <- 1; chi[Kd] <- 1 - lambda
    if (Kd >= 2) for (i in (Kd - 2):0)
      chi[i + 1] <- (1 - phi[i + 1]) + phi[i + 1] * (1 - p[i + 1]) * chi[i + 2]
    ll <- 0
    for (r in seq_len(nrow(hist_mat))) {
      h <- hist_mat[r, ]; L <- last_occ[r]; pr <- 1
      if (L == 0) pr <- chi[1]
      else {
        for (i in seq_len(L)) {
          if (i < Kd) pr <- pr * phi[i] * (if (h[i] == 1) p[i] else (1 - p[i]))
          else        pr <- pr * lambda
        }
        if (L < Kd) pr <- pr * chi[L + 1]
      }
      ll <- ll + cnt[r] * log(pmax(pr, 1e-300))
    }
    -ll
  }

  opt <- NULL
  for (seed in c(0, 0.5, -0.5, 1, -1, 2, -2)) {       # several starts: keep the best optimum
    r <- tryCatch(optim(rep(seed, npar), negll, method = "BFGS", hessian = TRUE,
                        control = list(maxit = 5000, reltol = 1e-12)), error = function(e) NULL)
    if (!is.null(r) && (is.null(opt) || r$value < opt$value)) opt <- r
  }
  if (is.null(opt)) return(NULL)
  est <- plogis(opt$par)
  J   <- diag(est * (1 - est), npar)
  Vth <- tryCatch(solve(opt$hessian), error = function(e) matrix(NA_real_, npar, npar))
  se  <- sqrt(pmax(diag(J %*% Vth %*% t(J)), 0))

  arrow <- "to"
  if (Kd == 1) {
    lab  <- paste0("Lambda: survival x detection at ", site_names[1]); type <- "Lambda"
  } else {
    up   <- c("Release", site_names)[seq_len(Kd - 1)]
    lab  <- c(paste0("Survival: ", up, " ", arrow, " ", site_names[seq_len(Kd - 1)]),
              paste0("Detection probability: ", site_names[seq_len(Kd - 1)]),
              paste0("Lambda: final-reach survival x detection at ", site_names[Kd]))
    type <- c(rep("Survival", Kd - 1), rep("Detection", Kd - 1), "Lambda")
  }
  out <- data.frame(parameter = lab, type = type, estimate = est, se = se, stringsAsFactors = FALSE)
  out$unstable <- out$type != "Detection" &
    (out$estimate > 0.995 | out$estimate < 0.005 | is.na(out$se) | out$se > 0.5)
  out
}

# Everything for one release group: capture counts, travel times, survival fit
survival_group <- function(tg, sf, chain) {
  nm  <- vapply(chain, function(s) s$name, "")
  N   <- nrow(tg)
  rel <- as.numeric(tg$reldt_posix)
  sf  <- sf[sf$pit %in% tg$pit, , drop = FALSE]
  CH  <- matrix(0L, N, length(nm), dimnames = list(tg$pit, nm))
  ft  <- matrix(NA_real_, N, length(nm), dimnames = list(tg$pit, nm))   # first travel-time detection (s)
  for (j in seq_along(chain)) {
    o <- sf[sf$site %in% chain[[j]]$codes, , drop = FALSE]
    if (nrow(o)) { f <- tapply(as.numeric(o$first_det), o$pit, min); CH[names(f), j] <- 1L }
    ot <- sf[sf$site %in% chain[[j]]$tt, , drop = FALSE]
    if (nrow(ot)) { f <- tapply(as.numeric(ot$first_det), ot$pit, min); ft[names(f), j] <- f }
  }
  TT <- (ft - rel) / 86400

  rows <- list()
  add <- function(reach, x) {
    a <- arith_stats(x); h <- harm_stats(x)
    rows[[length(rows) + 1]] <<- data.frame(reach = reach, n = as.integer(a["n"]),
                                            arith_mean = unname(a["mean"]), arith_sd = unname(a["sd"]),
                                            harm_mean = unname(h["hm"]), harm_se = unname(h["se"]), stringsAsFactors = FALSE)
  }
  for (j in seq_along(nm)) add(paste0("Release to ", nm[j]), TT[, j])
  if (length(nm) >= 2) {
    a <- nm[length(nm) - 1]; b <- nm[length(nm)]
    ok <- !is.na(ft[, a]) & !is.na(ft[, b])
    add(paste0(a, " to ", b), (ft[ok, b] - ft[ok, a]) / 86400)
  }
  keep <- colSums(CH) > 0
  list(n = N, tt = do.call(rbind, rows),
       det = data.frame(site = nm, fish_detected = as.integer(colSums(CH)), stringsAsFactors = FALSE),
       cjs = fit_cjs(CH[, keep, drop = FALSE], nm[keep]))
}

# =============================================================================
# 4. PLOTS (ggplot objects; theme applied separately so exports can use light)
# =============================================================================

plot_theme <- function(dark = FALSE) {
  base <- theme_minimal(base_size = 13) + theme(legend.position = "bottom",
                                                plot.title = element_text(face = "bold"))
  if (!dark) return(base + theme(plot.background = element_rect(fill = "white", colour = NA)))
  base + theme(plot.background  = element_rect(fill = "#23272b", colour = NA),
               panel.background = element_rect(fill = "#23272b", colour = NA),
               panel.grid.major = element_line(colour = "#3a4046"),
               panel.grid.minor = element_line(colour = "#2d3237"),
               text             = element_text(colour = "#e8ecef"),
               axis.text        = element_text(colour = "#c4ccd2"),
               strip.text       = element_text(colour = "#e8ecef"),
               legend.text      = element_text(colour = "#e8ecef"))
}
fill_scale <- function() scale_fill_manual(values = POP_FILL, name = NULL, drop = TRUE)

gg_release_year <- function(tg, dark = FALSE) {
  d <- tg %>% count(year, site_full)
  ggplot(d, aes(factor(year), n, fill = site_full)) +
    geom_col(width = 0.7) + fill_scale() +
    labs(title = "Fish released by year and release location", x = "Release year", y = "PIT-tagged fish") +
    plot_theme(dark)
}

gg_rate <- function(rate_df, dark = FALSE) {
  txt <- if (dark) "#e8ecef" else "#222222"
  ggplot(rate_df, aes(checkpoint, rate, fill = site_full)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.8) +
    geom_text(aes(label = paste0(rate, "%")), position = position_dodge2(width = 0.8, preserve = "single"),
              vjust = -0.4, size = 3, colour = txt) +
    fill_scale() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Detection rate by checkpoint", x = NULL, y = "Fish detected (% of fish released upstream)") +
    plot_theme(dark) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

gg_count <- function(rate_df, dark = FALSE) {
  ggplot(rate_df, aes(checkpoint, n_detected, fill = site_full)) +
    geom_col(width = 0.75) + fill_scale() +
    labs(title = "Fish detected by checkpoint", x = NULL, y = "Individual fish detected") +
    plot_theme(dark) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

gg_travel <- function(cp, tg, dark = FALSE) {
  d <- cp %>% inner_join(tg[, c("pit", "reldt_posix", "site_full")], by = "pit")
  d$days <- as.numeric(difftime(d$first_det, d$reldt_posix, units = "days"))
  d <- d[!is.na(d$days) & d$days > 0, , drop = FALSE]
  d$checkpoint <- factor(CHECKPOINTS$label[d$idx], levels = CHECKPOINTS$label)
  ggplot(d, aes(days, fill = site_full)) +
    geom_histogram(bins = 25) + facet_wrap(~checkpoint, scales = "free") + fill_scale() +
    labs(title = "Travel time from release to first detection", x = "Days since release", y = "Fish") +
    plot_theme(dark)
}

gg_weekly <- function(cp, dark = FALSE) {
  d <- cp
  d$week <- as.Date(lubridate::floor_date(d$first_det, "week"))
  d$checkpoint <- factor(CHECKPOINTS$label[d$idx], levels = CHECKPOINTS$label)
  d <- d %>% count(week, checkpoint)
  ggplot(d, aes(week, n, fill = checkpoint)) +
    geom_col(width = 6) +
    scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.9, name = NULL) +
    labs(title = "When fish were first detected at each checkpoint", x = "Week", y = "Fish (first detections)") +
    plot_theme(dark)
}


gg_surv <- function(sv, dark = FALSE) {
  d <- sv[sv$type != "Detection" & !is.na(sv$estimate), , drop = FALSE]
  d$parameter   <- factor(d$parameter, levels = unique(d$parameter[order(match(d$type, c("Survival", "Lambda")))]))
  d$group_label <- factor(d$group_label, levels = rev(sort(unique(d$group_label))))
  d$lo <- pmax(0, d$estimate - d$se); d$hi <- pmin(1, d$estimate + d$se)
  d$flag <- ifelse(d$unstable, "Unstable estimate", "Estimate")
  ggplot(d, aes(estimate, group_label, colour = site_full, shape = flag)) +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.25, na.rm = TRUE) +
    geom_point(size = 3) +
    facet_grid(parameter ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(24)) +
    scale_colour_manual(values = POP_FILL, name = NULL, guide = "none") +
    scale_shape_manual(values = c("Estimate" = 16, "Unstable estimate" = 1), name = NULL) +
    scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
    labs(title = "Survival and lambda (estimate with standard error)", x = "Estimate", y = NULL) +
    plot_theme(dark) + theme(strip.text.y = element_text(angle = 0, hjust = 0))
}

gg_tt_means <- function(tt, dark = FALSE) {
  a <- data.frame(group_label = tt$group_label, reach = tt$reach, type = "Arithmetic mean (bar = SD)",
                  m = tt$arith_mean, err = tt$arith_sd, stringsAsFactors = FALSE)
  h <- data.frame(group_label = tt$group_label, reach = tt$reach, type = "Harmonic mean (bar = SE)",
                  m = tt$harm_mean, err = tt$harm_se, stringsAsFactors = FALSE)
  d <- rbind(a, h); d <- d[!is.na(d$m), , drop = FALSE]
  d$reach <- factor(d$reach, levels = unique(tt$reach))
  d$group_label <- factor(d$group_label, levels = rev(sort(unique(d$group_label))))
  e <- ifelse(is.na(d$err), 0, d$err); d$lo <- pmax(d$m - e, d$m / 4); d$hi <- d$m + e
  ggplot(d, aes(m, group_label, colour = type)) +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.3,
                  position = position_dodge(width = 0.6), na.rm = TRUE) +
    geom_point(position = position_dodge(width = 0.6), size = 2.4) +
    facet_grid(reach ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(18)) +
    scale_x_log10() +
    scale_colour_manual(values = c("Arithmetic mean (bar = SD)" = "#1f7a6d", "Harmonic mean (bar = SE)" = "#e2573f"), name = NULL) +
    labs(title = "Travel time: arithmetic vs harmonic mean", x = "Days (log scale)", y = NULL) +
    plot_theme(dark) + theme(strip.text.y = element_text(angle = 0, hjust = 0))
}

# plot height grows with the number of rows so labels stay readable
plot_rows_height <- function(n_rows, base = 170, per = 30) max(320, base + per * n_rows)

# =============================================================================
# 5. SMALL HTML BUILDERS
# =============================================================================

bar_list <- function(labels, values, fills, right, max_val = NULL, sub = NULL) {
  if (length(labels) == 0) return(div(class = "empty", "Nothing to show for this selection."))
  mx <- nz(max_val, max(values, na.rm = TRUE)); if (!is.finite(mx) || mx <= 0) mx <- 1
  div(class = "barlist", lapply(seq_along(labels), function(i)
    div(class = "barrow",
        div(class = "bl", labels[i]),
        div(class = "bt", div(class = "bf", style = sprintf("width:%.1f%%;background:%s;",
                                                            max(0.4, 100 * values[i] / mx), fills[i]))),
        div(class = "bv", right[i], if (!is.null(sub)) span(class = "bs", sub[i])))))
}

stat_tile <- function(label, value, note = NULL, cls = NULL)
  div(class = paste("tile", cls), div(class = "tl", label), div(class = "tv", value),
      if (!is.null(note)) div(class = "tn", note))

card <- function(title, ..., right = NULL, class = NULL)
  div(class = paste("card", class),
      div(class = "card-h", h3(title), right), ...)

# =============================================================================
# 6. USER INTERFACE
# =============================================================================

APP_CSS <- "
:root{--bg:#f3efe8;--card:#fffdf8;--ink:#1f2a2b;--muted:#6b7776;--line:#e7e1d5;--accent:#1f7a6d;--accent2:#175e54;
--gold:#d4a24c;--red:#e2573f;--blue:#3d7ea6;--soft:#efe9dc;--on-accent:#fff;}
body.dark{--bg:#171a1d;--card:#23272b;--ink:#e8ecef;--muted:#9aa5ab;--line:#343a40;--accent:#3fb3a1;--accent2:#2d8f80;--soft:#2d3237;}
body{background:var(--bg);color:var(--ink);font-family:var(--font,'Segoe UI',system-ui,-apple-system,sans-serif);}
button,select,input,textarea{font-family:inherit;}
.side-tabs{display:flex;gap:4px;background:var(--soft);padding:4px;border-radius:999px;margin-bottom:14px;}
.st-b{flex:1;border:0;background:transparent;color:var(--muted);border-radius:999px;padding:6px 8px;font-weight:600;font-size:13px;}
.st-b.on{background:var(--card);color:var(--ink);box-shadow:0 1px 3px rgba(0,0,0,.18);}
.theme-grid{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-bottom:14px;}
.theme-card{background:var(--soft);border:1px solid var(--line);border-radius:10px;padding:6px;text-align:left;color:var(--ink);cursor:pointer;}
.theme-card:hover{border-color:var(--muted);}
.theme-card.on{border:2px solid var(--accent);padding:5px;}
.theme-card .sw{display:flex;height:26px;border-radius:6px;overflow:hidden;margin-bottom:5px;border:1px solid rgba(127,127,127,.25);}
.theme-card .sw span{flex:1;} .theme-card .tn{font-size:12px;font-weight:700;}
.set-t{font-weight:700;font-size:14px;color:var(--ink);} .set-d{font-size:11.5px;color:var(--muted);line-height:1.4;}
.sun-row{display:flex;justify-content:space-between;gap:10px;align-items:center;margin:4px 0 4px;}
.seg{display:flex;border:1px solid var(--line);border-radius:999px;padding:2px;flex:none;}
.seg-b,.snd-b{border:0;background:transparent;color:var(--muted);border-radius:999px;padding:4px 10px;font-weight:700;font-size:12px;}
.seg-b.on,.snd-b.on{background:var(--accent);color:var(--on-accent);}
#sun_status{margin-bottom:14px;}
#font_pick{background:var(--card);color:var(--ink);border:1px solid var(--line);border-radius:8px;width:100%;padding:6px 8px;margin-top:6px;}
#ss{position:fixed;top:0;left:0;right:0;bottom:0;z-index:100000;background:#000;display:none;cursor:none;overflow:hidden;}
#ss.on{display:block;}
#ss .ss-img{position:absolute;top:0;left:0;right:0;bottom:0;background:#000 center/contain no-repeat;opacity:0;transition:opacity 1.6s ease;}
#ss .ss-img.show{opacity:1;}
#ss .ss-shade{position:absolute;top:0;left:0;right:0;bottom:0;background:linear-gradient(to top,rgba(0,0,0,.7),rgba(0,0,0,0) 45%);}
#ss .ss-clock{position:absolute;left:5vw;bottom:8vh;color:#fff;text-shadow:0 2px 14px rgba(0,0,0,.6);}
#ss .ss-time{font-size:min(15vw,168px);font-weight:300;line-height:1;font-variant-numeric:tabular-nums;letter-spacing:-.02em;}
#ss .ss-time .ss-sec{opacity:.55;font-size:.45em;} #ss .ss-time small{font-size:.2em;font-weight:700;margin-left:.4em;opacity:.8;letter-spacing:.08em;}
#ss .ss-date{font-size:min(3.2vw,28px);opacity:.9;margin-top:8px;}
#ss .ss-cap{position:absolute;right:4vw;bottom:5vh;color:#fff;font-size:15px;opacity:.85;max-width:42vw;text-align:right;text-shadow:0 1px 8px rgba(0,0,0,.7);}
#ss .ss-hint{position:absolute;top:3vh;left:0;right:0;text-align:center;color:rgba(255,255,255,.45);font-size:13px;}
#ss.clock-only{background:var(--bg);} #ss.clock-only .ss-img,#ss.clock-only .ss-shade,#ss.clock-only .ss-cap{display:none;}
#ss.clock-only .ss-clock{left:0;right:0;bottom:auto;top:50%;transform:translateY(-50%);text-align:center;color:var(--ink);text-shadow:none;}
#ss.clock-only .ss-hint{color:var(--muted);}
#ss.photos-only .ss-clock{display:none;}
.ss-row{display:flex;gap:8px;margin-top:6px;} .ss-row select{flex:1;background:var(--card);color:var(--ink);border:1px solid var(--line);border-radius:8px;padding:6px 8px;}
#ss_preview{margin-top:8px;}
.font-prev{border:1px solid var(--line);background:var(--soft);border-radius:10px;padding:10px;font-size:13px;line-height:1.5;margin:8px 0;color:var(--ink);}
.container-fluid{max-width:1400px;padding:0 20px 40px;}
h1,h2,h3,h4{color:var(--ink);}
.topbar{display:flex;justify-content:space-between;align-items:center;padding:18px 0 10px;}
.brand{display:flex;align-items:center;gap:12px;}
.logo{width:46px;height:46px;flex:none;} .logo svg{width:100%;height:100%;display:block;}
.lg-fish{animation:lgswim 3.4s ease-in-out infinite;} .lg-wave{animation:lgwave 5s linear infinite;}
@keyframes lgswim{0%,100%{transform:translate(0,0) rotate(0deg);}50%{transform:translate(1.5px,-1.5px) rotate(-2deg);}}
@keyframes lgwave{from{transform:translateX(0);}to{transform:translateX(16px);}}
@media (prefers-reduced-motion: reduce){.lg-fish,.lg-wave,.tour-ring,.tour-do-ico{animation:none !important;}}
.beta-badge{display:inline-block;vertical-align:middle;margin-left:8px;font-size:10px;font-weight:800;letter-spacing:.12em;padding:2px 8px;border-radius:999px;background:var(--gold);color:#1f1a0e;position:relative;top:-2px;}
.top-right{display:flex;gap:10px;align-items:center;}
.tour-btn{background:var(--card);border:1px solid var(--line);color:var(--ink);border-radius:999px;padding:7px 13px;font-size:12px;font-weight:600;}
.tour-btn:hover{border-color:var(--accent);}
#tour{position:fixed;top:0;left:0;width:0;height:0;z-index:99000;}
.tour-shade{position:fixed;background:rgba(5,10,15,.62);}
.tour-ring{position:fixed;border:3px solid var(--accent);border-radius:12px;pointer-events:none;box-shadow:0 0 0 4px rgba(255,255,255,.22),0 0 22px var(--accent);animation:tourpulse 1.6s ease-in-out infinite;}
@keyframes tourpulse{50%{box-shadow:0 0 0 8px rgba(255,255,255,.1),0 0 32px var(--accent);}}
.tour-card{position:fixed;width:330px;max-width:calc(100vw - 16px);background:var(--card);color:var(--ink);border:1px solid var(--line);border-radius:16px;padding:16px 18px;box-shadow:0 18px 50px rgba(0,0,0,.35);font-size:13.5px;line-height:1.5;}
.tour-card.center{left:50% !important;top:50% !important;transform:translate(-50%,-50%);width:430px;text-align:center;}
.tour-card h4{margin:6px 0;font-size:17px;font-weight:700;color:var(--ink);}
.tour-top{display:flex;justify-content:space-between;align-items:center;font-size:11px;color:var(--muted);font-weight:700;letter-spacing:.06em;text-transform:uppercase;}
.tour-top>span:first-child{white-space:nowrap;margin-right:10px;}
.tour-dots{display:flex;gap:3px;flex-wrap:nowrap;} .tour-dots i{width:9px;height:5px;border-radius:3px;background:var(--line);display:block;}
.tour-dots i.past{background:var(--accent);opacity:.5;} .tour-dots i.now{background:var(--accent);}
.tour-do{display:flex;gap:8px;align-items:flex-start;background:var(--soft);border:1px dashed var(--accent);border-radius:10px;padding:8px 10px;margin:10px 0 2px;text-align:left;}
.tour-do-ico{width:16px;height:16px;border-radius:50%;border:2px solid var(--accent);flex:none;margin-top:2px;position:relative;animation:tourblink 1s ease-in-out infinite;}
@keyframes tourblink{50%{opacity:.3;}}
.tour-do.ok{border-style:solid;} .tour-do.ok .tour-do-ico{background:var(--accent);animation:none;}
.tour-do.ok .tour-do-ico::after{content:'';position:absolute;left:4px;top:1px;width:4px;height:8px;border:solid var(--on-accent);border-width:0 2px 2px 0;transform:rotate(45deg);}
.tour-stuck{display:none;font-size:11.5px;color:var(--muted);margin-top:6px;}
.tour-btns{display:flex;justify-content:space-between;align-items:center;margin-top:12px;gap:8px;}
.tour-card.center .tour-btns{justify-content:center;}
.tour-next{background:var(--accent);color:var(--on-accent);border:0;border-radius:999px;padding:7px 18px;font-weight:700;}
.tour-next:disabled{opacity:.35;cursor:not-allowed;}
.tour-back{background:transparent;border:1px solid var(--line);color:var(--ink);border-radius:999px;padding:6px 14px;}
.tour-card.center{animation:tourpop .45s ease-out;max-height:94vh;overflow:auto;}
@keyframes tourpop{from{opacity:0;transform:translate(-50%,-46%) scale(.96);}to{opacity:1;transform:translate(-50%,-50%) scale(1);}}
.tour-card.center.big{width:min(880px,94vw);padding:22px 26px 20px;font-size:14.5px;}
.tour-card.big h4{font-size:24px;margin:14px 0 6px;}
.tour-river svg{width:100%;height:auto;display:block;border-radius:14px;}
.tour-plan{display:block;max-width:460px;text-align:left;margin:12px auto 0;background:var(--soft);border-radius:12px;padding:10px 16px;}
.tour-plan-h{font-weight:700;margin-bottom:6px;}
.tour-plan-row{display:flex;align-items:center;gap:10px;margin:4px 0;}
.tour-plan-row span{width:22px;height:22px;border-radius:50%;background:var(--accent);color:var(--on-accent);font-weight:700;font-size:12px;display:inline-flex;align-items:center;justify-content:center;flex:none;}
.gloss,.gloss-demo{border-bottom:1px dotted var(--muted);cursor:help;}
.gloss-tip{position:absolute;z-index:9500;display:none;max-width:300px;background:var(--ink);color:var(--bg);font-size:12.5px;line-height:1.45;padding:8px 10px;border-radius:8px;box-shadow:0 6px 18px rgba(0,0,0,.25);}
.tour-medal{display:inline-block;background:var(--gold);color:#1f1a0e;font-size:11px;font-weight:800;letter-spacing:.08em;text-transform:uppercase;padding:3px 10px;border-radius:999px;margin-bottom:4px;}
.tour-badge{width:88px;height:88px;margin:2px auto 6px;} .tour-badge svg{width:100%;height:100%;}
.tour-tip{position:absolute;z-index:9000;width:310px;max-width:calc(100vw - 16px);background:var(--card);color:var(--ink);border:1px solid var(--accent);border-radius:14px;padding:12px 14px;box-shadow:0 10px 30px rgba(0,0,0,.25);font-size:13px;line-height:1.5;}
.tour-tip-h{font-weight:700;font-size:12px;color:var(--accent);text-transform:uppercase;letter-spacing:.06em;margin-bottom:4px;}
.tour-tip-off{font-size:11.5px;color:var(--muted);}
.brand h1{font-size:19px;font-weight:700;margin:0;} .brand p{margin:0;font-size:12px;color:var(--muted);}
.ver{font-size:11px;color:var(--muted);text-align:right;background:var(--card);border:1px solid var(--line);
border-radius:8px;padding:5px 10px;line-height:1.35;}
.layout{display:grid;grid-template-columns:270px minmax(0,1fr);gap:18px;align-items:start;}
@media(max-width:900px){.layout{grid-template-columns:1fr;}}
aside.side{position:sticky;top:12px;background:var(--card);border:1px solid var(--line);border-radius:16px;padding:16px;}
aside.side h4{font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);margin:0 0 10px;}
aside.side .form-group{margin-bottom:12px;} aside.side label{font-size:12px;color:var(--muted);font-weight:600;}
.btn.btn-go{width:100%;background:var(--accent);color:var(--on-accent);border:0;border-radius:10px;font-weight:700;padding:9px;font-size:15px;}
.btn.btn-go:hover,.btn.btn-go:focus{background:var(--accent2);color:var(--on-accent);}
#nerki{position:fixed;right:20px;bottom:20px;z-index:9800;}
.nk-launch{width:66px;height:66px;border-radius:50%;border:0;padding:0;cursor:pointer;position:relative;display:flex;align-items:center;justify-content:center;
  background:linear-gradient(160deg,#1f7a6d,#2a7f9a);box-shadow:0 8px 22px rgba(0,0,0,.22);animation:nkpop .6s ease-out;}
.nk-launch svg{width:84%;height:auto;} .nk-launch.hide{display:none;}
.nk-launch:hover{transform:scale(1.05);} .nk-launch:focus-visible{outline:3px solid var(--gold);outline-offset:3px;}
.nk-ring{position:absolute;top:-6px;left:-6px;right:-6px;bottom:-6px;border-radius:50%;border:2px solid #1f7a6d;opacity:0;animation:nkring 2.8s ease-out infinite;pointer-events:none;}
@keyframes nkring{0%{transform:scale(.85);opacity:.7;}100%{transform:scale(1.25);opacity:0;}}
@keyframes nkpop{from{transform:scale(.4);opacity:0;}to{transform:scale(1);opacity:1;}}
.nk-hello{position:absolute;right:80px;bottom:14px;width:230px;background:var(--card);color:var(--ink);border:1px solid var(--line);border-radius:14px 14px 4px 14px;
  padding:10px 26px 10px 13px;font-size:13.5px;line-height:1.45;box-shadow:0 8px 24px rgba(0,0,0,.15);cursor:pointer;display:none;}
.nk-hello.show{display:block;animation:nkpop .5s ease-out;} .nk-hello b{color:var(--accent);}
.nk-hello-x{position:absolute;top:4px;right:6px;border:0;background:transparent;color:var(--muted);font-size:16px;line-height:1;cursor:pointer;}
.nk-panel{display:none;position:absolute;right:0;bottom:0;width:min(370px,calc(100vw - 24px));height:min(560px,calc(100vh - 40px));background:var(--card);color:var(--ink);
  border:1px solid var(--line);border-radius:18px;box-shadow:0 18px 50px rgba(0,0,0,.25);overflow:hidden;flex-direction:column;}
.nk-panel.open{display:flex;animation:nkpop .3s ease-out;transform-origin:bottom right;}
.nk-head{display:flex;align-items:center;gap:10px;padding:10px 12px;background:linear-gradient(135deg,#145a50,#1f7a6d 60%,#2a7f9a);color:#fff;flex:none;}
.nk-av{width:46px;height:46px;border-radius:50%;background:rgba(255,255,255,.16);display:flex;align-items:center;justify-content:center;flex:none;} .nk-av svg{width:86%;}
.nk-ht{flex:1;line-height:1.25;} .nk-ht b{display:block;font-size:15px;} .nk-ht span{font-size:11px;opacity:.85;}
.nk-close{border:0;background:transparent;color:#fff;font-size:24px;line-height:1;cursor:pointer;padding:0 4px;}
.nk-msgs{flex:1;overflow-y:auto;padding:12px;font-size:13.5px;line-height:1.5;}
.nk-m{background:var(--soft);border-radius:12px 12px 12px 4px;padding:9px 11px;margin-bottom:8px;max-width:90%;word-wrap:break-word;}
.nk-m.me{background:var(--accent);color:var(--on-accent);border-radius:12px 12px 4px 12px;margin-left:auto;}
.nk-go{margin-top:6px;border:1px solid var(--accent);background:transparent;color:var(--accent);border-radius:999px;padding:3px 10px;font-size:12px;cursor:pointer;}
.nk-dots i{display:inline-block;width:6px;height:6px;border-radius:50%;background:var(--muted);margin-right:3px;animation:nkty 1s infinite;}
.nk-dots i:nth-child(2){animation-delay:.15s;} .nk-dots i:nth-child(3){animation-delay:.3s;}
@keyframes nkty{50%{transform:translateY(-4px);opacity:.4;}}
.nk-chips{display:flex;flex-wrap:wrap;gap:6px;padding:4px 12px 8px;flex:none;}
.nk-chip{border:1px solid var(--accent);color:var(--accent);background:transparent;border-radius:999px;padding:4px 10px;font-size:12px;cursor:pointer;text-align:left;}
.nk-chip:hover{background:var(--soft);}
.nk-inp{display:flex;gap:8px;border-top:1px solid var(--line);padding:10px 12px;margin:0;flex:none;}
.nk-inp input{flex:1;border:1px solid var(--line);border-radius:999px;padding:7px 12px;font-size:13px;background:var(--bg);color:var(--ink);min-width:0;}
.nk-send{width:36px;height:36px;border-radius:50%;border:0;background:var(--accent);color:var(--on-accent);flex:none;cursor:pointer;}
@media (prefers-reduced-motion: reduce){.nk-launch,.nk-ring,.nk-panel.open,.nk-hello.show,.nk-dots i{animation:none !important;}}
.btn-report{margin-top:12px !important;font-weight:600;}
.btn-soft{width:100%;background:var(--soft);color:var(--ink);border:1px solid var(--line);border-radius:10px;margin-top:8px;}
table.dataTable tbody tr.selected>*{box-shadow:inset 0 0 0 9999px rgba(31,122,109,.28) !important;color:var(--ink) !important;}
.btn-top{margin:0 0 14px !important;}
.pv{margin-top:14px;padding-top:12px;border-top:1px solid var(--line);}
.pv h4{margin:0 0 6px !important;} .pv .pv-t{font-family:Consolas,monospace;font-size:13px;font-weight:700;color:var(--ink);word-break:break-all;}
.pv .pv-s{font-size:12px;color:var(--muted);margin-bottom:8px;}
.pv .pv-r{display:flex;justify-content:space-between;gap:8px;font-size:12px;padding:3px 0;border-bottom:1px dotted var(--line);}
.pv .pv-r span:first-child{color:var(--muted);} .pv .pv-r span:last-child{font-weight:600;color:var(--ink);text-align:right;}
.hint{font-size:12px;color:var(--red);margin-top:6px;font-weight:600;}
.small-note{font-size:11px;color:var(--muted);margin-top:12px;line-height:1.4;}
.ptest{border-top:1px solid var(--line);padding:14px 0 6px;}
.ptest:first-child{border-top:none;padding-top:4px;}
.ptest h4{margin:0 0 6px;font-size:14px;}
.pbadge{display:inline-block;font-weight:700;font-size:15px;padding:3px 10px;border-radius:999px;margin:2px 8px 6px 0;color:#fff;}
.pbadge.sig{background:var(--accent);color:var(--on-accent);} .pbadge.ns{background:var(--muted);color:var(--bg);}
.pmeth{font-size:12px;color:var(--muted);}
.pblurb{font-size:13px;line-height:1.5;margin:4px 0 8px;}
.ptab{border-collapse:collapse;font-size:12px;margin:4px 0 8px;}
.ptab th,.ptab td{border-bottom:1px solid var(--line);padding:4px 10px;text-align:left;}
.mig-warn{border-left:4px solid var(--gold);background:var(--soft);padding:10px 12px;border-radius:8px;font-size:13px;line-height:1.5;margin:4px 0 12px;}
@media print{
  .layout{display:block !important;}
  aside.side{position:static !important;margin-bottom:16px;break-inside:avoid;}
  a.shiny-download-link,.btn-go,.btn-soft,.tour-btn,#tour,.tour-tip,.gloss-tip,#nerki{display:none !important;}
  .gloss{border-bottom:0;}
  .card{break-inside:avoid-page;}
}
/* tabs as pill bar */
.nav-pills{background:var(--card);border:1px solid var(--line);border-radius:999px;padding:4px;display:inline-flex;flex-wrap:wrap;margin-bottom:16px;}
.nav-pills>li{float:none;} .nav-pills>li>a{border-radius:999px;color:var(--ink);padding:7px 15px;font-size:13px;margin:0;}
.nav-pills>li.active>a,.nav-pills>li.active>a:hover,.nav-pills>li.active>a:focus{background:var(--accent);color:var(--on-accent);}
.nav-pills>li>a:hover{background:var(--soft);}
main,.layout>main{min-width:0;}
.card{min-width:0;max-width:100%;background:var(--card);border:1px solid var(--line);border-radius:16px;padding:18px;margin-bottom:16px;box-shadow:0 1px 2px rgba(0,0,0,.03);}
.card .dataTables_wrapper{max-width:100%;overflow-x:auto;}
.card table.dataTable{width:100% !important;}
.card-h{display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;gap:10px;}
.card-h h3{margin:0;font-size:16px;font-weight:700;}
.sub{color:var(--muted);font-size:13px;margin:-4px 0 12px;}
.hero{background:linear-gradient(135deg,#145a50 0%,#1f7a6d 55%,#2a7f9a 100%);color:#fff;border-radius:22px;padding:28px 30px;margin-bottom:16px;}
.hero .badge2{display:inline-block;background:rgba(255,255,255,.16);border-radius:999px;padding:4px 12px;font-size:11px;letter-spacing:.08em;text-transform:uppercase;}
.hero h2{color:#fff;font-size:32px;font-weight:700;margin:14px 0 8px;line-height:1.15;}
.hero p{color:rgba(255,255,255,.88);max-width:760px;font-size:14px;}
.hero .big{font-size:56px;font-weight:700;line-height:1;margin-top:8px;} .hero .bigl{font-size:12px;opacity:.85;}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin-bottom:16px;}
.tile{background:var(--card);border:1px solid var(--line);border-radius:14px;padding:13px 15px;}
.tl{font-size:10.5px;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);}
.tv{font-size:26px;font-weight:700;margin-top:2px;} .tn{font-size:12px;color:var(--muted);}
.tile.red .tv{color:var(--red);} .tile.teal .tv{color:var(--accent);} .tile.gold .tv{color:var(--gold);}
.barlist{display:flex;flex-direction:column;gap:9px;}
.barrow{display:grid;grid-template-columns:minmax(120px,190px) 1fr auto;gap:12px;align-items:center;font-size:13px;}
.bl{text-align:right;font-weight:600;} .bt{background:var(--soft);border-radius:8px;height:18px;overflow:hidden;}
.bf{height:100%;border-radius:8px;} .bv{font-weight:700;min-width:70px;} .bs{color:var(--muted);font-weight:400;font-size:11px;margin-left:6px;}
.stack{display:flex;height:18px;border-radius:8px;overflow:hidden;background:var(--soft);}
.legend{display:flex;flex-wrap:wrap;gap:6px 16px;margin-top:10px;font-size:12px;color:var(--muted);}
.legend i{display:inline-block;width:10px;height:10px;border-radius:3px;margin-right:6px;}
.empty{color:var(--muted);padding:20px;text-align:center;}
.photo-tools{display:flex;flex-wrap:wrap;gap:14px;align-items:flex-end;margin-bottom:8px;} .photo-tools .form-group{margin-bottom:0;}
.photo-tools .btn{margin-bottom:0;}
.pgrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:14px;margin-top:12px;}
.pitem{background:var(--soft);border:1px solid var(--line);border-radius:12px;overflow:hidden;cursor:pointer;transition:transform .12s;}
.pitem:hover{transform:translateY(-2px);} .pitem img{display:block;width:100%;aspect-ratio:4/3;object-fit:cover;background:var(--line);}
.pitem .cap{padding:8px 10px;font-size:12.5px;color:var(--ink);word-break:break-word;} .pitem .alb{display:block;font-size:11px;color:var(--muted);}
.pmodal img{max-width:100%;max-height:75vh;display:block;margin:0 auto;border-radius:8px;}

.grid2{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:16px;} @media(max-width:1100px){.grid2{grid-template-columns:1fr;}}
.spot{background:linear-gradient(160deg,#145a50,#1b6e63);color:#fff;border-radius:16px;padding:16px;}
.spot h4{color:#fff;font-size:11px;letter-spacing:.1em;text-transform:uppercase;opacity:.8;margin:0 0 8px;}
.spot .pit{font-family:Consolas,monospace;font-weight:700;font-size:15px;} .spot .n{font-size:26px;font-weight:700;}
.spot-slider{margin-top:12px;} .spot-slider .irs-grid{display:none;}
.spot-slider .irs-single,.spot-slider .irs-from,.spot-slider .irs-to{background:#ffd166 !important;color:#333 !important;}
.spot-slider .irs-bar{background:#ffd166 !important;border-color:#ffd166 !important;} .spot-slider .irs-line{background:rgba(255,255,255,.25) !important;border-color:transparent !important;}
.spot-slider .irs-handle>i:first-child{background:#fff !important;} .spot-slider .irs-min,.spot-slider .irs-max{display:none;}
.spot-slider .slider-animate-container{text-align:left;} .spot-slider .slider-animate-button{color:#fff !important;}
.spot-sel label{color:#fff;font-size:11px;opacity:.85;margin-bottom:2px;} .spot-sel .form-group{margin-bottom:8px;}
.spot-sel select{background:rgba(255,255,255,.14);color:#fff;border:1px solid rgba(255,255,255,.3);border-radius:8px;}
.spot-sel select option{color:#222;}
.spot-key{margin-top:12px;font-size:11px;line-height:1.3;} .spot-key div{display:flex;align-items:center;gap:8px;margin-bottom:4px;}
.sk{display:inline-block;width:12px;height:12px;border-radius:50%;flex:none;box-sizing:border-box;}
.sk-dot{background:#e2573f;border:2px solid #fff;} .sk-ring{border:3px solid #ffd166;} .sk-fill{background:#ffd166;border:3px solid #ffd166;}
.spot-note{font-size:11px;opacity:.75;margin-top:10px;line-height:1.35;}
.spot .btn{background:rgba(255,255,255,.14);color:#fff;border:0;border-radius:8px;width:100%;margin-top:10px;}
.rank{display:grid;grid-template-columns:34px minmax(150px,220px) 1fr 100px 250px;gap:12px;align-items:center;background:var(--card);
border:1px solid var(--line);border-radius:12px;padding:10px 14px;margin-bottom:8px;font-size:13px;}
.rank .pit{font-family:Consolas,monospace;font-weight:700;} .rank .m{color:var(--muted);font-size:11px;}
.rank .pace{font-weight:700;text-align:right;} .rank .d{color:var(--muted);font-size:12px;text-align:right;}
.form-control,.selectize-input,.selectize-dropdown{background:var(--card)!important;color:var(--ink)!important;border-color:var(--line)!important;}
.selectize-input>.item,.selectize-input.items .item{background:var(--soft)!important;color:var(--ink)!important;border:1px solid var(--line)!important;}
.selectize-dropdown .option{color:var(--ink);} .selectize-dropdown .active{background:var(--soft)!important;}
label,.radio label{color:var(--ink);}
.dataTables_wrapper,.dataTables_wrapper label,.dataTables_info,.dataTables_paginate a{color:var(--ink)!important;}
table.dataTable tbody tr{background:var(--card)!important;color:var(--ink);} table.dataTable td{border-color:var(--line)!important;}
.table{color:var(--ink);background:transparent;}
.table-striped>tbody>tr:nth-of-type(odd){background-color:var(--soft)!important;color:var(--ink);}
.table-striped>tbody>tr:nth-of-type(even){background-color:transparent!important;}
.table-hover>tbody>tr:hover{background-color:var(--line)!important;} .table>thead>tr>th,.table>tbody>tr>td{border-color:var(--line)!important;}
.btn-default{background:var(--soft);color:var(--ink);border-color:var(--line);}
.irs--shiny .irs-line{background:var(--soft);} .irs-min,.irs-max{background:var(--soft)!important;color:var(--ink)!important;}
body.dark .leaflet-tile-pane{filter:invert(1) hue-rotate(180deg) brightness(.92) contrast(.9);}
.mig-controls{display:grid;grid-template-columns:1fr 190px;gap:16px;align-items:end;}
"

# -----------------------------------------------------------------------------
# Settings: colour themes, follow-the-sun, interface font (all saved per browser)
# -----------------------------------------------------------------------------
mk_theme <- function(id, name, dark, bg, card, soft, line, ink, muted, accent, accent2, on, sw)
  list(id = id, name = name, dark = dark, sw = sw,
       vars = list(bg = bg, card = card, soft = soft, line = line, ink = ink, muted = muted,
                   accent = accent, accent2 = accent2, `on-accent` = on))
THEMES <- list(
  mk_theme("classic",  "Classic",  FALSE, "#f3efe8", "#fffdf8", "#efe9dc", "#e7e1d5", "#1f2a2b", "#6b7776", "#1f7a6d", "#175e54", "#ffffff",
           c("#f3efe8", "#fffdf8", "#efe9dc", "#1f7a6d", "#1f2a2b")),
  mk_theme("light",    "Light",    FALSE, "#eef1f6", "#ffffff", "#e5e9f2", "#d7dce6", "#18243d", "#5d6779", "#c3272f", "#a01f26", "#ffffff",
           c("#eef1f6", "#ffffff", "#e5e9f2", "#c3272f", "#18243d")),
  mk_theme("dark",     "Dark",     TRUE,  "#0c0c0d", "#18181b", "#222226", "#2f2f35", "#f0f0f0", "#a1a1aa", "#c3272f", "#a01f26", "#ffffff",
           c("#0c0c0d", "#18181b", "#222226", "#c3272f", "#f0f0f0")),
  mk_theme("midnight", "Midnight", TRUE,  "#070b18", "#0f1730", "#18223f", "#25315a", "#e8edfb", "#9aa6c8", "#8ab4ff", "#6f9cf0", "#0a1226",
           c("#070b18", "#0f1730", "#1b2547", "#8ab4ff", "#e8edfb")),
  mk_theme("graphite", "Graphite", TRUE,  "#121212", "#1c1c1c", "#262626", "#343434", "#ececec", "#a3a3a3", "#c3272f", "#a01f26", "#ffffff",
           c("#121212", "#1c1c1c", "#262626", "#c3272f", "#ececec")),
  mk_theme("forest",   "Forest",   TRUE,  "#0a120e", "#11211a", "#1a3026", "#25402f", "#e6f4ea", "#9ec2ab", "#6fd39c", "#52b982", "#0a1a12",
           c("#0a120e", "#11211a", "#1a3026", "#6fd39c", "#e6f4ea")),
  mk_theme("sand",     "Sand",     FALSE, "#ebe3d6", "#fbf8f1", "#e4dccb", "#d6cbb5", "#2b2316", "#6f6454", "#8b5a1e", "#6f4716", "#ffffff",
           c("#ebe3d6", "#fbf8f1", "#e4dccb", "#8b5a1e", "#2b2316")),
  mk_theme("contrast", "High Contrast", TRUE, "#000000", "#0b0b0b", "#161616", "#6a6a6a", "#ffffff", "#dcdcdc", "#ffd400", "#e6bf00", "#000000",
           c("#000000", "#0b0b0b", "#161616", "#ffd400", "#ffffff"))
)
names(THEMES) <- vapply(THEMES, function(t) t$id, "")
FONTS <- list(
  list(id = "segoe",   name = "Segoe UI (default)",          stack = "'Segoe UI',system-ui,-apple-system,sans-serif"),
  list(id = "system",  name = "System UI",                   stack = "system-ui,-apple-system,'Segoe UI',Roboto,sans-serif"),
  list(id = "arial",   name = "Arial / Helvetica",           stack = "Arial,Helvetica,sans-serif"),
  list(id = "verdana", name = "Verdana (wide, easy to read)", stack = "Verdana,Geneva,sans-serif"),
  list(id = "trebuc",  name = "Trebuchet MS",                stack = "'Trebuchet MS',Tahoma,sans-serif"),
  list(id = "georgia", name = "Georgia (serif)",             stack = "Georgia,'Times New Roman',serif"),
  list(id = "mono",    name = "Consolas (monospace)",        stack = "Consolas,'Courier New',monospace")
)
SETTINGS_JS <- sprintf("
(function(){
  var T = %s, F = %s, LAT = 49.49, LON = -119.59;
  function ls(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
  function $$(q){ return Array.prototype.slice.call(document.querySelectorAll(q)); }
  function apply(id){
    var t = T[id] || T.classic, b = document.body;
    for (var k in t.vars) b.style.setProperty('--' + k, t.vars[k]);
    b.classList.toggle('dark', !!t.dark); b.setAttribute('data-theme', t.id);
    $$('.theme-card').forEach(function(c){ c.classList.toggle('on', c.getAttribute('data-theme') === t.id); });
    window.__stsDark = !!t.dark;
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('ui_dark', !!t.dark);
  }
  /* sunrise / sunset at Penticton (sunrise equation, ~1-2 min accuracy) */
  function sun(d){
    var r = Math.PI / 180, jd = Date.UTC(d.getFullYear(), d.getMonth(), d.getDate(), 12) / 864e5 + 2440587.5;
    var n = Math.round(jd - 2451545), J = n - LON / 360;
    var M = (357.5291 + 0.98560028 * J) %% 360;
    var C = 1.9148 * Math.sin(M * r) + 0.02 * Math.sin(2 * M * r) + 0.0003 * Math.sin(3 * M * r);
    var L = (M + C + 282.9372) %% 360;
    var Jt = 2451545 + J + 0.0053 * Math.sin(M * r) - 0.0069 * Math.sin(2 * L * r);
    var dec = Math.asin(Math.sin(L * r) * Math.sin(23.44 * r));
    var cw = (Math.sin(-0.833 * r) - Math.sin(LAT * r) * Math.sin(dec)) / (Math.cos(LAT * r) * Math.cos(dec));
    var w = Math.acos(Math.max(-1, Math.min(1, cw))) / r;
    return [(Jt - w / 360 - 2440587.5) * 864e5, (Jt + w / 360 - 2440587.5) * 864e5];
  }
  function hm(ms){ return new Date(ms).toLocaleTimeString('en-GB', {hour: '2-digit', minute: '2-digit', timeZone: 'UTC'}) + ' UTC'; }
  function sunTick(){
    var st = document.getElementById('sun_status');
    if (ls('sts_sun') !== 'auto') { if (st) st.textContent = ''; return; }
    var s = sun(new Date()), now = Date.now(), up = now >= s[0] && now < s[1];
    apply(up ? 'light' : 'dark');
    if (st) st.textContent = 'Penticton today: sunrise ' + hm(s[0]) + ', sunset ' + hm(s[1]) + '. Now showing ' + (up ? 'Light' : 'Dark') + '.';
  }
  function setSun(m){
    ls('sts_sun', m);
    $$('.seg-b').forEach(function(b){ b.classList.toggle('on', b.getAttribute('data-sun') === m); });
    if (m === 'auto') sunTick(); else { apply(ls('sts_theme') || 'classic'); sunTick(); }
  }
  function setFont(id){
    var f = F.filter(function(x){ return x.id === id; })[0] || F[0];
    document.body.style.setProperty('--font', f.stack); ls('sts_font', f.id);
    var sel = document.getElementById('font_pick'); if (sel) sel.value = f.id;
  }
  function pane(p){
    $$('.st-b').forEach(function(b){ b.classList.toggle('on', b.getAttribute('data-pane') === p); });
    var a = document.getElementById('side_filters'), b = document.getElementById('side_settings');
    if (a) a.style.display = p === 'filters' ? '' : 'none';
    if (b) b.style.display = p === 'settings' ? '' : 'none';
  }
  document.addEventListener('DOMContentLoaded', function(){
    $$('.theme-card').forEach(function(c){ c.addEventListener('click', function(){
      ls('sts_theme', c.getAttribute('data-theme')); setSun('off'); }); });
    $$('.seg-b').forEach(function(b){ b.addEventListener('click', function(){ setSun(b.getAttribute('data-sun')); }); });
    $$('.st-b').forEach(function(b){ b.addEventListener('click', function(){ pane(b.getAttribute('data-pane')); }); });
    var sel = document.getElementById('font_pick');
    if (sel) sel.addEventListener('change', function(){ setFont(sel.value); });
    setFont(ls('sts_font') || 'segoe');
    setSun(ls('sts_sun') || 'off');
    setInterval(sunTick, 5 * 60 * 1000);
  });
  $(document).on('shiny:connected', function(){ Shiny.setInputValue('ui_dark', !!window.__stsDark); });
})();
", jsonlite::toJSON(THEMES, auto_unbox = TRUE), jsonlite::toJSON(FONTS, auto_unbox = TRUE))

# Soft synthesized pops and chimes (Web Audio, no sound files, nothing downloaded).
# Browsers keep a page silent until the first click, tap or key press; after that,
# sounds play. The Settings pane has an On/Off switch saved per browser (sts_snd).
SOUND_JS <- "
(function(){
  var ctx = null;
  function ls(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
  function on(){ return ls('sts_snd') !== '0'; }
  function ac(){
    var C = window.AudioContext || window.webkitAudioContext; if (!C) return null;
    if (!ctx) ctx = new C();
    if (ctx.state === 'suspended') { try { ctx.resume(); } catch(e) {} }
    return ctx;
  }
  function tone(f0, f1, d, type, v, at){
    var c = ac(); if (!c) return;
    var t = c.currentTime + (at || 0);
    var o = c.createOscillator(), g = c.createGain();
    o.type = type || 'sine';
    o.frequency.setValueAtTime(f0, t);
    if (f1 && f1 !== f0) o.frequency.exponentialRampToValueAtTime(f1, t + d);
    g.gain.setValueAtTime(0.0001, t);
    g.gain.exponentialRampToValueAtTime(v || 0.12, t + 0.012);
    g.gain.exponentialRampToValueAtTime(0.0001, t + d);
    o.connect(g); g.connect(c.destination);
    o.start(t); o.stop(t + d + 0.05);
  }
  var S = {
    pop:   function(){ tone(420, 980, 0.1, 'sine', 0.14); },
    blub:  function(){ tone(320, 180, 0.12, 'sine', 0.11); tone(640, 430, 0.09, 'sine', 0.05, 0.05); },
    tick:  function(){ tone(1300, 1300, 0.03, 'triangle', 0.05); },
    go:    function(){ tone(340, 700, 0.16, 'sine', 0.1); },
    chime: function(){ tone(880, 880, 0.16, 'sine', 0.08); tone(1318, 1318, 0.22, 'sine', 0.06, 0.1); },
    yay:   function(){ tone(523, 523, 0.1, 'sine', 0.08); tone(659, 659, 0.1, 'sine', 0.08, 0.1); tone(784, 784, 0.18, 'sine', 0.09, 0.2); },
    warn:  function(){ tone(460, 320, 0.22, 'sine', 0.07); }
  };
  window.appSnd = {
    on: on,
    set: function(v){ ls('sts_snd', v ? '1' : '0'); },
    play: function(nm){ if (!on()) return; try { if (S[nm]) S[nm](); } catch(e) {} }
  };
  ['pointerdown', 'keydown', 'touchstart'].forEach(function(ev){
    document.addEventListener(ev, function f(){ document.removeEventListener(ev, f); if (on()) ac(); }, {passive: true});
  });
  document.addEventListener('DOMContentLoaded', function(){
    var bs = Array.prototype.slice.call(document.querySelectorAll('.snd-b'));
    function paint(){ var v = on() ? 'on' : 'off'; bs.forEach(function(b){ b.classList.toggle('on', b.getAttribute('data-snd') === v); }); }
    bs.forEach(function(b){ b.addEventListener('click', function(){
      window.appSnd.set(b.getAttribute('data-snd') === 'on'); paint();
      if (on()) window.appSnd.play('pop');
    }); });
    paint();
    document.addEventListener('click', function(e){
      if (!e.target.closest) return;
      if (e.target.closest('#go')) { window.appSnd.play('go'); return; }
      if (e.target.closest('a.shiny-download-link')) window.appSnd.play('chime');
    }, true);
  });
  var prevW = 0, lastW = 0;
  $(document).on('shiny:value', function(){
    setTimeout(function(){
      var n = document.querySelectorAll('.mig-warn').length;
      if (n > 0 && prevW === 0 && Date.now() - lastW > 30000) { lastW = Date.now(); window.appSnd.play('warn'); }
      prevW = n;
    }, 60);
  });
  $(document).on('shown.bs.tab', function(e){
    var el = e.target; if (el && el.closest && el.closest('#tabs')) window.appSnd.play('tick');
  });
})();
"

# Screensaver: after the page sits idle, show a UTC clock and/or a slideshow of the
# photo library (MacusOng/sockeye-tracker-photos). Any mouse move, click, key or touch
# returns to the dashboard. It runs in the browser only and does not keep the
# shinyapps.io session awake.
SS_JS <- "
(function(){
  var P = [], idx = 0, timer = null, slide = null, tick = null, active = false, sx = null, sy = null, layer = 0;
  function ls(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
  function el(id){ return document.getElementById(id); }
  function mode(){ return ls('sts_ss') || 'both'; }
  function mins(){ var m = parseFloat(ls('sts_ss_min') || '5'); return isFinite(m) && m > 0 ? m : 5; }
  function pad(n){ return (n < 10 ? '0' : '') + n; }
  function clock(){
    var d = new Date();
    el('ss_time').innerHTML = pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()) +
      '<span class=\"ss-sec\">:' + pad(d.getUTCSeconds()) + '</span><small>UTC</small>';
    el('ss_date').textContent = d.toLocaleDateString('en-GB', {timeZone: 'UTC', weekday: 'long', day: 'numeric', month: 'long', year: 'numeric'});
  }
  function shuffle(a){ for (var i = a.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)), t = a[i]; a[i] = a[j]; a[j] = t; } return a; }
  function next(){
    if (!P.length) return;
    var p = P[idx % P.length]; idx++;
    var im = new Image();
    im.onload = function(){
      if (!active) return;
      var a = el('ss_img' + layer), b = el('ss_img' + (1 - layer));
      b.style.backgroundImage = 'url(\"' + p.url + '\")'; b.classList.add('show'); a.classList.remove('show');
      layer = 1 - layer; el('ss_cap').textContent = p.caption || '';
    };
    im.src = p.url;
  }
  function start(force){
    if (window.__tourActive && !force) return;
    var m = force || mode(); if (m === 'off') return;
    var photos = (m === 'photos' || m === 'both') && P.length > 0;
    el('ss').className = 'on ' + (photos ? (m === 'photos' ? 'photos-only' : 'both') : 'clock-only');
    active = true; sx = null; clock(); clearInterval(tick); tick = setInterval(clock, 1000);
    clearInterval(slide); if (photos) { shuffle(P); idx = 0; next(); slide = setInterval(next, 8000); }
  }
  function stop(){
    active = false; el('ss').className = ''; clearInterval(tick); clearInterval(slide);
    ['ss_img0', 'ss_img1'].forEach(function(i){ el(i).classList.remove('show'); });
    arm();
  }
  function arm(){ clearTimeout(timer); if (window.__tourActive) { timer = setTimeout(arm, 30000); return; } if (mode() !== 'off') timer = setTimeout(function(){ start(); }, mins() * 60000); }
  function activity(e){
    if (active) {
      if (e.type === 'mousemove') {
        if (sx === null) { sx = e.clientX; sy = e.clientY; return; }
        if (Math.abs(e.clientX - sx) + Math.abs(e.clientY - sy) < 12) return;
      }
      stop(); return;
    }
    arm();
  }
  function register(){
    if (window.Shiny && Shiny.addCustomMessageHandler && !window.__ssReg) {
      window.__ssReg = true;
      Shiny.addCustomMessageHandler('ss_photos', function(d){ P = Array.isArray(d) ? d : []; });
    }
  }
  register();
  document.addEventListener('DOMContentLoaded', function(){
    register();
    ['mousemove', 'mousedown', 'keydown', 'touchstart', 'wheel', 'scroll'].forEach(function(t){
      document.addEventListener(t, activity, true); });
    var m = el('ss_mode'), n = el('ss_min'), pv = el('ss_preview');
    if (m) { m.value = mode(); m.addEventListener('change', function(){ ls('sts_ss', m.value); arm(); }); }
    if (n) { n.value = String(mins()); n.addEventListener('change', function(){ ls('sts_ss_min', n.value); arm(); }); }
    if (pv) pv.addEventListener('click', function(ev){ ev.stopPropagation(); setTimeout(function(){ start(mode() === 'off' ? 'both' : mode()); }, 250); });
    arm();
  });
})();
"

# -----------------------------------------------------------------------------
# Walk-through (game-style tutorial). Required on a browser's first visit and
# again whenever TOUR_VERSION changes (bump it after a big layout change).
# Do-it steps unlock only when the user performs the action. After the tour,
# each tab shows a one-time tip the first time it is opened.
# -----------------------------------------------------------------------------
TOUR_VERSION <- "2"

logo_svg <- function(s = "a") HTML(sprintf(
'<svg viewBox="0 0 64 64" role="img" aria-label="Sockeye salmon logo">
<defs><linearGradient id="lgw%1$s" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#3a93ad"/><stop offset="1" stop-color="#145a50"/></linearGradient>
<clipPath id="lgc%1$s"><circle cx="32" cy="32" r="30"/></clipPath></defs>
<circle cx="32" cy="32" r="30" fill="url(#lgw%1$s)"/>
<g clip-path="url(#lgc%1$s)">
<path class="lg-wave" d="M-16 47 q8 -4 16 0 t16 0 t16 0 t16 0 t16 0 t16 0 v30 h-96z" fill="#0d4038" opacity=".55"/>
<g class="lg-fish">
<path d="M15 33 L5 24.5 C7.5 30 7.5 36 5 41.5 Z" fill="#a8261f"/>
<path d="M27 22.5 L31 16.5 L36 22 Z" fill="#a8261f"/>
<path d="M14 33 C20 22 33 19.5 43 24.5 C48 27 51.5 29.5 54 32 C51.5 35 48 37.5 43 38.5 C33 42 21 41 14 33 Z" fill="#d6392d"/>
<path d="M42.5 24.3 C48 26.6 51.5 29.6 54 32 C51.5 35 48.2 37.4 43.2 38.4 C44.6 34 44.4 29 42.5 24.3 Z" fill="#2f6b4f"/>
<path d="M53.6 32.4 C56.2 34 55.8 37.2 52.2 36.6" fill="none" stroke="#2f6b4f" stroke-width="1.8" stroke-linecap="round"/>
<path d="M43.2 26.2 C45 29.8 45 33.6 43.4 37.2" fill="none" stroke="#1d4a35" stroke-width="1.1"/>
<circle cx="48.6" cy="30" r="1.6" fill="#fff"/><circle cx="48.9" cy="30" r=".8" fill="#111"/>
</g></g>
<circle cx="32" cy="32" r="30" fill="none" stroke="rgba(255,255,255,.35)" stroke-width="1.5"/>
</svg>', s))

# Animated river intro for the welcome window: a sockeye swims from the Okanagan
# past the dams to the ocean while each checkpoint lights up (not to scale).
river_svg <- function() {
  x0 <- 60; x1 <- 700
  xs <- seq(x0, x1, length.out = 161); ys <- 118 + 24 * sin((xs - x0) / 92)
  cum <- c(0, cumsum(sqrt(diff(xs)^2 + diff(ys)^2))); tot <- max(cum)
  pth <- paste0("M", sprintf("%.1f %.1f", xs[1], ys[1]), paste0(" L", sprintf("%.1f %.1f", xs[-1], ys[-1]), collapse = ""))
  nm <- c("Okanagan release", "Okanagan Channel", "Zosel Dam", "Wells Dam", "Rocky Reach Dam", "McNary Dam", "Bonneville Dam", "Pacific Ocean")
  idx <- round(seq(1, length(xs), length.out = length(nm)))
  cps <- vapply(seq_along(nm), function(k) {
    j <- idx[k]; t <- 0.85 * cum[j] / tot; t1 <- min(t + 0.012, 0.99); t2 <- min(t + 0.07, 0.995)
    up <- k %% 2 == 1; ly <- if (up) ys[j] - 20 else ys[j] + 30
    sprintf(paste0(
      '<circle cx="%.1f" cy="%.1f" r="7" fill="#7f97a0" stroke="#fff" stroke-width="2">',
      '<animate attributeName="fill" dur="10s" repeatCount="indefinite" values="#7f97a0;#7f97a0;#ffd166;#ffd166" keyTimes="0;%.3f;%.3f;1"/></circle>',
      '<circle cx="%.1f" cy="%.1f" r="7" fill="none" stroke="#ffd166" stroke-width="2" opacity="0">',
      '<animate attributeName="r" dur="10s" repeatCount="indefinite" values="7;7;20;20" keyTimes="0;%.3f;%.3f;1"/>',
      '<animate attributeName="opacity" dur="10s" repeatCount="indefinite" values="0;0;.9;0;0" keyTimes="0;%.3f;%.3f;%.3f;1"/></circle>',
      '<text x="%.1f" y="%.1f" text-anchor="middle" font-size="12" font-weight="600" fill="#eaf6f8">%s</text>'),
      xs[j], ys[j], t, t1, xs[j], ys[j], t, t2, t, t1, t2, xs[j], ly, nm[k])
  }, "")
  HTML(paste0(
    '<svg viewBox="0 0 760 230" role="img" aria-label="Animation of a sockeye migrating from the Okanagan to the Pacific Ocean">',
    '<defs><linearGradient id="rvbg" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#18413c"/><stop offset="1" stop-color="#0f2f45"/></linearGradient></defs>',
    '<rect width="760" height="230" rx="16" fill="url(#rvbg)"/>',
    '<path d="M0 70 L40 34 L72 58 L104 22 L146 64 L176 46 L214 78 L0 78 Z" fill="#24584f" opacity=".7"/>',
    '<path d="M640 200 q15 -8 30 0 t30 0 t30 0 t30 0" fill="none" stroke="#5fb3c9" stroke-width="2" opacity=".5"/>',
    '<text x="24" y="214" font-size="11" fill="#9fc7cf">Okanagan, BC</text>',
    '<text x="736" y="24" font-size="11" fill="#9fc7cf" text-anchor="end">Columbia River to the Pacific (not to scale)</text>',
    '<path d="', pth, '" fill="none" stroke="#3f8fa8" stroke-width="12" stroke-linecap="round" opacity=".55"/>',
    '<path d="', pth, '" fill="none" stroke="#bfe6ef" stroke-width="2" stroke-dasharray="6 10" opacity=".8">',
    '<animate attributeName="stroke-dashoffset" from="32" to="0" dur="1.2s" repeatCount="indefinite"/></path>',
    paste(cps, collapse = ""),
    '<g><g transform="scale(.62) translate(-30,-32)">',
    '<path d="M15 33 L5 24.5 C7.5 30 7.5 36 5 41.5 Z" fill="#a8261f"/><path d="M27 22.5 L31 16.5 L36 22 Z" fill="#a8261f"/>',
    '<path d="M14 33 C20 22 33 19.5 43 24.5 C48 27 51.5 29.5 54 32 C51.5 35 48 37.5 43 38.5 C33 42 21 41 14 33 Z" fill="#e0453a"/>',
    '<path d="M42.5 24.3 C48 26.6 51.5 29.6 54 32 C51.5 35 48.2 37.4 43.2 38.4 C44.6 34 44.4 29 42.5 24.3 Z" fill="#3a8a63"/>',
    '<circle cx="48.6" cy="30" r="1.6" fill="#fff"/></g>',
    '<animateMotion dur="10s" repeatCount="indefinite" rotate="auto" calcMode="linear" keyPoints="0;1;1" keyTimes="0;.85;1" path="', pth, '"/></g>',
    '</svg>'))
}

TOUR_STEPS <- list(
  list(center = TRUE, big = TRUE, river = as.character(river_svg()), next_label = "Let's go",
       title = "Welcome to Let's Follow the Sockeye",
       body = paste0("Follow PIT-tagged juvenile sockeye from the Okanagan, past the Columbia River dams, to the Pacific. This site is in <b>beta</b>, so features and numbers may change.",
                     "<div class=tour-plan><div class=tour-plan-h>In this 3-minute walk-through you will work a real example:</div>",
                     "<div class=tour-plan-row><span>1</span>Choose Osoyoos Lake fish released in 2025</div>",
                     "<div class=tour-plan-row><span>2</span>Follow one fish down the river</div>",
                     "<div class=tour-plan-row><span>3</span>Export a plot and a data table</div></div>")),
  list(sel = ".side-tabs", place = "right", title = "Two panels, one switch",
       body = "The left panel has two sides. <b>Filters</b> choose which fish you are looking at. <b>Settings</b> changes how the site looks. You will visit both."),
  list(sel = "pop", place = "right", doit = "check", check = "pop_osoyoos", auto = TRUE, title = "Choose Osoyoos Lake",
       body = "This list picks which fish you look at: one release site, any mix of sites, or <b>All Populations</b>. Hatchery sites and the lakes are never pooled.",
       action = "Open the list and choose <b>Osoyoos Lake</b> only.", nice = "Osoyoos Lake selected."),
  list(sel = "#year_ui", place = "right", doit = "check", check = "one_year", auto = TRUE, title = "Pick one release year",
       body = "Years and dates work together: a fish must match both. Picking years now moves the date range to match, so the two can't contradict each other.",
       action = "Keep only <b>2025</b>: click each other year and press Delete or Backspace.", nice = "One year selected."),
  list(sel = "#date_ui", place = "right", title = "Release dates",
       body = "The range has moved to the release dates in your year. Narrow it further if you only want part of the season. If years and dates ever stop overlapping, a red warning appears under Go."),
  list(sel = "#go", place = "right", doit = "click", click = "#go", auto = TRUE, title = "Press Go to apply",
       body = "Changes in the left panel do nothing until you press <b>Go</b>, so you can set several filters first. If you change something and forget, a red reminder appears under the button.",
       action = "Press Go.", nice = "Now showing your selection."),
  list(sel = "#side_summary", place = "right", title = "Your selection at a glance",
       body = "This box sums up the selection: fish released, how many were detected downstream, the farthest checkpoint reached and the median days to Bonneville Dam."),
  list(sel = "#tiles", pre = "Overview", place = "bottom", title = "The Overview",
       body = "Headline numbers for your selection. Scroll down for releases by year and the season survival funnel: the share of fish detected at each checkpoint on the way to the ocean."),
  list(sel = "#tabs", place = "bottom", doit = "tab", tab = "Journeys", auto = TRUE, title = "Follow a fish",
       body = "Each tab shows the same selection a different way. <b>Journeys</b> lists every fish detected downstream of where it was released.",
       action = "Open the <b>Journeys</b> tab.", nice = "Here are the fish that made it downstream."),
  list(sel = "#tbl_journeys", place = "top", doit = "row", auto = TRUE, title = "Pick a fish",
       body = "Each row is one PIT tag: where it was released, the farthest checkpoint it reached and how many days that took.",
       action = "Click any row to follow that fish.", nice = "Its journey is shown above the table."),
  list(sel = "#journey_detail", place = "bottom", title = "One fish's journey",
       body = "Every checkpoint this fish passed, in order, with the date it was detected. One PIT tag tells the story from the Okanagan towards the ocean."),
  list(sel = "#tabs", place = "bottom", doit = "tab", tab = "Charts", auto = TRUE, title = "Charts",
       body = "Charts summarise the whole selection: releases, detection rates by checkpoint, travel time and weekly detections.",
       action = "Open the <b>Charts</b> tab.", nice = "Next, save one of these charts."),
  list(sel = "#dl_p_rate", place = "right", doit = "click", click = "#dl_p_rate", auto = TRUE, title = "Export a plot",
       body = "Every chart can be saved as a PNG. Its footer records the population, years, version and time (UTC), so you always know what the file shows.",
       action = "Press <b>Export plot (PNG)</b> under Detection rate.", nice = "Plot saved to your Downloads folder."),
  list(sel = "#tabs", place = "bottom", doit = "tab", tab = "Explore", auto = TRUE, title = "Explore the records",
       body = "<b>Explore</b> shows every tagged fish in the selection as a table you can search and sort.",
       action = "Open the <b>Explore</b> tab.", nice = "Last export to go."),
  list(sel = "#dl_fish", place = "right", doit = "click", click = "#dl_fish", auto = TRUE, title = "Export the data",
       body = "Save exactly what is in the table as a CSV, ready for Excel or R.",
       action = "Press <b>Export table (CSV)</b>.", nice = "Table saved to your Downloads folder."),
  list(sel = ".st-b[data-pane=settings]", place = "right", doit = "click", click = ".st-b[data-pane=settings]", auto = TRUE, title = "Make it yours",
       body = "<b>Settings</b> holds the colour themes, Follow the sun (light by day, dark at night in Penticton), the interface font and the screensaver. Choices are saved in this browser only.",
       action = "Switch to Settings.", nice = "Try a theme any time. Filters is one click away."),
  list(sel = ".ver", place = "left", title = "Beta, version and plain-language help",
       body = "This site is in <b>beta</b>: numbers can change as new data arrive. All times are UTC. Words with a <span class=gloss-demo>dotted underline</span>, such as p-value or lambda, show a plain-language definition when you hover over them."),
  list(sel = "#tour_btn", place = "left", title = "Need a refresher?",
       body = "Press <b>Tour</b> any time to replay this walk-through."),
  list(center = TRUE, badge = as.character(logo_svg("f")), next_label = "Start exploring", final = TRUE,
       title = "Tutorial complete!",
       body = "You chose a population, followed a fish towards the ocean and exported your first plot and table. Explore freely now, and press Tour any time to replay.")
)
TOUR_TIPS <- list(
  Overview = "The big picture for your selection: how many fish were tagged, where they were released, and the season survival funnel showing the share of fish detected at each checkpoint downstream.",
  Explore = "Every tagged fish in your selection as a searchable table. Type a tag code, site or year in the search box, sort by any column, and save it with Export table (CSV).",
  Charts = "Ready-made charts: fish released, detection rate and counts by checkpoint, travel time and weekly detections. Each has an Export plot (PNG) button; the saved file records your selection in its footer.",
  Journeys = "Fish detected downstream of their release site. Click a row in the table to trace that fish checkpoint by checkpoint.",
  Migration = "Press play or drag the season date to watch fish reach each checkpoint on the map. The panel on the right follows a single fish; press Another fish to pick a new one.",
  Recaptures = "Fish caught again after release and let go a second time. Shown for reference only: they do not change detection rates, travel times or survival.",
  Speed = "Pace in km per day for every fish seen at two or more checkpoints. Switch between Fastest and Slowest; the bottom card compares median pace by release location, with a p-value when two or more populations are selected.",
  Survival = "Travel time, survival between checkpoints and lambda from the Cormack-Jolly-Seber model, plus p-values comparing the populations you picked. Read the gold warning if a group may still be migrating.",
  Photos = "Field photos from the shared photo library. Filter by album, search by name, and click a photo to enlarge it. New uploads appear after Refresh photos."
)

TOUR_JS <- paste0("
(function(){
  var S = ", jsonlite::toJSON(TOUR_STEPS, auto_unbox = TRUE), ", TIPS = ", jsonlite::toJSON(TOUR_TIPS, auto_unbox = TRUE), ", TV = '", TOUR_VERSION, "';
  var i = 0, done = false, stuck = null, poll = null, root = null, card = null, ring = null, shades = [];
  var CHECKS = {
    pop_osoyoos: function(){ var v = $('#pop').val() || []; return v.length === 1 && v[0] === 'OSOYOL'; },
    one_year: function(){ var v = $('#years').val() || []; return v.length === 1; }
  };
  function ls(k, v){ try { if (v === undefined) return localStorage.getItem(k); localStorage.setItem(k, v); } catch(e) { return null; } }
  function q(s){ return document.querySelector(s); }
  function target(st){
    if (!st.sel) return null;
    if (st.sel === 'pop') { var b = q('button[data-id=pop]'); return b ? (b.closest('.bootstrap-select') || b) : null; }
    var el = q(st.sel); if (el && st.sel === '#tbl_journeys') el = el.querySelector('table') || el;
    return el;
  }
  function holeRect(el){
    var r = el.getBoundingClientRect(), x1 = r.left, y1 = r.top, x2 = r.right, y2 = r.bottom;
    var ex = document.querySelectorAll('.bootstrap-select.open .dropdown-menu, .bootstrap-select.show .dropdown-menu, .selectize-dropdown, .datepicker-dropdown');
    for (var j = 0; j < ex.length; j++) {
      var er = ex[j].getBoundingClientRect();
      if (er.width < 2 || er.height < 2 || getComputedStyle(ex[j]).display === 'none') continue;
      x1 = Math.min(x1, er.left); y1 = Math.min(y1, er.top); x2 = Math.max(x2, er.right); y2 = Math.max(y2, er.bottom);
    }
    return {l: x1 - 6, t: Math.max(y1 - 6, 0), r: x2 + 6, b: Math.min(y2 + 6, window.innerHeight)};
  }
  function box(e, l, t, w, h){ e.style.display = 'block'; e.style.left = l + 'px'; e.style.top = t + 'px'; e.style.width = Math.max(0, w) + 'px'; e.style.height = Math.max(0, h) + 'px'; }
  function layout(){
    if (!root) return;
    var st = S[i], el = target(st), W = window.innerWidth, H = window.innerHeight, k;
    if (st.check && !done && CHECKS[st.check] && CHECKS[st.check]()) complete();
    if (st.center || !el || !el.offsetParent) {
      box(shades[0], 0, 0, W, H); for (k = 1; k < 4; k++) shades[k].style.display = 'none';
      ring.style.display = 'none'; card.classList.add('center'); card.style.left = ''; card.style.top = ''; return;
    }
    var h = holeRect(el);
    box(shades[0], 0, 0, W, h.t); box(shades[1], 0, h.b, W, H - h.b);
    box(shades[2], 0, h.t, h.l, h.b - h.t); box(shades[3], h.r, h.t, W - h.r, h.b - h.t);
    box(ring, h.l, h.t, h.r - h.l, h.b - h.t);
    card.classList.remove('center');
    var cw = card.offsetWidth, ch = card.offsetHeight, g = 16, x, y;
    var fits = {right: h.r + g + cw <= W - 8, left: h.l - g - cw >= 8, bottom: h.b + g + ch <= H - 8, top: h.t - g - ch >= 8};
    var order = [st.place || 'right', 'right', 'bottom', 'top', 'left'], pick = 'bottom';
    for (k = 0; k < order.length; k++) if (fits[order[k]]) { pick = order[k]; break; }
    if (pick === 'right') { x = h.r + g; y = h.t; } else if (pick === 'left') { x = h.l - g - cw; y = h.t; }
    else if (pick === 'bottom') { x = h.l; y = h.b + g; } else { x = h.l; y = h.t - g - ch; }
    card.style.left = Math.max(8, Math.min(x, W - cw - 8)) + 'px';
    card.style.top = Math.max(8, Math.min(y, H - ch - 8)) + 'px';
  }
  function showTab(name){
    var a = q('#tabs a[data-value=\"' + name + '\"]');
    if (a && !(a.parentNode && a.parentNode.classList.contains('active'))) $(a).tab('show');
  }
  function render(){
    var st = S[i], n = S.filter(function(s){ return !s.center; }).length;
    var k = S.slice(0, i + 1).filter(function(s){ return !s.center; }).length, h = '', d;
    done = !st.doit; clearTimeout(stuck);
    if (st.pre) showTab(st.pre);
    card.className = 'tour-card' + (st.big ? ' big' : '');
    if (!st.center) {
      h += '<div class=tour-top><span>Step ' + k + ' of ' + n + '</span><span class=tour-dots>';
      for (d = 1; d <= n; d++) h += '<i class=' + (d < k ? 'past' : (d === k ? 'now' : 'todo')) + '></i>';
      h += '</span></div>';
    } else if (st.river) h += '<div class=tour-river>' + st.river + '</div>';
    else if (st.badge) h += '<div class=tour-badge>' + st.badge + '</div>' + (st.final ? '<div class=tour-medal>' + n + ' of ' + n + ' steps complete</div>' : '');
    h += '<h4>' + st.title + '</h4><div class=tour-body>' + st.body + '</div>';
    if (st.doit) h += '<div class=tour-do id=tour_do><span class=tour-do-ico></span><span><b>Your move:</b> ' + st.action + '</span></div>' +
                      '<a href=# class=tour-stuck id=tour_stuck>Stuck? Continue anyway</a>';
    h += '<div class=tour-btns>' + ((i > 0 && !st.center) ? '<button type=button class=tour-back id=tour_back>Back</button>' : '<span></span>') +
         '<button type=button class=tour-next id=tour_next' + (done ? '' : ' disabled') + '>' + (st.next_label || 'Next') + '</button></div>';
    card.innerHTML = h;
    q('#tour_next').onclick = function(){ if (done) go(i + 1); };
    if (q('#tour_back')) q('#tour_back').onclick = function(){ go(i - 1); };
    if (q('#tour_stuck')) q('#tour_stuck').onclick = function(e){ e.preventDefault(); go(i + 1); };
    setTimeout(function(){ var el = target(st); if (el && !st.center && el.scrollIntoView) el.scrollIntoView({block: 'center'}); layout(); }, st.pre ? 350 : 0);
    layout(); setTimeout(layout, 80);
    if (st.doit) stuck = setTimeout(function(){ var s = q('#tour_stuck'); if (s) s.style.display = 'inline-block'; }, 30000);
  }
  function complete(){
    var st = S[i]; if (done || !st.doit) return; done = true; if (window.appSnd) appSnd.play('yay');
    var dd = q('#tour_do'); if (dd) { dd.classList.add('ok'); dd.innerHTML = '<span class=tour-do-ico></span><span><b>Nice!</b> ' + (st.nice || '') + '</span>'; }
    var nb = q('#tour_next'); if (nb) nb.disabled = false;
    var sk = q('#tour_stuck'); if (sk) sk.style.display = 'none';
    var at = i; if (st.auto) setTimeout(function(){ if (root && i === at) go(i + 1); }, 1500);
  }
  function go(n){ if (n < 0) return; if (n >= S.length) return finish(); i = n; render(); }
  function finish(){
    ls('sts_tour', TV); clearInterval(poll); clearTimeout(stuck);
    ['Overview', 'Journeys', 'Charts', 'Explore'].forEach(function(t){ ls('sts_tip_' + t, '1'); });
    if (root) root.parentNode.removeChild(root); root = null; window.__tourActive = false;
    var f = q('.st-b[data-pane=filters]'); if (f) f.click();
  }
  function start(){
    if (root) return;
    var old = q('.tour-tip'); if (old) old.parentNode.removeChild(old);
    window.__tourActive = true; i = 0; shades = [];
    root = document.createElement('div'); root.id = 'tour';
    for (var k = 0; k < 4; k++) { var s = document.createElement('div'); s.className = 'tour-shade'; root.appendChild(s); shades.push(s); }
    ring = document.createElement('div'); ring.className = 'tour-ring'; root.appendChild(ring);
    card = document.createElement('div'); card.className = 'tour-card'; root.appendChild(card);
    document.body.appendChild(root);
    render(); poll = setInterval(layout, 250);
  }
  function tip(a){
    if (root || ls('sts_tour') !== TV || ls('sts_tips_off') === '1' || !a) return;
    var name = a.getAttribute('data-value') || a.textContent.trim(), t = TIPS[name];
    if (!t || ls('sts_tip_' + name) === '1') return;
    var old = q('.tour-tip'); if (old) old.parentNode.removeChild(old);
    var d = document.createElement('div'); d.className = 'tour-tip';
    d.innerHTML = '<div class=tour-tip-h>Tip: ' + name + '</div><div class=tour-body>' + t + '</div>' +
      '<div class=tour-btns><a href=# class=tour-tip-off>Hide all tips</a><button type=button class=tour-next>Got it</button></div>';
    document.body.appendChild(d);
    var r = a.getBoundingClientRect();
    d.style.left = Math.max(8, Math.min(r.left + window.scrollX, window.scrollX + window.innerWidth - d.offsetWidth - 8)) + 'px';
    d.style.top = (r.bottom + window.scrollY + 12) + 'px';
    d.querySelector('.tour-next').onclick = function(){ ls('sts_tip_' + name, '1'); d.parentNode.removeChild(d); };
    d.querySelector('.tour-tip-off').onclick = function(e){ e.preventDefault(); ls('sts_tips_off', '1'); d.parentNode.removeChild(d); };
  }
  document.addEventListener('click', function(e){
    if (!root || done) return;
    var st = S[i], t = e.target;
    if (!st.doit || !t.closest) return;
    if (st.doit === 'click' && st.click && t.closest(st.click)) complete();
    if (st.doit === 'row') { var tr = t.closest('#tbl_journeys tbody tr'); if (tr && !tr.querySelector('.dataTables_empty')) setTimeout(complete, 200); }
  }, true);
  window.addEventListener('resize', layout);
  window.addEventListener('scroll', layout, true);
  $(document).on('shown.bs.tab', function(e){
    if (root) { var st = S[i]; if (st.doit === 'tab' && (!st.tab || e.target.getAttribute('data-value') === st.tab)) complete(); }
    else tip(e.target);
  });
  function boot(force){
    var tries = 0;
    (function wait(){
      if ((q('#tabs') && q('button[data-id=pop]')) || tries > 60) { if (force || ls('sts_tour') !== TV) start(); return; }
      tries++; setTimeout(wait, 300);
    })();
  }
  document.addEventListener('DOMContentLoaded', function(){
    var b = q('#tour_btn'); if (b) b.addEventListener('click', function(){ boot(true); });
    boot(false);
  });
})();
")

# Glossary: technical terms on the page get a dotted underline and a plain-language
# definition on hover (or tap). Applied to the main content after each update.
GLOSSARY <- list(
  "p-value" = "How often chance alone would produce a gap at least this large if the groups really did not differ. Under 0.05 usually counts as a real difference.",
  "Cormack-Jolly-Seber" = "A mark-recapture model that separates survival from the chance of detection, using each fish's sequence of detections at downstream checkpoints.",
  "lambda" = "Survival through the last reach multiplied by the chance of being detected at the last checkpoint. The two cannot be separated, so lambda is not survival on its own.",
  "harmonic mean" = "An average that gives less weight to the slowest fish; the usual way PIT-tag studies report travel time.",
  "arithmetic mean" = "The ordinary average: add up all the values and divide by the number of fish.",
  "detection probability" = "The chance that a fish passing a checkpoint was actually read by its antennas.",
  "standard error" = "How uncertain an estimate is. The true value is usually within about two standard errors of the estimate.",
  "standard deviation" = "How spread out individual fish are around the average.",
  "Holm-adjusted" = "P-values corrected for making several comparisons at once, so chance differences are not over-counted.",
  "PIT tag" = "Passive integrated transponder: a rice-sized tag with a unique code, read by antennas as the fish swims past.",
  "PIT-tagged" = "Carrying a PIT tag: a rice-sized tag with a unique code, read by antennas as the fish swims past.",
  "Fisher's exact test" = "Tests whether two groups differ in the share of fish detected. Exact, so it works with small numbers.",
  "chi-squared test" = "Tests whether three or more groups differ in the share of fish detected.",
  "Wilcoxon rank-sum test" = "Tests whether two groups differ in a measurement such as travel days, without assuming a bell-shaped spread.",
  "Kruskal-Wallis test" = "The three-or-more-group version of the Wilcoxon rank-sum test.",
  "z-test" = "Compares two model estimates using their standard errors."
)
GLOSS_JS <- paste0("
(function(){
  var G = ", jsonlite::toJSON(GLOSSARY, auto_unbox = TRUE), ", keys = Object.keys(G).sort(function(a, b){ return b.length - a.length; });
  var low = {}; keys.forEach(function(k){ low[k.toLowerCase()] = G[k]; });
  var re = new RegExp('(^|[^A-Za-z])(' + keys.join('|') + ')(?![A-Za-z])', 'gi');
  var SKIP = {SCRIPT: 1, STYLE: 1, TEXTAREA: 1, INPUT: 1, SELECT: 1, OPTION: 1, BUTTON: 1, A: 1, svg: 1, SVG: 1};
  function wrap(node){
    var txt = node.nodeValue, m, last = 0, frag = null; re.lastIndex = 0;
    while ((m = re.exec(txt)) !== null) {
      if (!frag) frag = document.createDocumentFragment();
      var start = m.index + m[1].length;
      frag.appendChild(document.createTextNode(txt.slice(last, start)));
      var s = document.createElement('span'); s.className = 'gloss'; s.setAttribute('data-def', low[m[2].toLowerCase()]); s.textContent = m[2];
      frag.appendChild(s); last = start + m[2].length;
    }
    if (frag) { frag.appendChild(document.createTextNode(txt.slice(last))); node.parentNode.replaceChild(frag, node); }
  }
  function scan(){
    var main = document.querySelector('main'); if (!main) return;
    var w = document.createTreeWalker(main, NodeFilter.SHOW_TEXT, {acceptNode: function(n){
      for (var p = n.parentNode; p && p !== main; p = p.parentNode) {
        if (SKIP[p.nodeName] || (p.classList && (p.classList.contains('gloss') || p.classList.contains('leaflet-container')))) return NodeFilter.FILTER_REJECT;
      }
      re.lastIndex = 0; return re.test(n.nodeValue) ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_SKIP;
    }});
    var list = [], n; while ((n = w.nextNode())) list.push(n);
    list.forEach(wrap);
  }
  var t = null; function later(){ clearTimeout(t); t = setTimeout(scan, 300); }
  var tipEl = null;
  function show(el){
    if (!tipEl) { tipEl = document.createElement('div'); tipEl.className = 'gloss-tip'; document.body.appendChild(tipEl); }
    tipEl.innerHTML = '<b>' + el.textContent + '</b>: ' + el.getAttribute('data-def'); tipEl.style.display = 'block';
    var r = el.getBoundingClientRect(), w = tipEl.offsetWidth;
    tipEl.style.left = Math.max(8, Math.min(r.left + window.scrollX, window.scrollX + window.innerWidth - w - 8)) + 'px';
    tipEl.style.top = (r.bottom + window.scrollY + 6) + 'px';
  }
  function hide(){ if (tipEl) tipEl.style.display = 'none'; }
  document.addEventListener('mouseover', function(e){ var g = e.target.closest && e.target.closest('.gloss'); if (g) show(g); });
  document.addEventListener('mouseout', function(e){ if (e.target.closest && e.target.closest('.gloss')) hide(); });
  document.addEventListener('click', function(e){ var g = e.target.closest && e.target.closest('.gloss'); if (g) show(g); else hide(); });
  $(document).on('shiny:value shown.bs.tab', later);
  if (window.jQuery) $(function(){ later(); setInterval(scan, 4000); });
})();
")

# -----------------------------------------------------------------------------
# Nerki: the built-in helper (no AI service, no key, nothing sent anywhere).
# A visitor's question is matched to a curated answer bank (TF-IDF similarity on
# sample questions, with synonyms) and a few "live" answers are calculated from
# the current selection. Unmatched questions are written to the server log as
# "NERKI_UNANSWERED: ..." so new answers can be added. To plug in a real AI
# service later, only nerki_answer() in the server needs to change.
# -----------------------------------------------------------------------------
NERKI_CONTACT <- ""   # e.g. "email name@example.com" - shown when Nerki can't help

nk <- function(id, q, a, goto = NULL, rel = NULL, live = NULL) list(id = id, q = q, a = a, goto = goto, rel = rel, live = live)
NERKI_KB <- list(
  # ---- live answers (worked out from the current selection) ----
  nk("live_released", c("how many fish were released", "how many fish were tagged", "number of fish released", "how many fish are in my selection"), "", live = "released", rel = c("live_reached", "sites")),
  nk("live_reached", c("how many fish reached bonneville", "how many fish made it to the ocean", "how many fish were detected at wells dam", "how many fish got to rocky reach", "how many reached mcnary"), "", live = "reached", rel = c("why_undetected", "detect_vs_surv")),
  nk("live_days", c("how long does it take to reach bonneville", "how many days to rocky reach", "travel time to mcnary", "how long to get to the ocean", "how long does migration take"), "", live = "days", rel = c("harmonic", "travel_time")),
  nk("live_fast", c("which fish is fastest", "fastest fish", "what is the fastest pace", "slowest fish", "quickest fish"), "", live = "fast", rel = c("speed_tab", "hatch_slow")),
  nk("live_selection", c("what am i looking at", "what is selected", "current selection", "what does this page show"), "", live = "selection", rel = c("choose_pop", "go")),
  nk("live_survival", c("what is the survival rate", "how many fish survive", "survival for my selection", "survival estimate"), "", live = "survival", rel = c("lambda", "detect_vs_surv")),
  nk("live_pvalue", c("what does this p value mean", "is the difference significant", "are the populations different", "explain this p value"), "", live = "pvalue", rel = c("pvalue", "holm")),
  nk("live_data", c("how recent is the data", "when was the data updated", "how old is the data", "latest data export"), "", live = "data", rel = c("upload_lag", "data_source")),
  nk("live_zero", c("why is it zero", "why are there no detections", "why is equesis zero", "why do hatchery fish show zero", "no fish detected downstream"), "", live = "zero", rel = c("still_migrating", "why_undetected")),

  # ---- using the site ----
  nk("help", c("what can you help with", "what can you do", "who are you", "what is nerki"),
     "I'm Nerki, a young sockeye and your helper here. I can explain how to use each part of the dashboard, what the numbers and terms mean, and how fish tracking with PIT tags works. I can also look up a few numbers for your current selection, such as how many fish reached Bonneville Dam or which fish was fastest. I'm a built-in guide in beta, so if I don't understand, try other words or tap a suggestion.",
     rel = c("how_use", "pit", "live_reached")),
  nk("about_site", c("what is this site", "what does this dashboard do", "what is lets follow the sockeye", "what is this for"),
     "Let's Follow the Sockeye tracks juvenile sockeye from the Okanagan as they head down the Columbia River to the Pacific. Each fish carries a PIT tag, and antennas at dams and river sites record it as it swims past. The dashboard turns those detections into counts, travel times, survival estimates and maps. It is in beta, so features and numbers can change.",
     rel = c("how_use", "pit", "beta")),
  nk("how_use", c("how do i use this site", "how does this work", "getting started", "where do i start"),
     "Start in the left panel: choose one or more populations, pick release years and press <b>Go</b>. Then use the tabs across the top: <b>Overview</b> for the big picture, <b>Journeys</b> to follow one fish, <b>Charts</b> and <b>Survival</b> for the numbers. Press <b>Tour</b> at the top right for a guided walk-through.",
     rel = c("choose_pop", "go", "tour")),
  nk("choose_pop", c("how do i choose a population", "how do i pick a release site", "how do i select a lake", "change population"),
     "Open the <b>Population</b> list in the left panel, tick the release sites you want, then press <b>Go</b>. You can pick one site, any mix, or All Populations.",
     rel = c("all_pop", "compare", "sites")),
  nk("all_pop", c("what does all populations do", "select everything", "how to select all sites"),
     "All Populations is a shortcut for every release site at once. Picking it clears the individual sites, and ticking all six sites switches back to All Populations by itself.",
     rel = c("compare", "not_pooled")),
  nk("compare", c("how do i compare two populations", "compare skaha and osoyoos", "compare lakes", "compare release sites"),
     "Pick two or more release sites in the Population list and press <b>Go</b>. The <b>Survival</b> tab then shows whether they differ, with p-values and a plain-language note, and <b>Speed</b> compares their pace. Compare groups from the same release year, and remember hatchery fry need about a year before they migrate.",
     goto = "Survival", rel = c("pvalue", "still_migrating")),
  nk("years", c("how do i choose a year", "release year filter", "remove a year", "pick one year"),
     "The <b>Release year</b> box holds the years you want. Click a year and press Delete or Backspace to remove it, or click the box to add one back. Choosing years also moves the date range to match. Press <b>Go</b> to apply.",
     rel = c("dates", "go")),
  nk("dates", c("what do release dates do", "date range", "filter by date", "change the dates"),
     "Release dates narrow the selection to fish released between two dates. Years and dates work together: a fish must match both. If they stop overlapping, a red warning appears under Go.",
     rel = c("years", "go")),
  nk("go", c("why does nothing change", "what does the go button do", "my changes do not show", "press go"),
     "Changes in the left panel only take effect when you press <b>Go</b>, so you can set several filters first. If you change something and forget, a red reminder appears under the button. Go also loads any newer data.",
     rel = c("choose_pop", "years")),
  nk("export_plot", c("how do i export a plot", "download a chart", "save a graph as png", "save the figure"),
     "Every chart has an <b>Export plot (PNG)</b> button underneath it. The file name and a footer record the population, years, version and time (UTC), so you always know what it shows.",
     goto = "Charts", rel = c("export_table", "report")),
  nk("export_table", c("how do i export the data", "download a csv", "export table to excel", "download the data"),
     "Tables have an <b>Export table (CSV)</b> button. <b>Explore</b> has the full list of tagged fish in your selection. CSV files open in Excel or R.",
     goto = "Explore", rel = c("export_plot", "report")),
  nk("report", c("how do i make a report", "summary report", "export everything as pdf", "make a pdf"),
     "Press <b>Summary report</b> in the left panel. You get one file with the headline numbers, charts, survival and p-values for your selection. Open it in a browser and use Print, then Save as PDF, if you want a PDF.",
     rel = c("export_plot", "export_table")),
  nk("search_tag", c("how do i search for a tag", "find a specific fish", "look up a pit tag code", "search for a fish"),
     "Go to <b>Explore</b> and type part of the tag code, a release site or a year into the search box. <b>Journeys</b> has the same search for fish detected downstream.",
     goto = "Explore", rel = c("follow_fish", "columns")),
  nk("columns", c("what do the columns mean", "explain the table columns", "what is in the table"),
     "Each row is one tagged fish: its PIT tag code, where and when it was released, how many times it was detected, the farthest checkpoint it reached and how long that took. Hover over a dotted term for a definition.",
     rel = c("farthest", "search_tag")),
  nk("follow_fish", c("how do i follow one fish", "track a single fish", "see one fish journey", "fish journey"),
     "Open <b>Journeys</b> and click any row. Above the table you will see every checkpoint that fish passed, in order, with dates. The <b>Migration</b> tab also has a spotlight that animates one fish down the river.",
     goto = "Journeys", rel = c("spotlight", "farthest")),
  nk("migration", c("how does the migration animation work", "what does the play button do", "migration map", "watch the migration"),
     "On <b>Migration</b>, press play or drag the season date to watch fish reach each checkpoint over the season. The map lights up checkpoints as fish arrive.",
     goto = "Migration", rel = c("spotlight", "checkpoint")),
  nk("spotlight", c("what is the spotlight fish", "another fish button", "follow a fish on the map", "red dot on the map"),
     "The panel on the right of <b>Migration</b> follows a single fish. The red dot is where it is on the river, the hollow gold ring is the next site where it will be detected, and filled gold marks sites it has already passed. Press <b>Another fish</b> to pick a new one.",
     goto = "Migration", rel = c("follow_fish", "migration")),
  nk("speed_tab", c("what does km per day mean", "how is speed calculated", "what is pace", "how fast do fish swim"),
     "Pace is the river distance between a fish's first and last checkpoint divided by the days it took, in kilometres per day. It only covers fish seen at two or more checkpoints.",
     goto = "Speed", rel = c("live_fast", "hatch_slow")),
  nk("hatch_slow", c("why do hatchery fish look slower", "why are some populations slower", "why is one site slower"),
     "Pace is measured between checkpoints, so time spent rearing in a lake before migrating is not counted. Release sites start at different places, though: hatchery fish are first picked up at Penticton and still pass Skaha and Vaseux lakes, where the water moves slowly, while Osoyoos Lake fish start near Zosel Dam. Compare groups over similar stretches of river.",
     rel = c("speed_tab", "distance")),
  nk("photos_add", c("how do i add photos", "upload a photo", "add my pictures"),
     "Photos come from a shared photo library on GitHub. The site owner uploads them there, and they appear after pressing <b>Refresh photos</b>. Folders become albums and file names become captions.",
     goto = "Photos", rel = c("photos_src")),
  nk("photos_src", c("where do photos come from", "who took these photos", "photo library"),
     "The Photos tab reads from a public photo library kept by the site owner. During tagging, fish welfare comes first, so photos are taken when there is time.",
     goto = "Photos", rel = c("photos_add")),
  nk("theme", c("how do i change the colours", "dark mode", "change theme", "night mode"),
     "Switch the left panel to <b>Settings</b> and pick a theme: Classic, Light, Dark, Midnight, Graphite, Forest, Sand or High Contrast. Your choice is saved in this browser only.",
     rel = c("followsun", "font")),
  nk("followsun", c("what is follow the sun", "automatic dark mode", "switch theme at sunset"),
     "Follow the sun shows Light while the sun is up over Penticton and Dark after sunset. Choosing a theme by hand turns it off.",
     rel = c("theme")),
  nk("font", c("change the font", "text is hard to read", "bigger text", "make the text larger"),
     "In <b>Settings</b> you can pick an interface font; all are standard system fonts. For bigger text, use your browser zoom: Ctrl and + (Cmd and + on a Mac).",
     rel = c("theme")),
  nk("screensaver", c("what is the screensaver", "turn off the clock", "why did a clock appear", "slideshow"),
     "When the page sits idle, a screensaver shows a UTC clock and a photo slideshow. Move the mouse or press a key to return. Change it or turn it off in <b>Settings</b>, under Screensaver.",
     rel = c("theme", "utc")),
  nk("tour", c("how do i replay the tour", "tutorial", "walkthrough", "show me around"),
     "Press <b>Tour</b> at the top right any time to replay the guided walk-through.",
     rel = c("how_use")),
  nk("tips", c("how do i hide tips", "turn off the tips", "tip popups"),
     "Each tab shows a short tip the first time you open it. Press <b>Got it</b> to close one, or <b>Hide all tips</b> to stop them.",
     rel = c("tour")),
  nk("glossary_ul", c("what do dotted underlines mean", "underlined words", "dotted line under words"),
     "Words with a dotted underline are technical terms. Hover over one (or tap it on a phone) for a short plain-language definition.",
     rel = c("pvalue", "lambda")),
  nk("disconnected", c("the page went grey", "disconnected", "the site stopped working", "app froze", "page not responding"),
     "The site goes to sleep after about 15 minutes without activity. Reload the page and it starts again. Your settings are kept, but you will need to press <b>Go</b> again.",
     rel = c("go")),
  nk("utc", c("what time zone", "why utc", "times are wrong", "what is utc"),
     "All times on the dashboard are in UTC (Coordinated Universal Time), a world standard that never changes with the seasons. Local time in the Okanagan is several hours behind UTC.",
     rel = c("screensaver")),
  nk("beta", c("what does beta mean", "is this final", "can i trust these numbers"),
     "Beta means the site is still being built. Features are being added and numbers can change as new data arrive, so check with the site owner before using results in formal reports.",
     rel = c("live_data", "who_made")),
  nk("who_made", c("who made this site", "who built this", "contact", "who do i ask"),
     "This site was built and is maintained by Macus Ong.",
     rel = c("beta")),

  # ---- fish tracking ----
  nk("pit", c("what is a pit tag", "how do pit tags work", "what is a tag", "how are fish tagged"),
     "A PIT tag (passive integrated transponder) is a tiny glass tag about the size of a grain of rice, placed inside the fish. It has no battery: when the fish swims past an antenna, the antenna powers the tag for a moment and reads its unique code. That is how we know which fish passed where, and when.",
     rel = c("antenna", "why_undetected")),
  nk("antenna", c("how do antennas work", "what is an array", "where are the antennas", "how are fish detected"),
     "Antennas sit in fish ladders, dam bypass systems and river sites. Each one records the tag code, place and time when a tagged fish passes close enough. The dashboard calls each group of antennas a checkpoint.",
     rel = c("checkpoint", "det_prob")),
  nk("checkpoint", c("what is a checkpoint", "list of checkpoints", "where are the checkpoints", "which dams"),
     "Checkpoints are the detection sites downstream, in order: Penticton, the Okanagan Channel, Zosel Dam, the lower Okanogan River, then Wells, Rocky Reach, Rock Island, Priest Rapids, McNary, John Day, The Dalles and Bonneville dams on the Columbia, and finally the Columbia estuary.",
     goto = "Migration", rel = c("antenna", "route")),
  nk("farthest", c("what does farthest checkpoint mean", "farthest checkpoint"),
     "It is the checkpoint furthest downstream where the fish was detected. The fish may have gone further without being read.",
     rel = c("why_undetected")),
  nk("why_undetected", c("why are some fish never detected", "why was my fish not detected", "missing fish", "fish not picked up", "why are some fish missing", "where did the fish go"),
     "Antennas only read tags that pass close by, so some fish slip past unseen. Others do not survive the trip, and some, like hatchery fry, have not left the lake yet. A fish that is not detected is not necessarily dead.",
     rel = c("detect_vs_surv", "still_migrating")),
  nk("detect_vs_surv", c("what is the difference between detection and survival", "why is survival different from detection", "detection vs survival"),
     "Detection rate is the share of fish read at a checkpoint. Survival is the share still alive there. They differ because live fish can pass without being read. The survival model uses fish that were missed at one site but seen further down to work out how often fish are missed, and corrects for it.",
     goto = "Survival", rel = c("cjs", "det_prob")),
  nk("detection_rate", c("what is detection rate", "what does detection rate mean", "percent detected"),
     "The share of fish detected at a checkpoint, counting only fish released upstream of it. It mixes survival with the chance of being read, so the true share that got there is usually higher.",
     goto = "Charts", rel = c("detect_vs_surv", "live_reached")),
  nk("funnel", c("what is the survival funnel", "season funnel", "what does the funnel show"),
     "The funnel shows the share of fish detected at each checkpoint, from the top of the river to the ocean. It shrinks as fish die or slip past antennas unseen, so it is a detection funnel, not a pure survival estimate.",
     goto = "Overview", rel = c("detect_vs_surv")),
  nk("survival", c("what is survival", "how is survival estimated", "what does survival mean here"),
     "Survival here is the estimated share of fish still alive at a checkpoint. It comes from the Cormack-Jolly-Seber model, which uses the pattern of detections and misses at downstream sites to separate survival from the chance of being read.",
     goto = "Survival", rel = c("cjs", "live_survival")),
  nk("cjs", c("what is cormack jolly seber", "what is cjs", "what model do you use", "how does the survival model work"),
     "The Cormack-Jolly-Seber (CJS) model is the standard way to estimate survival from tagged animals. If a fish was missed at Rocky Reach but seen at Bonneville, it must have been alive at Rocky Reach. Counting cases like that tells the model how often antennas miss fish, and from there it estimates survival between checkpoints.",
     goto = "Survival", rel = c("lambda", "det_prob")),
  nk("lambda", c("what is lambda", "what does lambda mean", "lambda in the table"),
     "Lambda is survival through the last stretch multiplied by the chance of being detected at the last checkpoint. With no sites further down to check against, the model cannot separate the two, so lambda is not survival on its own.",
     goto = "Survival", rel = c("cjs", "det_prob")),
  nk("det_prob", c("what is detection probability", "chance of detection", "how often are fish missed"),
     "The chance that a live fish passing a checkpoint is actually read by its antennas. It changes with river flow, spill at the dams and how the antennas are set up.",
     rel = c("detect_vs_surv", "antenna")),
  nk("unstable", c("what does unstable mean", "why is an estimate unstable", "estimate is 100 percent", "survival is 1"),
     "An estimate near 0% or 100%, or with a very large standard error, is marked unstable. It usually means too few fish were detected for the model to work out a real value.",
     rel = c("se", "survival")),
  nk("se", c("what is standard error", "what are the error bars", "what does the bar mean"),
     "Standard error shows how uncertain an estimate is. The true value is usually within about two standard errors either side. On the charts it is drawn as the bars around each point.",
     rel = c("unstable", "pvalue")),
  nk("harmonic", c("what is harmonic mean", "arithmetic vs harmonic mean", "what is arithmetic mean", "which average is used"),
     "Both are averages of travel time. The arithmetic mean is the usual average. The harmonic mean gives less weight to the slowest fish, so a few stragglers don't drag it up; it is the standard way PIT-tag studies report travel time.",
     rel = c("travel_time", "live_days")),
  nk("travel_time", c("what is travel time", "why does travel time vary", "why are some fish slow"),
     "Travel time is the number of days from release to a fish's first detection at a checkpoint. It varies with fish size, river flow, water temperature, spill at the dams and how long a fish lingers in lakes or reservoirs.",
     goto = "Charts", rel = c("harmonic", "live_days")),
  nk("pvalue", c("what is a p value", "what does significant mean", "explain p values", "statistically significant"),
     "A p-value is how often chance alone would produce a gap at least as big as the one you see, if the groups really did not differ. Under 0.05 usually counts as a real difference. It does not tell you how big the difference is, or why it happened.",
     rel = c("live_pvalue", "holm")),
  nk("holm", c("what is holm adjusted", "why are p values adjusted", "pairwise table"),
     "When you compare many pairs at once, some will look different by luck alone. Holm adjustment raises each p-value to correct for that, so only differences that hold up are flagged.",
     rel = c("pvalue", "tests")),
  nk("tests", c("which statistical tests do you use", "what is fisher exact test", "what is wilcoxon", "what is kruskal wallis", "what is a z test"),
     "Detection shares are compared with Fisher's exact test (two groups) or a chi-squared test (three or more). Travel days and pace use the Wilcoxon rank-sum test (two groups) or Kruskal-Wallis (three or more). Survival estimates are compared with a z-test.",
     rel = c("pvalue", "holm")),
  nk("recapture", c("what is a recapture", "recaptured fish", "what are recaptures"),
     "A recapture is a tagged fish caught again, for example in a trap or during sampling, and then let go. Recaptures are shown for reference only.",
     goto = "Recaptures", rel = c("recap_count")),
  nk("recap_count", c("why dont recaptures count", "are recaptures used in survival"),
     "Recaptures come from separate sampling, not the fixed antennas, so they are kept out of detection rates, travel times and survival to avoid mixing two different kinds of data.",
     rel = c("recapture")),
  nk("still_migrating", c("what does may still be migrating mean", "gold warning", "still migrating", "include release groups that may still be migrating"),
     "Some groups, especially hatchery fry, spend about a year in a lake before heading to the ocean. If the data end before a group's usual travel time, its low numbers may just mean the fish haven't left yet. Those groups are flagged and left out of the p-values unless you tick the box.",
     goto = "Survival", rel = c("hatchery_year", "live_zero")),
  nk("fry_smolt", c("what is a smolt", "what is the difference between fry and smolt", "what is a fry", "what is a juvenile"),
     "Fry are young fish not long after they emerge from the gravel. Smolts are juveniles that have turned silvery and changed inside to get ready for salt water, and they head down to the ocean. Most Okanagan sockeye spend about a year in a lake before becoming smolts.",
     rel = c("lifecycle", "hatchery_year")),
  nk("hatchery_year", c("why do hatchery fish take a year", "why are hatchery fish detected so late", "when do hatchery fish migrate"),
     "Hatchery fry are released into streams that flow into Okanagan Lake. Like wild sockeye, they rear in the lake for about a year and migrate as smolts the next spring, so they reach the dams roughly 340 to 370 days after release.",
     rel = c("still_migrating", "fry_smolt")),
  nk("sites", c("what are the release sites", "where are fish released", "list of populations", "which populations"),
     "There are six release groups: Skaha Lake, Osoyoos Lake, Okanagan Lake (fish from the rotary screw trap at Penticton), and three ONA Fish Hatchery release sites at Equesis Creek, Mission Creek and Shingle Creek. They are always kept separate, never pooled.",
     rel = c("screwtrap", "not_pooled")),
  nk("screwtrap", c("what is a screw trap", "okanagan lake screw trap", "rotary trap"),
     "A rotary screw trap is a floating, cone-shaped trap that turns in the current and gently catches young fish moving downstream. The Okanagan Lake group are fish caught this way in the Okanagan River channel at Penticton, then tagged and released.",
     rel = c("sites")),
  nk("not_pooled", c("why are hatchery and lake fish separate", "why not combine populations", "why not pool"),
     "Hatchery fry and lake-reared fish have different histories, release places and timing, so pooling them would blur real differences. The dashboard always keeps each release site separate.",
     rel = c("sites", "compare")),
  nk("distance", c("why do osoyoos fish survive better", "is skaha survival lower", "why do lakes differ"),
     "Part of the difference is distance. Skaha Lake fish travel further and pass more river, lakes and structures before reaching the Columbia than Osoyoos Lake fish, which start near Zosel Dam. For a fair test, compare survival over the same stretch of river.",
     rel = c("live_survival", "compare")),
  nk("lifecycle", c("what is the sockeye life cycle", "how long do sockeye live", "sockeye life history"),
     "Sockeye hatch in gravel over winter, emerge as fry in spring and usually spend about a year in a lake. They migrate to the Pacific as smolts, grow at sea for usually two to three years, then return to their home river to spawn, turning bright red with a green head. Sockeye die after spawning.",
     rel = c("fry_smolt", "route")),
  nk("route", c("how far do they travel", "how many dams", "how far is the ocean", "how long is the trip"),
     "From Penticton to the mouth of the Columbia River is roughly 1,050 river kilometres. On the way, Okanagan sockeye pass nine dams on the Columbia River: Wells, Rocky Reach, Rock Island, Wanapum, Priest Rapids, McNary, John Day, The Dalles and Bonneville.",
     rel = c("checkpoint", "live_days")),
  nk("kokanee", c("what is the difference between kokanee and sockeye", "what is a kokanee"),
     "Kokanee are the same species as sockeye but spend their whole lives in fresh water. Sockeye go to the ocean and come back much bigger.",
     rel = c("lifecycle")),
  nk("why_red", c("why are sockeye red", "why do sockeye turn red"),
     "Sockeye turn bright red with a green head when they return to spawn. The red comes from pigments called carotenoids, stored from their ocean diet.",
     rel = c("lifecycle")),
  nk("temperature", c("how does temperature affect sockeye", "thermal barrier", "warm water", "water temperature"),
     "Warm water stresses sockeye. When the Okanogan River gets warmer than about 21 degrees C in summer, returning adults often hold in cooler water and delay their migration until it cools.",
     rel = c("route", "lifecycle")),
  nk("data_source", c("where does the data come from", "what is ptagis", "data source"),
     "The data come from PTAGIS, the Columbia Basin PIT Tag Information System, which collects tagging and detection records from across the basin. The dashboard reads exports of the tagging and detection (interrogation) records.",
     rel = c("live_data", "upload_lag")),
  nk("upload_lag", c("why are recent detections missing", "data lag", "is the data up to date"),
     "Detection records reach PTAGIS after a delay, sometimes weeks, so the most recent months can look incomplete. Check again after the next data export.",
     rel = c("live_data", "still_migrating"))
)
names(NERKI_KB) <- vapply(NERKI_KB, function(e) e$id, "")

NERKI_CHIPS <- list(
  Overview = c("What is a PIT tag?", "How many fish reached Bonneville?", "What is the survival funnel?"),
  Explore = c("How do I export the data?", "What do the columns mean?", "How do I search for a tag?"),
  Charts = c("How do I export a plot?", "What is detection rate?", "Why does travel time vary?"),
  Journeys = c("How do I follow one fish?", "What is a checkpoint?", "What does farthest checkpoint mean?"),
  Migration = c("How does the migration animation work?", "What is the spotlight fish?", "Where are the checkpoints?"),
  Recaptures = c("What is a recapture?", "Why don't recaptures count?"),
  Speed = c("Which fish is fastest?", "What does km per day mean?", "Why do hatchery fish look slower?"),
  Survival = c("What is lambda?", "Why is survival different from detection?", "What does this p-value mean?"),
  Photos = c("How do I add photos?", "Where do photos come from?"),
  default = c("What can you help with?", "How do I use this site?", "What is a PIT tag?")
)

# --- text normalising and matching ---
# places and populations mentioned in a question
NK_CP <- c(penticton = "PEN", okchannel = "OKC", zosel = "ZSL", malott = "OKL", wells = "WEL", rockyreach = "RRJ",
           rockisland = "RIS", priestrapids = "PRD", mcnary = "MCJ", johnday = "JDJ", dalles = "TDA", bonneville = "BON",
           ocean = "EST")
NK_POP <- c(osoyoos = "OSOYOL", skaha = "SKATAL", okanaganlake = "OKANR", screwtrap = "OKANR", equesis = "EQUESC",
            mission = "MISS3C", shingle = "SHINGC")
NK_STOP <- c("a", "an", "the", "is", "are", "was", "were", "be", "been", "do", "does", "did", "i", "me", "my", "we", "you",
             "your", "it", "its", "this", "that", "these", "those", "of", "to", "in", "on", "for", "and", "or", "what", "whats",
             "how", "when", "where", "which", "who", "can", "could", "should", "would", "will", "there", "their", "with",
             "about", "from", "at", "by", "as", "so", "if", "any", "some", "get", "have", "has", "had", "tell", "explain",
             "please", "show", "nerki", "just", "really", "here", "out", "up", "into", "than", "too")
NK_PHRASE <- c("p value" = "pvalue", "p values" = "pvalue", "rocky reach" = "rockyreach", "the dalles" = "dalles",
               "john day" = "johnday", "priest rapids" = "priestrapids", "rock island" = "rockisland", "screw trap" = "screwtrap",
               "okanagan lake" = "okanaganlake", "okanagan channel" = "okchannel", "lower okanogan" = "malott",
               "harmonic mean" = "harmonicmean", "arithmetic mean" = "arithmeticmean", "standard error" = "standarderror",
               "cormack jolly seber" = "cjs", "follow the sun" = "followsun", "km per day" = "kmday", "per day" = "kmday",
               "error bars" = "standarderror", "dark mode" = "darkmode", "night mode" = "darkmode", "made it" = "reach",
               "got to" = "reach", "pit tag" = "pittag", "z test" = "ztest", "chi squared" = "chisquared",
               "picked up" = "detect", "the ocean" = "ocean", "fish ladder" = "antenna")
NK_SYN <- c(download = "export", save = "export", saving = "export", chart = "plot", graph = "plot", figure = "plot",
            csv = "table", spreadsheet = "table", excel = "table", arrive = "reach", arrived = "reach", reached = "reach",
            reaching = "reach", survive = "survival", survived = "survival", surviving = "survival", alive = "survival",
            die = "survival", died = "survival", dead = "survival", mortality = "survival", death = "survival",
            detected = "detect", detection = "detect", detections = "detect", detecting = "detect", seen = "detect",
            spotted = "detect", missed = "detect", missing = "detect", fastest = "fast", quickest = "fast", quick = "fast",
            speed = "fast", pace = "fast", faster = "fast", slowest = "slow", slower = "slow", salmon = "sockeye",
            tagged = "tag", tags = "tag", tagging = "tag", pit = "pittag", chip = "pittag", antennas = "antenna",
            array = "antenna", arrays = "antenna", reader = "antenna", readers = "antenna", dams = "dam",
            populations = "population", sites = "population", site = "population", stock = "population", group = "population",
            groups = "population", lakes = "lake", years = "year", dates = "date", significant = "pvalue",
            significance = "pvalue", tabs = "tab", colours = "colour", color = "colour", colors = "colour", themes = "theme",
            fonts = "font", typeface = "font", photos = "photo", pictures = "photo", picture = "photo", images = "photo",
            image = "photo", pics = "photo", ocean = "ocean", sea = "ocean", pacific = "ocean", estuary = "ocean",
            fry = "fry", smolts = "smolt", juveniles = "juvenile", released = "release", releases = "release",
            releasing = "release", stocked = "release", greyed = "grey", gray = "grey", froze = "frozen", stuck = "frozen",
            numbers = "number", count = "number", total = "number", much = "many", days = "day", takes = "take",
            took = "take", taking = "take", travelling = "travel", traveling = "travel", travels = "travel")
nk_tokens <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  x <- gsub("[-_/]", " ", x); x <- gsub("[^a-z0-9 ]", " ", x); x <- gsub("\\s+", " ", x)
  for (p in names(NK_PHRASE)) x <- gsub(paste0("\\b", p, "\\b"), NK_PHRASE[[p]], x)
  w <- strsplit(trimws(x), " ", fixed = TRUE)[[1]]
  w <- ifelse(w %in% names(NK_SYN), NK_SYN[w], w)
  w <- w[nchar(w) > 1 & !(w %in% NK_STOP)]
  keep <- w %in% c(names(NK_CP), names(NK_POP), "dalles", "always", "series", "species", "analysis", "lambdas")
  w <- ifelse(!keep & nchar(w) > 4 & grepl("s$", w) & !grepl("ss$", w), sub("s$", "", w), w)
  unique(unname(w))
}
NK_SAMPLES <- do.call(rbind, lapply(NERKI_KB, function(e) data.frame(id = e$id, q = e$q, stringsAsFactors = FALSE)))
NK_SAMPLES$tok <- lapply(NK_SAMPLES$q, nk_tokens)
NK_IDF <- local({ v <- unlist(NK_SAMPLES$tok); df <- table(v); log((nrow(NK_SAMPLES) + 1) / (as.numeric(df) + 0.5)) + 1 -> w; setNames(w, names(df)) })
nk_vec <- function(tok) { tok <- tok[tok %in% names(NK_IDF)]; if (!length(tok)) return(numeric()); setNames(NK_IDF[tok], tok) }
NK_SAMPLES$vec <- lapply(NK_SAMPLES$tok, nk_vec)
nk_match <- function(text) {
  tok <- nk_tokens(text); v <- nk_vec(tok)
  if (!length(v)) return(list(best = NA, score = 0, top = character(), tokens = tok))
  unknown <- setdiff(tok, c(names(NK_IDF), names(NK_CP), names(NK_POP), "mean", "means", "meaning"))
  extra <- length(unknown) * median(NK_IDF)^2
  sc <- vapply(NK_SAMPLES$vec, function(s) {
    if (!length(s)) return(0)
    common <- intersect(names(v), names(s)); if (!length(common)) return(0)
    sum(v[common] * s[common]) / (sqrt(sum(v^2) + extra) * sqrt(sum(s^2)))
  }, 0)
  best <- tapply(sc, NK_SAMPLES$id, max); best <- sort(best, decreasing = TRUE)
  list(best = names(best)[1], score = unname(best[1]), top = names(best)[best > 0.12][1:min(3, sum(best > 0.12))], tokens = tok)
}

nerki_svg <- function(s = "a") HTML(sprintf('<svg viewBox="0 0 140 120" aria-hidden="true">
<defs><linearGradient id="nkb%1$s" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#2c6878"/><stop offset=".42" stop-color="#8fcbd6"/><stop offset=".68" stop-color="#e6f5f7"/><stop offset="1" stop-color="#ffffff"/></linearGradient>
<radialGradient id="nkt%1$s" cx=".5" cy=".5" r=".5"><stop offset="0" stop-color="#ffe79a"/><stop offset="1" stop-color="#ffd166" stop-opacity="0"/></radialGradient></defs>
<g><animateTransform attributeName="transform" type="translate" values="0 0; 0 -3; 0 0" dur="3s" repeatCount="indefinite"/>
<g><animateTransform attributeName="transform" type="rotate" values="-9 26 62; 9 26 62; -9 26 62" dur="1.1s" repeatCount="indefinite"/>
<path d="M27 62 L7 45 C12 55 12 69 7 79 Z" fill="#2c6878"/><path d="M24 62 L12 52 C14 58 14 66 12 72 Z" fill="#5c97a3"/></g>
<path d="M60 34 L67 21 L78 35 Z" fill="#2c6878"/><path d="M38 41 L42 35 L46 42 Z" fill="#2c6878"/>
<path d="M23 62 C31 37 70 28 98 39 C113 45 122 54 124 62 C122 72 112 81 97 85 C71 94 33 89 23 62 Z" fill="url(#nkb%1$s)" stroke="#24525d" stroke-width="1.6"/>
<g fill="#4f8592" opacity=".38"><ellipse cx="45" cy="57" rx="4" ry="7"/><ellipse cx="58" cy="55" rx="4" ry="7.5"/><ellipse cx="71" cy="55" rx="4" ry="7.5"/><ellipse cx="84" cy="56" rx="3.5" ry="6.5"/></g>
<circle cx="66" cy="72" r="9" fill="url(#nkt%1$s)"><animate attributeName="r" values="6;10;6" dur="2.2s" repeatCount="indefinite"/><animate attributeName="opacity" values=".9;.35;.9" dur="2.2s" repeatCount="indefinite"/></circle>
<rect x="61" y="70" width="10" height="4" rx="2" fill="#ffd166" stroke="#b8862b" stroke-width=".8"/>
<path d="M96 44 C100 52 100 64 96 74" fill="none" stroke="#24525d" stroke-width="1.3" stroke-linecap="round"/>
<g><animateTransform attributeName="transform" type="rotate" values="0 86 70; 22 86 70; 0 86 70" dur="1.6s" repeatCount="indefinite"/>
<path d="M86 70 C94 73 97 81 91 86 C88 80 85 76 84 72 Z" fill="#5c97a3" stroke="#24525d" stroke-width="1"/></g>
<ellipse cx="104" cy="67" rx="4" ry="2.6" fill="#f3a3a0" opacity=".75"/>
<ellipse cx="106" cy="53" rx="9" ry="9" fill="#fff" stroke="#24525d" stroke-width="1.4"><animate attributeName="ry" values="9;9;1;9" keyTimes="0;.9;.94;1" dur="4.5s" repeatCount="indefinite"/></ellipse>
<ellipse cx="108" cy="54" rx="5" ry="5.2" fill="#18292e"><animate attributeName="ry" values="5.2;5.2;.6;5.2" keyTimes="0;.9;.94;1" dur="4.5s" repeatCount="indefinite"/></ellipse>
<circle cx="110" cy="51.5" r="1.8" fill="#fff"><animate attributeName="opacity" values="1;1;0;1" keyTimes="0;.9;.94;1" dur="4.5s" repeatCount="indefinite"/></circle>
<path d="M113 67 Q118 71 122 65.5" fill="none" stroke="#18292e" stroke-width="2" stroke-linecap="round"/></g>
<g fill="none" stroke="#7cc3d1" stroke-width="1.4"><circle cx="128" cy="52" r="3"><animate attributeName="cy" values="56;18" dur="2.6s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" dur="2.6s" repeatCount="indefinite"/></circle>
<circle cx="133" cy="50" r="2"><animate attributeName="cy" values="54;12" dur="2.6s" begin="1.1s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" dur="2.6s" begin="1.1s" repeatCount="indefinite"/></circle></g>
</svg>', s))

nerki_ui <- div(id = "nerki",
  div(id = "nk_hello", class = "nk-hello",
      tags$button(type = "button", class = "nk-hello-x", `aria-label` = "Close", HTML("&times;")),
      HTML("Hi, I am <b>Nerki</b>, your AI helper. Ask me anything about the sockeye!")),
  tags$button(type = "button", id = "nk_launch", class = "nk-launch", `aria-label` = "Open Nerki, your helper",
              span(class = "nk-ring"), nerki_svg("l")),
  div(id = "nk_panel", class = "nk-panel", role = "dialog", `aria-label` = "Nerki helper",
      div(class = "nk-head", div(class = "nk-av", nerki_svg("h")),
          div(class = "nk-ht", tags$b("Nerki"), span("Your helper (beta). Answers come from a built-in guide.")),
          tags$button(type = "button", id = "nk_close", class = "nk-close", `aria-label` = "Close", HTML("&times;"))),
      div(id = "nk_msgs", class = "nk-msgs", `aria-live` = "polite"),
      div(id = "nk_chips", class = "nk-chips"),
      tags$form(id = "nk_form", class = "nk-inp", autocomplete = "off",
                tags$input(id = "nk_text", type = "text", placeholder = "Ask Nerki a question...", maxlength = "300", `aria-label` = "Your question"),
                tags$button(type = "submit", class = "nk-send", `aria-label` = "Send", icon("paper-plane")))))

NERKI_JS <- "
(function(){
  var busy = false, greeted = false, n = 0;
  function q(s){ return document.querySelector(s); }
  function esc(t){ var d = document.createElement('div'); d.textContent = t; return d.innerHTML; }
  function tab(){ var a = q('#tabs li.active a'); return a ? a.getAttribute('data-value') : ''; }
  function add(html, cls){
    var m = document.createElement('div'); m.className = 'nk-m' + (cls ? ' ' + cls : ''); m.innerHTML = html;
    var box = q('#nk_msgs'); box.appendChild(m); box.scrollTop = box.scrollHeight; return m;
  }
  function chips(list){
    var c = q('#nk_chips'); c.innerHTML = '';
    (list || []).forEach(function(s){ var b = document.createElement('button'); b.type = 'button'; b.className = 'nk-chip'; b.textContent = s;
      b.onclick = function(){ ask(s); }; c.appendChild(b); });
  }
  function send(text){ n++; if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('nerki_q', {text: text, tab: tab(), n: n}, {priority: 'event'}); }
  function ask(text){
    text = (text || '').trim(); if (!text || busy) return;
    add(esc(text), 'me'); if (window.appSnd) appSnd.play('tick'); q('#nk_text').value = ''; busy = true;
    add('<span class=nk-dots><i></i><i></i><i></i></span>', 'typing');
    send(text);
    setTimeout(function(){ if (busy) done({html: 'Sorry, I lost the thread there. Please try again in a moment.'}); }, 12000);
  }
  function done(d){
    var t = q('#nk_msgs .typing'); if (t) t.parentNode.removeChild(t);
    if (d.html) { add(d.html); if (window.appSnd) appSnd.play('blub'); } if (d.chips) chips(d.chips); busy = false;
  }
  function open(){
    var p = q('#nk_panel'); p.classList.add('open'); q('#nk_launch').classList.add('hide'); hello(false); if (window.appSnd) appSnd.play('pop');
    if (!greeted) { greeted = true; busy = true; add('<span class=nk-dots><i></i><i></i><i></i></span>', 'typing'); send('__hello__'); }
    setTimeout(function(){ var i = q('#nk_text'); if (i) i.focus(); }, 200);
  }
  function close(){ q('#nk_panel').classList.remove('open'); q('#nk_launch').classList.remove('hide'); }
  function hello(on){ var h = q('#nk_hello'); if (h) h.classList.toggle('show', !!on); }
  if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('nerki_a', function(d){ setTimeout(function(){ done(d); }, 450); });
  document.addEventListener('DOMContentLoaded', function(){
    q('#nk_launch').addEventListener('click', open);
    q('#nk_close').addEventListener('click', close);
    q('#nk_hello').addEventListener('click', function(e){ if (e.target.closest('.nk-hello-x')) hello(false); else open(); });
    q('#nk_form').addEventListener('submit', function(e){ e.preventDefault(); ask(q('#nk_text').value); });
    q('#nk_msgs').addEventListener('click', function(e){
      var g = e.target.closest('.nk-go'); if (!g) return;
      var a = q('#tabs a[data-value=\"' + g.getAttribute('data-tab') + '\"]'); if (a) $(a).tab('show');
    });
    $(document).on('shown.bs.tab', function(){ if (greeted && !busy && q('#nk_panel').classList.contains('open')) send('__chips__'); });
    setTimeout(function(){
      var seen = null; try { seen = sessionStorage.getItem('nk_hello'); } catch(e) {}
      if (window.__tourActive || seen) return;
      hello(true); try { sessionStorage.setItem('nk_hello', '1'); } catch(e) {}
      setTimeout(function(){ hello(false); }, 12000);
    }, 3000);
  });
})();
"

screensaver_ui <- div(id = "ss",
  div(id = "ss_img0", class = "ss-img"), div(id = "ss_img1", class = "ss-img"), div(class = "ss-shade"),
  div(class = "ss-hint", "Move the mouse or press any key to return"),
  div(class = "ss-clock", div(id = "ss_time", class = "ss-time"), div(id = "ss_date", class = "ss-date")),
  div(id = "ss_cap", class = "ss-cap"))

settings_ui <- div(id = "side_settings", style = "display:none;",
  h4("Theme"),
  div(class = "theme-grid", lapply(THEMES, function(t)
    tags$button(type = "button", class = "theme-card", `data-theme` = t$id,
                div(class = "sw", lapply(t$sw, function(c) span(style = paste0("background:", c, ";")))),
                div(class = "tn", t$name)))),
  div(class = "sun-row",
      div(div(class = "set-t", "Follow the sun"),
          div(class = "set-d", "Light while the sun is up over Penticton, dark once it sets. Choosing a theme by hand switches this off.")),
      div(class = "seg", tags$button(type = "button", class = "seg-b", `data-sun` = "off", "Off"),
          tags$button(type = "button", class = "seg-b", `data-sun` = "auto", "Auto"))),
  div(id = "sun_status", class = "set-d"),
  h4("Interface font"),
  div(class = "set-d", "System typefaces only - nothing extra is downloaded. Charts keep their own font."),
  tags$select(id = "font_pick", lapply(FONTS, function(f) tags$option(value = f$id, f$name))),
  div(class = "font-prev",
      div(tags$b("SKATAL-2025-0412"), " - Skaha Lake release - 2,143 fish - crew of 3"),
      div("The quick brown fox jumps over the lazy dog 0123456789")),
  h4("Screensaver", style = "margin-top:14px;"),
  div(class = "set-d", "When the page sits idle, show a UTC clock and/or a slideshow from the photo library. Any mouse move, click or key brings the dashboard back."),
  div(class = "ss-row",
      tags$select(id = "ss_mode", tags$option(value = "both", "Clock + photos"), tags$option(value = "clock", "Clock only"),
                  tags$option(value = "photos", "Photos only"), tags$option(value = "off", "Off")),
      tags$select(id = "ss_min", tags$option(value = "1", "after 1 min"), tags$option(value = "2", "after 2 min"),
                  tags$option(value = "5", "after 5 min"), tags$option(value = "10", "after 10 min"),
                  tags$option(value = "15", "after 15 min"))),
  tags$button(type = "button", id = "ss_preview", class = "btn btn-default btn-sm", "Preview screensaver"),
  h4("Sound", style = "margin-top:14px;"),
  div(class = "sun-row",
      div(div(class = "set-t", "Pops and chimes"),
          div(class = "set-d", "Soft sounds when Nerki speaks, when Go runs, when an export starts, when a warning appears and when tabs change. Browsers keep a page silent until your first click or tap.")),
      div(class = "seg", tags$button(type = "button", class = "snd-b", `data-snd` = "on", "On"),
          tags$button(type = "button", class = "snd-b", `data-snd` = "off", "Off"))),
  div(class = "small-note", "Settings are saved in this browser only. All times on the dashboard are UTC."))

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$title("Let's Follow the Sockeye (Beta)"), tags$style(HTML(APP_CSS)), tags$script(HTML(SETTINGS_JS)), tags$script(HTML(SOUND_JS)), tags$script(HTML(SS_JS)), tags$script(HTML(TOUR_JS)), tags$script(HTML(GLOSS_JS)), tags$script(HTML(NERKI_JS)),
            tags$script(HTML("$(document).on('shown.bs.tab', function(){ setTimeout(function(){ if ($.fn.dataTable) $.fn.dataTable.tables({visible:true, api:true}).columns.adjust(); }, 60); });"))),

  screensaver_ui,
  nerki_ui,
  div(class = "topbar",
      div(class = "brand", div(class = "logo", logo_svg("t")),
          div(h1("Let's Follow the Sockeye", span(class = "beta-badge", "BETA")),
              p("Okanagan fry and smolt outmigration tracker"))),
      div(class = "top-right",
          tags$button(type = "button", id = "tour_btn", class = "tour-btn", icon("circle-question"), " Tour"),
          div(class = "ver", HTML(sprintf("v%s beta<br>built %s UTC", APP_VERSION, APP_VERSION_DATE))))),

  div(class = "layout",
      # ---------------- left pane ----------------
      tags$aside(class = "side",
                 div(class = "side-tabs",
                     tags$button(type = "button", class = "st-b on", `data-pane` = "filters", icon("filter"), " Filters"),
                     tags$button(type = "button", class = "st-b", `data-pane` = "settings", icon("gear"), " Settings")),
                 div(id = "side_filters",
                 h4("Population"),
                 pickerInput(
                   inputId = "pop",
                   label = NULL,
                   choices = POP_CHOICES,
                   selected = "all",
                   multiple = TRUE,
                   width = "100%",
                   options = pickerOptions(
                     actionsBox = TRUE,
                     size = 5,
                     liveSearch = TRUE,
                     selectedTextFormat = "count"
                   )
                 ),
                 actionButton("go", "Go", icon = icon("play"), class = "btn-go"),
                 uiOutput("pending"),
                 hr(style = "border-color:var(--line);margin:14px 0;"),
                 h4("Release year"),
                 uiOutput("year_ui"),
                 h4("Release dates", style = "margin-top:6px;"),
                 uiOutput("date_ui"),
                 div(class = "small-note",
                     "Pick a population, years and dates, then press ", tags$b("Go"),
                     " to apply. Go also checks the data folder for newer exports."),
                 uiOutput("side_summary"),
                 downloadButton("dl_report", "Summary report", icon = icon("file-lines"), class = "btn-soft btn-report"),
                 div(class = "small-note", style = "margin-top:6px;", "One file with the numbers, charts and p-values for this selection. Open it in a browser; Print > Save as PDF makes a PDF.")),
                 settings_ui),

      # ---------------- pages ----------------
      tags$main(
        tabsetPanel(id = "tabs", type = "pills",

                    tabPanel("Overview",
                             uiOutput("hero"),
                             uiOutput("tiles"),
                             card("Tagged sockeye by year", uiOutput("year_bars")),
                             div(class = "grid2",
                                 card("Where fish were released", uiOutput("site_bars")),
                                 card("Season survival funnel",
                                      div(class = "sub", "Share of fish detected at each checkpoint, counting only fish released upstream of it."),
                                      uiOutput("funnel_overview")))),

                    tabPanel("Explore",
                             card("Explore the records",
                                  div(class = "sub", "Every tagged fish in the current selection. Search by tag, release location or year."),
                                  DTOutput("tbl_fish"),
                                  downloadButton("dl_fish", "Export table (CSV)", class = "btn-default"))),

                    tabPanel("Charts",
                             card("Fish released", plotOutput("p_year", height = "340px"),
                                  downloadButton("dl_p_year", "Export plot (PNG)", class = "btn-default")),
                             card("Detection rate",
                                  plotOutput("p_rate", height = "400px"),
                                  downloadButton("dl_p_rate", "Export plot (PNG)", class = "btn-default"),
                                  tableOutput("t_rate"),
                                  downloadButton("dl_t_rate", "Export table (CSV)", class = "btn-default")),
                             card("Fish detected", plotOutput("p_count", height = "380px"),
                                  downloadButton("dl_p_count", "Export plot (PNG)", class = "btn-default")),
                             card("Travel time", plotOutput("p_travel", height = "520px"),
                                  downloadButton("dl_p_travel", "Export plot (PNG)", class = "btn-default"),
                                  tableOutput("t_travel"),
                                  downloadButton("dl_t_travel", "Export table (CSV)", class = "btn-default")),
                             card("Detections through the season", plotOutput("p_weekly", height = "360px"),
                                  downloadButton("dl_p_weekly", "Export plot (PNG)", class = "btn-default"))),

                    tabPanel("Journeys",
                             card("Fish journeys",
                                  uiOutput("journey_intro"),
                                  uiOutput("journey_detail"),
                                  DTOutput("tbl_journeys"),
                                  downloadButton("dl_journeys", "Export table (CSV)", class = "btn-default"))),

                    tabPanel("Migration",
                             card("Watch the migration",
                                  div(class = "sub", "Press play or drag the date to see fish reach each checkpoint over the season."),
                                  div(class = "mig-controls",
                                      uiOutput("mig_slider_ui"),
                                      selectInput("mig_speed", "Speed",
                                                  c("1 day per step" = 1, "3 days per step" = 3, "1 week per step" = 7)))),
                             card("Season survival funnel", uiOutput("funnel_mig")),
                             uiOutput("mig_tiles"),
                             div(class = "grid2", style = "grid-template-columns:2fr 1fr;",
                                 card("Migration map", leafletOutput("mig_map", height = "480px"), class = ""),
                                 div(class = "spot",
                                     div(class = "spot-sel",
                                         selectInput("spot_site", "Release site", c("Any release site" = "any"), selectize = FALSE, width = "100%"),
                                         selectInput("spot_dest", "Final destination", c("Any final destination" = "any"), selectize = FALSE, width = "100%")),
                                     uiOutput("spot"),
                                     uiOutput("spot_slider_ui"),
                                     actionButton("spot_any", "Another fish"),
                                     div(class = "spot-key",
                                         div(span(class = "sk sk-dot"), "Fish position on the river"),
                                         div(span(class = "sk sk-ring"), "Site where the fish will be detected"),
                                         div(span(class = "sk sk-fill"), "Site where it has been detected")),
                                     div(class = "spot-note", "The play button here follows this fish only, from release to its last detection. The season date above is separate.")))),

                    tabPanel("Recaptures",
                             card("Recaptured fish",
                                  div(class = "sub", "Fish that were caught again after release and let go a second time. Shown for reference only; they do not change detection rates, travel time or survival."),
                                  uiOutput("recap_tiles"),
                                  uiOutput("recap_by_site")),
                             card("Where they were caught again",
                                  div(class = "sub", "Each line runs from the release location to the place the fish was caught again."),
                                  leafletOutput("recap_map", height = "520px")),
                             card("Recapture list",
                                  DTOutput("tbl_recap"),
                                  downloadButton("dl_recap", "Export table (CSV)", class = "btn-default"))),

                    tabPanel("Speed",
                             card("Who is moving fastest?",
                                  div(class = "sub", "Distance covered over time, for every fish detected at two or more checkpoints."),
                                  uiOutput("speed_tiles"),
                                  radioButtons("speed_dir", NULL, c("Fastest" = "fast", "Slowest" = "slow"), inline = TRUE),
                                  uiOutput("speed_list"),
                                  downloadButton("dl_speed", "Export table (CSV)", class = "btn-default")),
                             card("Median pace by release location", uiOutput("speed_by_site"),
                                  uiOutput("pace_test"))),

                    tabPanel("Survival",
                             card("Survival and travel time",
                                  div(class = "sub", "Travel time (arithmetic and harmonic means), survival between checkpoints, and lambda, using the Cormack-Jolly-Seber model on each release group. The Skaha Lake, Osoyoos Lake and Okanagan Lake chain is Release, Rocky Reach Dam, Bonneville Dam. The ONA Fish Hatchery chain adds Penticton and the Okanagan Channel."),
                                  radioButtons("surv_mode", NULL, inline = TRUE,
                                               c("Each release year separately" = "year", "Selected years combined" = "all"))),
                             card("Do the selected populations differ? (p-values)",
                                  div(class = "sub", "Compares the populations picked in the left pane. Release years in the selection are pooled."),
                                  checkboxInput("incl_migrating", "Include release groups that may still be migrating", FALSE),
                                  uiOutput("pop_tests"),
                                  downloadButton("dl_pvals", "Export p-values (CSV)", class = "btn-default")),
                             card("Travel time (days)",
                                  plotOutput("p_tt_means", height = "auto"),
                                  downloadButton("dl_p_tt_means", "Export plot (PNG)", class = "btn-default"),
                                  tableOutput("t_tt_means"),
                                  downloadButton("dl_t_tt_means", "Export table (CSV)", class = "btn-default"),
                                  uiOutput("surv_omitted")),
                             card("Survival and lambda",
                                  plotOutput("p_surv", height = "auto"),
                                  downloadButton("dl_p_surv", "Export plot (PNG)", class = "btn-default"),
                                  tableOutput("t_surv"),
                                  downloadButton("dl_t_surv", "Export table (CSV)", class = "btn-default"),
                                  div(class = "small-note", style = "font-size:12px;",
                                      "Survival at the last checkpoint and detection there cannot be separated on a single release, so only their product, lambda, is reported. Estimates near 0% or 100%, or with a large standard error, are flagged as unstable: they reflect too few detections, not a real value."),
                                  uiOutput("surv_tests")),
                             card("Fish detected at each modeled checkpoint", tableOutput("t_surv_det"))),

                    tabPanel("Photos",
                             card("Photos",
                                  div(class = "photo-tools",
                                      selectInput("photo_album", "Album", c("All photos" = "all"), width = "220px"),
                                      textInput("photo_q", "Search", placeholder = "Type part of a name", width = "260px"),
                                      actionButton("photo_refresh", "Refresh photos", icon = icon("rotate"), class = "btn-default")),
                                  uiOutput("photo_count"),
                                  uiOutput("photo_grid"),
                                  uiOutput("photo_more")))
        )
      )
  )
)

# =============================================================================
# 7. SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---------------- theme ----------------
  # set in the browser by the Settings pane (theme or follow-the-sun); TRUE = dark theme
  dark <- reactive(isTRUE(input$ui_dark))

  # ---------------- data (reload on Go if files changed) ----------------
  raw <- reactiveVal(NULL)
  do_load <- function() {
    b <- tryCatch(get_bundle(DATA_DIR), error = function(e) {
      showNotification(paste("Data load failed:", e$message), type = "error", duration = 12); NULL })
    if (is.null(b)) {
      showNotification("No PitPro tagging / interrogation files found in the data folder.",
                       type = "error", duration = 12)
    } else raw(b)
  }
  observeEvent(TRUE, do_load(), once = TRUE)
  observeEvent(input$go, do_load(), ignoreInit = TRUE)

  output$year_ui <- renderUI({
    b <- raw(); req(b)
    yrs <- sort(unique(b$tag$year[!is.na(b$tag$year)]), decreasing = TRUE)
    keep <- isolate(input$years)
    selectInput("years", NULL, choices = as.character(yrs),
                selected = if (is.null(keep)) as.character(yrs) else intersect(keep, as.character(yrs)),
                multiple = TRUE, width = "100%")
  })

  # free date range (not limited to the dates in the data); starts at the span of the data
  data_span <- reactive({
    b <- raw(); req(b); rc <- recap_all()
    range(c(as.Date(b$tag$reldt_posix), if (!is.null(rc)) rc$rel_date), na.rm = TRUE) })
  last_span <- reactiveVal(NULL)
  output$date_ui <- renderUI({
    sp <- data_span(); cur <- isolate(input$dates); old <- isolate(last_span())
    st <- sp[1]; en <- sp[2]
    if (!is.null(cur) && length(cur) == 2 && !any(is.na(cur)) && !is.null(old) && !identical(as.Date(cur), old)) { st <- cur[1]; en <- cur[2] }
    last_span(sp)
    dateRangeInput("dates", NULL, start = st, end = en, min = as.Date("2000-01-01"),
                   max = Sys.Date() + 365, format = "yyyy-mm-dd", separator = " to ", width = "100%")
  })

  # ---------------- selection applied only when Go is pressed ----------------
  dates_ready <- reactiveVal(FALSE)
  observeEvent(input$dates, dates_ready(TRUE), once = TRUE)
  sel <- eventReactive(list(input$go, dates_ready()),
                       list(pop = input$pop, years = input$years, dates = input$dates), ignoreNULL = FALSE)

  output$pending <- renderUI({
    s <- sel(); b <- raw(); req(b)
    all_y <- as.character(unique(b$tag$year[!is.na(b$tag$year)]))
    norm  <- function(y) sort(if (length(y) == 0) all_y else as.character(y))
    hints <- list()
    if (!identical(input$pop, s$pop) || !identical(norm(input$years), norm(s$years)) ||
        !identical(as.character(input$dates), as.character(s$dates)))
      hints <- c(hints, list(div(class = "hint", "Selection changed - press Go to apply.")))
    # years and dates both apply: warn when no release date in the chosen years falls inside the range
    dts <- input$dates
    if (length(dts) == 2 && !any(is.na(dts))) {
      d <- as.Date(b$tag$reldt_posix)
      if (length(input$years)) d <- d[b$tag$year %in% as.integer(input$years)]
      if (!any(d >= as.Date(dts[1]) & d <= as.Date(dts[2]), na.rm = TRUE))
        hints <- c(hints, list(div(class = "hint", "No fish were released in the selected years within these dates. Widen the dates or change the years.")))
    }
    tagList(hints)
  })

  # keep the release-date range in step with the selected years (skips the first
  # value set when the year box is built, so start-up does not look like a change)
  years_seen <- reactiveVal(FALSE)
  observeEvent(input$years, {
    if (!isolate(years_seen())) { years_seen(TRUE); return() }
    b <- raw(); req(b)
    d <- as.Date(b$tag$reldt_posix)
    if (length(input$years)) d <- d[b$tag$year %in% as.integer(input$years)]
    d <- d[!is.na(d)]
    if (length(d)) updateDateRangeInput(session, "dates", start = min(d), end = max(d))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Reactive: which populations to actually filter by (handles multi-select auto-sync)
  selected_populations <- reactive({
    if ("all" %in% input$pop) {
      INDIVIDUAL_SITES  # Return all 6 sites
    } else if (length(input$pop) == 0) {
      INDIVIDUAL_SITES  # Empty defaults to all
    } else {
      input$pop  # Use selected individual sites
    }
  })

  # Smart auto-sync observer: enforce exclusive mode for "All Populations"
  observeEvent(input$pop, {
    current <- input$pop

    # Rule 1: User clicked "All Populations" alongside individual sites
    if ("all" %in% current && length(current) > 1) {
      updatePickerInput(session, "pop", selected = "all")
    }
    # Rule 2: User selected all 6 individual sites
    else if (setequal(current, INDIVIDUAL_SITES)) {
      updatePickerInput(session, "pop", selected = "all")
    }
    # Rule 3: User clicked an individual site while "All Populations" was selected
    else if ("all" %in% current && length(setdiff(current, "all")) > 0) {
      new_sites <- setdiff(current, "all")
      updatePickerInput(session, "pop", selected = new_sites)
    }
  }, ignoreInit = TRUE)

  pop_name <- reactive({
    s <- sel()
    pops <- selected_populations()
    if (setequal(pops, INDIVIDUAL_SITES)) "All populations"
    else {
      labels <- sapply(pops, function(code) {
        k <- match(code, RELEASE$code)
        if (is.na(k)) code else RELEASE$label[k]
      })
      paste(labels, collapse = ", ")
    }
  })

  tags_f <- reactive({
    b <- raw(); req(b); s <- sel()
    tg <- b$tag
    pops_to_filter <- selected_populations()
    tg <- tg[tg$relsite %in% pops_to_filter, , drop = FALSE]
    if (length(s$years)) tg <- tg[tg$year %in% as.integer(s$years), , drop = FALSE]
    if (length(s$dates) == 2 && !any(is.na(s$dates))) {
      d <- as.Date(tg$reldt_posix); tg <- tg[!is.na(d) & d >= min(s$dates) & d <= max(s$dates), , drop = FALSE] }
    tg
  })
  cp_f <- reactive({ b <- raw(); req(b); tg <- tags_f(); b$cp[b$cp$pit %in% tg$pit, , drop = FALSE] })
  rate_f   <- reactive({ rate_by_site(tags_f(), cp_f()) })
  funnel_f <- reactive({ funnel_from(rate_f()) })
  fish_f   <- reactive({ b <- raw(); req(b); fish_summary(tags_f(), cp_f(), b$obs_n) })
  speed_f  <- reactive({ speed_from(cp_f(), tags_f()) })

  # ---------------- Overview ----------------
  output$hero <- renderUI({
    tg <- tags_f(); cp <- cp_f()
    if (nrow(tg) == 0) return(div(class = "hero", h2("No fish in this selection"),
                                  p("Try a different population or year, then press Go.")))
    d1 <- min(tg$reldt_posix, na.rm = TRUE)
    d2 <- max(c(tg$reldt_posix, cp$last_det), na.rm = TRUE)
    n_sites <- length(unique(tg$site_label))
    n_rec <- sum(fish_f()$n_obs)
    div(class = "hero",
        span(class = "badge2", paste("Okanagan Nation Alliance -", pop_name())),
        h2(sprintf("%s juvenile sockeye salmon tracked", fmt_int(nrow(tg)))),
        p(sprintf("Between %s and %s, PIT-tagged sockeye were released at %d location%s. Each fish carries a unique PIT tag, so it is recognised again whenever and wherever it passes a detection site.",
                  fmt_date(d1), fmt_date(d2), n_sites, if (n_sites == 1) "" else "s")),
        div(class = "big", fmt_int(n_rec)), div(class = "bigl", "detection records on file"))
  })

  output$tiles <- renderUI({
    tg <- tags_f(); cp <- cp_f(); b <- raw(); req(b)
    n_det <- length(unique(cp$pit))
    div(class = "tiles",
        stat_tile("Release locations", length(unique(tg$site_label)), "in this selection"),
        stat_tile("Release years", length(unique(tg$year)), paste(range(tg$year), collapse = " - ")),
        stat_tile("Fish detected downstream", fmt_int(n_det),
                  if (nrow(tg)) sprintf("%.1f%% of fish tagged", 100 * n_det / nrow(tg)), "teal"),
        stat_tile("Checkpoints reached", length(unique(cp$cp)), sprintf("of %d", nrow(CHECKPOINTS)), "gold"),
        stat_tile("Latest data export", format(b$export_date, "%b %d, %Y")))
  })

  output$year_bars <- renderUI({
    tg <- tags_f(); if (nrow(tg) == 0) return(div(class = "empty", "No fish in this selection."))
    d <- tg %>% count(year, site_full)
    tot <- d %>% group_by(year) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(desc(year))
    mx <- max(tot$n)
    rows <- lapply(seq_len(nrow(tot)), function(i) {
      di <- d[d$year == tot$year[i], ]
      div(class = "barrow",
          div(class = "bl", tot$year[i]),
          div(class = "bt", style = "display:flex;",
              lapply(seq_len(nrow(di)), function(j)
                div(title = sprintf("%s: %s", di$site_full[j], fmt_int(di$n[j])),
                    style = sprintf("height:100%%;width:%.2f%%;background:%s;", 100 * di$n[j] / mx,
                                    unname(POP_FILL[di$site_full[j]]))))),
          div(class = "bv", fmt_int(tot$n[i])))
    })
    used <- unique(d$site_full)
    div(div(class = "barlist", rows),
        div(class = "legend", lapply(used, function(u)
          span(tags$i(style = paste0("background:", POP_FILL[u])), u))))
  })

  output$site_bars <- renderUI({
    tg <- tags_f(); if (nrow(tg) == 0) return(div(class = "empty", "No fish in this selection."))
    d <- tg %>% count(site_full) %>% arrange(desc(n))
    bar_list(d$site_full, d$n, unname(POP_FILL[d$site_full]), fmt_int(d$n),
             sub = sprintf("%.0f%%", 100 * d$n / sum(d$n)))
  })

  funnel_ui <- function() {
    f <- funnel_f()
    if (nrow(f) == 0) return(div(class = "empty", "No detections in this selection."))
    cols <- c(rep("#1f7a6d", 1), rep("#d4a24c", 2), rep("#e2573f", 20))[seq_len(nrow(f))]
    bar_list(f$checkpoint, f$pct, cols, sprintf("%.1f%%", f$pct), max_val = 100,
             sub = paste0(fmt_int(f$n_detected), " of ", fmt_int(f$n_applicable)))
  }
  output$funnel_overview <- renderUI(funnel_ui())
  output$funnel_mig      <- renderUI(funnel_ui())

  # ---------------- Explore ----------------
  fish_display <- reactive({
    f <- fish_f()
    data.frame(`PIT tag` = f$pit, `Release location` = f$site_full,
               `Released` = format(f$reldt_posix, "%Y-%m-%d"), `Year` = f$year,
               `Detections` = f$n_obs, `Checkpoints passed` = f$n_cp,
               `Farthest checkpoint` = f$farthest,
               `Reached on` = format(f$t_last, "%Y-%m-%d"),
               `Days from release` = round(f$days_to_last, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$tbl_fish <- renderDT(datatable(fish_display(), rownames = FALSE, filter = "top", selection = "single",
                                        options = list(pageLength = 15, scrollX = FALSE, autoWidth = FALSE)))

  # ---------------- Charts ----------------
  bg_t <- "transparent"
  output$p_year   <- renderPlot({ req(nrow(tags_f()) > 0); gg_release_year(tags_f(), dark()) }, bg = bg_t)
  output$p_rate   <- renderPlot({ req(nrow(rate_f()) > 0); gg_rate(rate_f(), dark()) }, bg = bg_t)
  output$p_count  <- renderPlot({ req(nrow(rate_f()) > 0); gg_count(rate_f(), dark()) }, bg = bg_t)
  output$p_travel <- renderPlot({ req(nrow(cp_f()) > 0); gg_travel(cp_f(), tags_f(), dark()) }, bg = bg_t)
  output$p_weekly <- renderPlot({ req(nrow(cp_f()) > 0); gg_weekly(cp_f(), dark()) }, bg = bg_t)

  rate_tbl <- reactive({
    r <- rate_f(); if (nrow(r) == 0) return(data.frame())
    data.frame(Checkpoint = as.character(r$checkpoint), `Release location` = r$site_full,
               `Fish released upstream` = r$n_applicable, `Fish detected` = r$n_detected,
               `Detection rate (%)` = r$rate, check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$t_rate <- renderTable(rate_tbl(), striped = TRUE, digits = 1)

  travel_tbl <- reactive({
    cp <- cp_f(); tg <- tags_f(); if (nrow(cp) == 0) return(data.frame())
    d <- cp %>% inner_join(tg[, c("pit", "reldt_posix")], by = "pit")
    d$days <- as.numeric(difftime(d$first_det, d$reldt_posix, units = "days"))
    d <- d[!is.na(d$days) & d$days > 0, ]
    if (nrow(d) == 0) return(data.frame())
    d %>% group_by(idx) %>%
      summarise(Fish = n(), `Mean (days)` = round(mean(days), 1), `Median (days)` = round(median(days), 1),
                `SD (days)` = round(sd(days), 1), `Min (days)` = round(min(days), 1),
                `Max (days)` = round(max(days), 1), .groups = "drop") %>%
      mutate(Checkpoint = CHECKPOINTS$label[idx]) %>%
      select(Checkpoint, everything(), -idx) %>% as.data.frame(check.names = FALSE)
  })
  output$t_travel <- renderTable(travel_tbl(), striped = TRUE)

  # ---------------- Journeys ----------------
  journeys <- reactive({
    f <- fish_f(); f <- f[f$n_cp > 0, ]
    f[order(-f$last_idx, f$days_to_last), ]
  })
  output$journey_intro <- renderUI({
    j <- journeys()
    div(class = "sub", sprintf("%s tagged fish have been detected downstream of their release site. Click a row to trace its journey.",
                               fmt_int(nrow(j))))
  })
  output$tbl_journeys <- renderDT({
    j <- journeys()
    d <- data.frame(`PIT tag` = j$pit, `Released from` = j$site_full,
                    `Released` = format(j$reldt_posix, "%Y-%m-%d"),
                    `Farthest checkpoint` = j$farthest, `Reached on` = format(j$t_last, "%Y-%m-%d"),
                    `Days since release` = round(j$days_to_last, 1), `Checkpoints passed` = j$n_cp,
                    check.names = FALSE, stringsAsFactors = FALSE)
    datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 12, scrollX = FALSE, autoWidth = FALSE))
  })
  output$journey_detail <- renderUI({
    i <- input$tbl_journeys_rows_selected; req(length(i) == 1)
    j <- journeys()[i, ]
    steps <- cp_f() %>% filter(pit == j$pit) %>% arrange(idx)
    div(class = "card", style = "margin:0 0 14px;",
        h3(j$pit, style = "font-family:Consolas,monospace;font-size:15px;"),
        p(sprintf("Released at %s on %s", j$site_full, format(j$reldt_posix, "%b %d, %Y"))),
        tags$ol(lapply(seq_len(nrow(steps)), function(k)
          tags$li(sprintf("%s - first seen %s (%s days after release)", CHECKPOINTS$label[steps$idx[k]],
                          format(steps$first_det[k], "%b %d, %Y"),
                          round(as.numeric(difftime(steps$first_det[k], j$reldt_posix, units = "days")), 1))))))
  })

  # ---------------- Migration ----------------
  mig_range <- reactive({
    tg <- tags_f(); cp <- cp_f()
    d1 <- as.Date(min(tg$reldt_posix, na.rm = TRUE))
    d2 <- if (nrow(cp)) as.Date(max(cp$first_det)) else d1 + 1
    if (!is.finite(as.numeric(d1))) { d1 <- Sys.Date() - 30; d2 <- Sys.Date() }
    if (d2 <= d1) d2 <- d1 + 1
    c(d1, d2)
  })
  output$mig_slider_ui <- renderUI({
    r <- mig_range()
    sliderInput("mig_date", "Date", min = r[1], max = r[2],
                value = isolate(min(max(nz(input$mig_date, r[1]), r[1]), r[2])),
                step = as.numeric(isolate(nz(input$mig_speed, 1))),
                timeFormat = "%b %d, %Y", width = "100%",
                animate = animationOptions(interval = 700, loop = FALSE))
  })
  observeEvent(input$mig_speed, updateSliderInput(session, "mig_date", step = as.numeric(input$mig_speed)),
               ignoreInit = TRUE)

  mig_counts <- reactive({
    req(input$mig_date)
    cp <- cp_f()
    d <- cp[as.Date(cp$first_det) <= as.Date(input$mig_date), , drop = FALSE]
    n <- table(factor(d$idx, levels = seq_len(nrow(CHECKPOINTS))))
    as.integer(n)
  })
  output$mig_tiles <- renderUI({
    n <- mig_counts(); tg <- tags_f()
    rel <- sum(as.Date(tg$reldt_posix) <= as.Date(input$mig_date), na.rm = TRUE)
    div(class = "tiles",
        stat_tile("Released so far", fmt_int(rel), "tagged fish released by this date", "gold"),
        lapply(seq_along(n), function(i) stat_tile(CHECKPOINTS$label[i], fmt_int(n[i]),
                                                   tagList("fish detected by this date", tags$br(), tags$span(style = "opacity:.75;font-size:11px;", CHECKPOINTS$desc[i])), "teal")))
  })

  output$mig_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addPolylines(lng = RIVER$master$lon, lat = RIVER$master$lat, color = "#5b8db8", weight = 3, opacity = 0.7) %>%
      fitBounds(min(CHECKPOINTS$lon) - 0.5, min(CHECKPOINTS$lat) - 0.4, max(RELEASE$lon) + 0.5, max(RELEASE$lat) + 0.4)
  })
  observe({
    input$mig_map_bounds                      # fires once the map exists (and on pan)
    n <- mig_counts(); tg <- tags_f()
    rs <- tg %>% count(relsite) %>% left_join(RELEASE[, c("code", "lat", "lon", "color", "full")], by = c("relsite" = "code"))
    rs <- rs[!is.na(rs$lat), ]
    p <- leafletProxy("mig_map") %>% clearGroup("live") %>% clearGroup("rel")
    if (nrow(rs)) p <- p %>% addCircleMarkers(lng = rs$lon, lat = rs$lat, radius = 6, color = "#fff", weight = 2,
                                              fillColor = rs$color, fillOpacity = 0.95, label = rs$full, group = "rel")
    p %>% addCircleMarkers(lng = CHECKPOINTS$lon, lat = CHECKPOINTS$lat, radius = 5 + 2.2 * sqrt(n),
                           color = "#fff", weight = 2, fillColor = "#e2573f", fillOpacity = 0.9,
                           label = sprintf("%s (%s): %s fish detected so far", CHECKPOINTS$label, CHECKPOINTS$desc, fmt_int(n)), group = "live")
  })

  # spotlight fish
  spot_pit <- reactiveVal(NULL)
  spot_pool <- function(site = "any", dest = "any") {
    f <- fish_f(); f <- f[f$n_cp >= 2, , drop = FALSE]
    if (!identical(site, "any")) f <- f[f$relsite == site, , drop = FALSE]
    if (!identical(dest, "any")) f <- f[!is.na(f$last_idx) & f$last_idx == as.integer(dest), , drop = FALSE]
    f
  }
  pick_spot <- function() {
    f <- spot_pool(nz(input$spot_site, "any"), nz(input$spot_dest, "any"))
    spot_pit(if (nrow(f) == 0) NULL else sample(f$pit, 1))
  }
  # choices follow the current selection: release sites, and where fish ended up
  observeEvent(fish_f(), {
    f <- spot_pool()
    rs <- unique(f$relsite); rs <- RELEASE$code[RELEASE$code %in% rs]
    updateSelectInput(session, "spot_site", selected = "any",
                      choices = c("Any release site" = "any", setNames(rs, RELEASE$full[match(rs, RELEASE$code)])))
    updateSelectInput(session, "spot_dest", selected = "any", choices = dest_choices(f))
    spot_pit(if (nrow(f) == 0) NULL else sample(f$pit, 1))
  })
  dest_choices <- function(f) {
    n <- table(f$last_idx); ix <- sort(as.integer(names(n)))
    c("Any final destination" = "any", setNames(as.character(ix), sprintf("%s (%s fish)", CHECKPOINTS$label[ix], n[as.character(ix)])))
  }
  # picking a release site narrows the destinations that are offered
  observeEvent(input$spot_site, {
    f <- spot_pool(nz(input$spot_site, "any"))
    keep <- isolate(input$spot_dest); ch <- dest_choices(f)
    updateSelectInput(session, "spot_dest", choices = ch, selected = if (!is.null(keep) && keep %in% ch) keep else "any")
  }, ignoreInit = TRUE)
  observeEvent(list(input$spot_site, input$spot_dest), pick_spot(), ignoreInit = TRUE)
  observeEvent(input$spot_any, pick_spot())
  # anchors of the spotlight fish (release, then each checkpoint it was detected at) on the river route
  spot_path <- reactive({
    pit <- spot_pit(); req(pit)
    f <- fish_f(); r <- f[f$pit == pit, ][1, ]
    k <- match(r$relsite, RELEASE$code); req(!is.na(k))
    st <- cp_f() %>% filter(pit == !!pit) %>% arrange(idx)
    rt <- river_route(r$relsite, CHECKPOINTS$cp[max(st$idx)]); req(!is.null(rt))
    dist <- rt$km[match(paste0("cp:", CHECKPOINTS$cp[st$idx]), rt$key)]
    ok <- !is.na(dist); st <- st[ok, , drop = FALSE]; dist <- dist[ok]
    t0 <- as.numeric(as.Date(r$reldt_posix))
    t <- cummax(c(t0, pmax(as.numeric(as.Date(st$first_det)), t0))) + seq_len(nrow(st) + 1) * 1e-6
    list(route = rt,
         a = data.frame(t = t, dist = cummax(c(0, dist)),
                        lat = c(RELEASE$lat[k], CHECKPOINTS$lat[st$idx]),
                        lon = c(RELEASE$lon[k], CHECKPOINTS$lon[st$idx]),
                        label = c(RELEASE$full[k], CHECKPOINTS$label[st$idx]), stringsAsFactors = FALSE))
  })
  # where the fish is on its own date: distance along the river between detections
  spot_state <- reactive({
    sp <- spot_path(); w <- sp$a; rt <- sp$route
    req(input$spot_day); d <- as.numeric(as.Date(input$spot_day)); req(is.finite(d))
    if (d < w$t[1]) return(list(released = FALSE, w = w))
    k <- max(which(w$t <= d)); n <- nrow(w)
    dd <- if (k >= n) w$dist[n] else w$dist[k] + (d - w$t[k]) / (w$t[k + 1] - w$t[k]) * (w$dist[k + 1] - w$dist[k])
    list(released = TRUE, k = k, arrived = k >= n, w = w,
         lat = approx(rt$km, rt$lat, xout = dd, rule = 2, ties = "ordered")$y,
         lon = approx(rt$km, rt$lon, xout = dd, rule = 2, ties = "ordered")$y)
  })
  # animate on the migration map: stations where it was detected, and a dot moving along the river
  observe({
    input$mig_map_bounds
    pit <- spot_pit(); p <- leafletProxy("mig_map") %>% clearGroup("spot")
    if (is.null(pit) || is.null(input$spot_day)) return()
    st <- spot_state(); w <- st$w
    if (nrow(w) > 1) {
      dn <- w[-1, , drop = FALSE]
      reached <- if (isTRUE(st$released)) as.numeric(as.Date(input$spot_day)) >= dn$t else rep(FALSE, nrow(dn))
      lab <- sprintf("%s - detected %s", dn$label, format(as.Date(dn$t, origin = "1970-01-01"), "%b %d, %Y"))
      p <- p %>% addCircleMarkers(lng = dn$lon, lat = dn$lat, radius = 9, color = "#ffd166", weight = 3,
                                  fillColor = "#ffd166", fillOpacity = ifelse(reached, 1, 0), opacity = 1,
                                  label = lab, group = "spot")
    }
    if (isTRUE(st$released))
      p <- p %>% addCircleMarkers(lng = st$lon, lat = st$lat, radius = 7, color = "#ffffff", weight = 3,
                                  fillColor = "#e2573f", fillOpacity = 1, label = pit, group = "spot")
    p
  })
  # the fish has its own small clock: release date to last detection, stops at the end
  output$spot_slider_ui <- renderUI({
    w <- spot_path()$a; r <- range(as.Date(w$t, origin = "1970-01-01"))
    span <- as.numeric(diff(r)); stp <- max(1, ceiling(span / 100))
    div(class = "spot-slider",
        sliderInput("spot_day", NULL, min = r[1], max = max(r[2], r[1] + 1), value = r[1], step = stp,
                    timeFormat = "%b %d, %Y", width = "100%",
                    animate = animationOptions(interval = 400, loop = FALSE)))
  })
  output$spot <- renderUI({
    pit <- spot_pit()
    if (is.null(pit)) return(div(h4("Spotlight fish"), p("No fish detected at two or more checkpoints match these choices. Try another release site or destination.")))
    req(input$spot_day)
    st <- spot_state(); w <- st$w; r <- fish_f()[fish_f()$pit == pit, ][1, ]
    rel_txt <- format(as.Date(r$reldt_posix), "%b %d, %Y")
    d <- as.numeric(as.Date(input$spot_day))
    if (!isTRUE(st$released)) { n_pass <- 0; days <- 0; now <- div(style = "font-size:13px;margin:8px 0;", "Press play to follow this fish.") }
    else {
      n_pass <- st$k - 1; days <- max(0, d - w$t[1])
      now <- div(style = "font-size:13px;margin:8px 0;",
                 if (st$arrived) HTML(sprintf("Last seen at <b>%s</b> on %s.", w$label[st$k], format(as.Date(w$t[st$k], origin = "1970-01-01"), "%b %d, %Y")))
                 else HTML(sprintf("Travelling from <b>%s</b> to <b>%s</b>.", w$label[st$k], w$label[st$k + 1])))
    }
    div(h4("Spotlight fish"),
        p(class = "m", "Released from"), p(r$site_full, style = "font-weight:700;margin-bottom:4px;"),
        div(class = "pit", pit), p(paste("Released", rel_txt), style = "opacity:.8;font-size:12px;margin:0;"),
        now,
        div(style = "display:flex;gap:26px;",
            div(div(class = "n", n_pass), div(style = "font-size:11px;opacity:.8;", "checkpoints passed")),
            div(div(class = "n", round(days)), div(style = "font-size:11px;opacity:.8;", "days since release"))))
  })

  # ---------------- Speed ----------------
  output$speed_tiles <- renderUI({
    s <- speed_f()
    if (nrow(s) == 0) return(div(class = "empty", "No fish were detected at two or more checkpoints in this selection."))
    div(class = "tiles",
        stat_tile("Fish measured", fmt_int(nrow(s))),
        stat_tile("Fastest pace", sprintf("%.1f km/day", max(s$pace)), s$pit[which.max(s$pace)], "teal"),
        stat_tile("Slowest pace", sprintf("%.1f km/day", min(s$pace)), s$pit[which.min(s$pace)], "gold"),
        stat_tile("Average pace", sprintf("%.1f km/day", mean(s$pace))))
  })
  output$speed_list <- renderUI({
    s <- speed_f(); req(nrow(s) > 0)
    s <- if (identical(input$speed_dir, "slow")) s[order(s$pace), ] else s[order(-s$pace), ]
    s <- head(s, 10); mx <- max(speed_f()$pace)
    lapply(seq_len(nrow(s)), function(i)
      div(class = "rank",
          div(class = "m", paste0("#", i)),
          div(div(class = "pit", s$pit[i]), div(class = "m", s$site_full[i])),
          div(class = "bt", div(class = "bf", style = sprintf("width:%.1f%%;background:%s;",
                                                              max(1, 100 * s$pace[i] / mx), unname(POP_FILL[s$site_full[i]])))),
          div(class = "pace", sprintf("%.1f km/day", s$pace[i])),
          div(class = "d", sprintf("%s to %s: %s km in %s days", s$from[i], s$to[i], fmt_int(s$km[i]), round(s$days[i], 1)))))
  })
  output$speed_by_site <- renderUI({
    s <- speed_f(); if (nrow(s) == 0) return(div(class = "empty", "No speed data for this selection."))
    m <- s %>% group_by(site_full) %>% summarise(med = median(pace), n = n(), .groups = "drop") %>% arrange(desc(med))
    bar_list(m$site_full, m$med, unname(POP_FILL[m$site_full]), sprintf("%.1f km/day", m$med),
             sub = paste0("n = ", fmt_int(m$n)))
  })


  # ---------------- Population comparisons (p-values) ----------------
  mig_flags <- reactive({
    b <- raw(); req(b); tg <- tags_f()
    migration_flags(b$tag, b$cp, tg, b$export_date)
  })
  mig_flagged <- reactive({ f <- mig_flags(); if (nrow(f)) f[f$flagged, , drop = FALSE] else f })
  incl_mig <- reactive(isTRUE(input$incl_migrating))
  tags_cmp <- reactive({
    tg <- tags_f(); f <- mig_flagged()
    if (incl_mig() || nrow(f) == 0) return(tg)
    tg[!(paste(tg$relsite, tg$year) %in% paste(f$relsite, f$year)), , drop = FALSE]
  })
  mig_warning <- function() {
    f <- mig_flagged(); if (nrow(f) == 0) return(NULL)
    who <- paste(sprintf("%s %d (released %s; data run to %s, %d days, but these fish usually need about %d)",
                         f$site_full, f$year, format(f$released, "%b %d, %Y"), format(f$cutoff, "%b %d, %Y"),
                         round(f$elapsed), round(f$threshold)), collapse = "; ")
    div(class = "mig-warn",
        strong("May still be migrating: "), paste0(who, ". "),
        "Fewer days have passed between release and the end of the data than these fish usually take to reach Rocky Reach Dam, so a low detection rate may reflect timing, not survival. ",
        if (incl_mig()) "They are included in the tests below because the box above is ticked."
        else "They are left out of the tests below; tick the box above to include them.")
  }
  pop_cmp <- reactive({
    tg <- tags_cmp(); req(nrow(tg) > 0)
    cp <- cp_f(); sp <- speed_f()
    compare_pops(tg, cp[cp$pit %in% tg$pit, , drop = FALSE],
                 if (nrow(sp)) sp[sp$pit %in% tg$pit, , drop = FALSE] else sp)
  })
  n_pops_sel <- reactive(length(unique(tags_cmp()$site_full)))
  need_two <- function() div(class = "empty",
    "Select two or more populations in the left pane and press Go to compare them with a p-value.")

  ptest_ui <- function(r) {
    sig <- !is.na(r$p) && r$p < P_ALPHA
    pairs <- r$pairs
    div(class = "ptest",
        h4(r$title),
        span(class = paste("pbadge", if (sig) "sig" else "ns"), fmt_p(r$p)),
        if (!is.na(r$p)) span(class = "pmeth",
                              if (sig) "probably a real difference" else "no clear difference", " - ", r$method),
        div(class = "pblurb", pop_blurb(r)),
        if (!is.null(pairs) && nrow(pairs))
          tags$table(class = "ptab",
                     tags$tr(tags$th("Population"), tags$th("vs."), tags$th("p-value (Holm-adjusted)"), tags$th("Different?")),
                     lapply(seq_len(nrow(pairs)), function(i)
                       tags$tr(tags$td(pairs$a[i]), tags$td(pairs$b[i]), tags$td(fmt_p(pairs$p[i])),
                               tags$td(if (pairs$p[i] < P_ALPHA) "Probably" else "No clear difference")))),
        if (length(r$dropped)) div(class = "pmeth", sprintf("Left out (fewer than %d fish): %s.",
                                                            MIN_FISH_TEST, paste(r$dropped, collapse = "; "))))
  }
  pvals_note <- div(class = "small-note", style = "font-size:12px;",
    "How to read this: a p-value is how often chance alone would produce a gap this large if the populations really did not differ. ",
    "Under 0.05 (highlighted) is the usual cut-off for \"probably real\". A p-value does not say how big or how important a gap is, ",
    "and it does not say what caused it: release timing, fish size and route also differ between populations. ",
    "Detection at Rocky Reach mixes survival with the chance of being detected, so it is not a survival estimate. ",
    "Hatchery fish start upstream at Penticton, so their pace covers a longer stretch of river than lake fish.")

  output$pop_tests <- renderUI({
    if (n_pops_sel() < 2) return(div(mig_warning(), need_two()))
    r <- pop_cmp()
    div(mig_warning(), ptest_ui(r$detect), ptest_ui(r$days), ptest_ui(r$pace), pvals_note)
  })
  output$pace_test <- renderUI({
    if (n_pops_sel() < 2) return(mig_warning())
    div(style = "margin-top:10px;", mig_warning(), ptest_ui(pop_cmp()$pace))
  })
  pvals_tbl <- reactive({
    if (n_pops_sel() < 2) return(data.frame(Note = "Select two or more populations to compare."))
    r <- pop_cmp()
    do.call(rbind, lapply(r, function(x) {
      main <- data.frame(Measure = x$title, Comparison = "All selected populations", Test = nz(x$method, NA),
                         `p-value` = signif(x$p, 4), Summary = pop_blurb(x), check.names = FALSE, stringsAsFactors = FALSE)
      if (!is.null(x$pairs) && nrow(x$pairs))
        main <- rbind(main, data.frame(Measure = x$title, Comparison = paste(x$pairs$a, "vs.", x$pairs$b),
                                       Test = "pairwise, Holm-adjusted", `p-value` = signif(x$pairs$p, 4), Summary = "",
                                       check.names = FALSE, stringsAsFactors = FALSE))
      main
    }))
  })

  # ---------------- Survival ----------------
  surv_res <- reactive({
    b <- raw(); req(b); tg <- tags_f(); req(nrow(tg) > 0)
    tg$grp <- if (identical(input$surv_mode, "all")) tg$site_full else paste0(tg$site_full, " - ", tg$year)
    gl <- split(tg, tg$grp)
    out <- list()
    withProgress(message = "Fitting survival models", value = 0, {
      for (g in names(gl)) {
        incProgress(1 / length(gl), detail = g)
        r <- survival_group(gl[[g]], b$site_first, CHAINS[[chain_for(gl[[g]]$relsite[1])]])
        r$group <- g; r$site_full <- gl[[g]]$site_full[1]
        out[[g]] <- r
      }
    })
    out
  })
  bind_part <- function(part) {
    r <- surv_res()
    d <- do.call(rbind, lapply(r, function(x) {
      y <- x[[part]]; if (is.null(y) || nrow(y) == 0) return(NULL)
      y$group_label <- x$group; y$site_full <- x$site_full; y$n_released <- x$n; y }))
    if (is.null(d)) data.frame() else d
  }
  surv_long <- reactive(bind_part("cjs"))
  tt_long   <- reactive(bind_part("tt"))
  det_long  <- reactive(bind_part("det"))

  surv_plot_rows <- reactive({ d <- surv_long(); if (nrow(d) == 0) 0 else sum(d$type != "Detection") })
  tt_plot_rows   <- reactive({ d <- tt_long();   if (nrow(d) == 0) 0 else sum(d$n > 0) })
  output$p_surv <- renderPlot({
    d <- surv_long(); req(nrow(d) > 0); gg_surv(d, dark())
  }, height = function() plot_rows_height(surv_plot_rows()), bg = "transparent")
  output$p_tt_means <- renderPlot({
    d <- tt_long(); req(nrow(d) > 0); gg_tt_means(d, dark())
  }, height = function() plot_rows_height(tt_plot_rows(), per = 34), bg = "transparent")

  surv_tbl <- reactive({
    d <- surv_long(); if (nrow(d) == 0) return(data.frame())
    data.frame(`Release group` = d$group_label, `Fish released` = d$n_released, Parameter = d$parameter,
               Estimate = round(d$estimate, 4), `Standard error` = round(d$se, 4),
               Note = ifelse(d$unstable, "Unstable - too few detections", ""),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  tt_tbl <- reactive({
    d <- tt_long(); d <- d[d$n > 0, , drop = FALSE]; if (nrow(d) == 0) return(data.frame())
    data.frame(`Release group` = d$group_label, Reach = d$reach, `Fish (n)` = d$n,
               `Arithmetic mean (days)` = round(d$arith_mean, 3), `Arithmetic SD` = round(d$arith_sd, 3),
               `Harmonic mean (days)` = round(d$harm_mean, 3), `Harmonic SE` = round(d$harm_se, 3),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$surv_omitted <- renderUI({
    r <- surv_res(); none <- names(r)[vapply(r, function(x) is.null(x$cjs), logical(1))]
    if (length(none)) div(class = "small-note", style = "font-size:12px;",
                          "No fish were detected at the modeled checkpoints for: ", paste(none, collapse = "; "), ".")
  })
  output$t_surv <- renderTable(surv_tbl(), striped = TRUE, na = "-")

  # p-values between release groups for the same survival estimate
  surv_cmp <- reactive({
    d <- surv_long(); f <- mig_flagged()
    if (!incl_mig() && nrow(f) && nrow(d)) {
      bad <- if (identical(input$surv_mode, "all")) unique(f$site_full) else paste0(f$site_full, " - ", f$year)
      d <- d[!(d$group_label %in% bad), , drop = FALSE]
    }
    compare_survival(d)
  })
  output$surv_tests <- renderUI({
    r <- surv_cmp()
    if (length(r) == 0)
      return(div(class = "ptest", mig_warning(), h4("Do survival estimates differ? (p-values)"),
                 div(class = "pblurb", "Select two or more populations (or release years) that share a survival estimate, then press Go.")))
    blocks <- lapply(r, function(x) {
      pr <- x$pairs
      best <- if (!is.null(pr) && nrow(pr)) min(pr$p) else NA
      div(class = "ptest",
          h4(paste0("Survival p-values: ", sub("^Survival: ", "", x$parameter))),
          if (!is.na(best)) span(class = paste("pbadge", if (best < P_ALPHA) "sig" else "ns"),
                                 if (nrow(pr) == 1) fmt_p(pr$p[1]) else paste("smallest", fmt_p(best))),
          if (!is.na(best)) span(class = "pmeth", "z-test on the model estimates", if (nrow(pr) > 1) ", Holm-adjusted"),
          div(class = "pblurb", surv_blurb(x)),
          if (!is.null(pr) && nrow(pr) > 1)
            tags$table(class = "ptab",
                       tags$tr(tags$th("Release group"), tags$th("Survival"), tags$th("vs."), tags$th("Survival"),
                               tags$th("p-value"), tags$th("Different?")),
                       lapply(seq_len(nrow(pr)), function(i)
                         tags$tr(tags$td(pr$a[i]), tags$td(sprintf("%.0f%%", 100 * pr$ea[i])),
                                 tags$td(pr$b[i]), tags$td(sprintf("%.0f%%", 100 * pr$eb[i])),
                                 tags$td(fmt_p(pr$p[i])),
                                 tags$td(if (pr$p[i] < P_ALPHA) "Probably" else "No clear difference")))),
          if (length(x$dropped)) div(class = "pmeth", "Left out (unstable estimate): ", paste(x$dropped, collapse = "; "), "."))
    })
    div(style = "margin-top:14px;", mig_warning(),
        if (nrow(mig_flagged()) && !incl_mig() && identical(input$surv_mode, "all"))
          div(class = "pmeth", "With \"Selected years combined\", a population is left out entirely if any of its years may still be migrating; switch to \"Each release year separately\" to compare the finished years."),
        blocks,
        div(class = "small-note", style = "font-size:12px;",
            "Only the same survival estimate is compared between groups. Detection probability and lambda are not compared: ",
            "lambda mixes survival with detection at the last checkpoint, so a difference in lambda is not a difference in survival. ",
            "Survival from release covers different distances for different release sites (Skaha Lake fish pass more river and dams than Osoyoos Lake fish ",
            "before Rocky Reach), so a real difference here does not say why it happened."))
  })
  output$t_tt_means <- renderTable(tt_tbl(), striped = TRUE, na = "-")
  output$t_surv_det <- renderTable({
    d <- det_long(); if (nrow(d) == 0) return(data.frame())
    data.frame(`Release group` = d$group_label, `Fish released` = d$n_released,
               Checkpoint = d$site, `Fish detected` = d$fish_detected, check.names = FALSE)
  }, striped = TRUE)


  # ---------------- Recaptures (display only) ----------------
  recap_all <- reactiveVal(NULL)
  observeEvent(TRUE, recap_all(tryCatch(load_recaptures(DATA_DIR), error = function(e) NULL)), once = TRUE)
  observeEvent(input$go, recap_all(tryCatch(load_recaptures(DATA_DIR), error = function(e) NULL)), ignoreInit = TRUE)
  recap_f <- reactive({
    r <- recap_all(); s <- sel()
    if (is.null(r)) return(NULL)
    if (!identical(s$pop, "all")) r <- r[r$group == s$pop, , drop = FALSE]
    if (length(s$years)) r <- r[!is.na(r$year) & r$year %in% as.integer(s$years), , drop = FALSE]
    if (length(s$dates) == 2 && !any(is.na(s$dates)))
      r <- r[!is.na(r$rel_date) & r$rel_date >= min(s$dates) & r$rel_date <= max(s$dates), , drop = FALSE]
    r
  })
  output$recap_tiles <- renderUI({
    r <- recap_f()
    if (is.null(r)) return(div(class = "empty", "No recapture file found. Put a file with \"Recapture\" in its name in the data folder and press Go."))
    if (!nrow(r)) return(div(class = "empty", "No recaptured fish for this selection."))
    div(class = "tiles",
        stat_tile("Fish recaptured", fmt_int(n_distinct(r$pit))),
        stat_tile("Places recaptured", fmt_int(n_distinct(r$recap_label))),
        stat_tile("Typical days at large", sprintf("%.0f", median(r$days, na.rm = TRUE)), "median, release to recapture", "teal"),
        stat_tile("Latest recapture", fmt_date(max(r$recap_date))))
  })
  output$recap_by_site <- renderUI({
    r <- recap_f(); req(!is.null(r), nrow(r) > 0)
    d <- r %>% count(recap_label) %>% arrange(desc(n))
    bar_list(d$recap_label, d$n, rep("#e2573f", nrow(d)), fmt_int(d$n))
  })
  recap_tbl <- reactive({
    r <- recap_f(); req(!is.null(r))
    data.frame(`PIT tag` = r$pit, `Population` = r$pop_label, `Released from` = r$rel_label,
               `Released` = format(r$rel_date, "%Y-%m-%d"), `Recaptured at` = r$recap_label,
               `Recaptured on` = format(r$recap_date, "%Y-%m-%d"), `Days at large` = r$days,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$tbl_recap <- renderDT({
    datatable(recap_tbl(), rownames = FALSE, options = list(pageLength = 12, scrollX = FALSE, autoWidth = FALSE))
  })
  output$recap_map <- renderLeaflet({
    r <- recap_f()
    m <- leaflet() %>% addTiles()
    if (is.null(r) || !nrow(r)) return(m %>% setView(-119.6, 49.2, 7))
    r <- r[!is.na(r$recap_lat) & !is.na(r$rel_lat), , drop = FALSE]
    if (!nrow(r)) return(m %>% setView(-119.6, 49.2, 7))
    for (i in seq_len(nrow(r)))
      m <- m %>% addPolylines(lng = c(r$rel_lon[i], r$recap_lon[i]), lat = c(r$rel_lat[i], r$recap_lat[i]),
                              color = r$color[i], weight = 2, opacity = 0.6, dashArray = "5,5")
    rs <- r %>% group_by(rel_label, rel_lat, rel_lon, color) %>% summarise(n = n(), .groups = "drop")
    m <- m %>% addCircleMarkers(lng = rs$rel_lon, lat = rs$rel_lat, radius = 6 + 2 * sqrt(rs$n), color = "#ffffff",
                                weight = 2, fillColor = rs$color, fillOpacity = 0.95,
                                label = paste0(rs$rel_label, ": ", rs$n, " recaptured"))
    cs <- r %>% group_by(recap_label, recap_lat, recap_lon) %>% summarise(n = n(), .groups = "drop")
    m <- m %>% addCircleMarkers(lng = cs$recap_lon, lat = cs$recap_lat, radius = 6 + 2 * sqrt(cs$n), color = "#ffffff",
                                weight = 2, fillColor = "#e2573f", fillOpacity = 0.95,
                                label = paste0(cs$recap_label, ": ", cs$n, " fish caught again"))
    m %>% fitBounds(min(c(r$rel_lon, r$recap_lon)) - 0.3, min(c(r$rel_lat, r$recap_lat)) - 0.3,
                    max(c(r$rel_lon, r$recap_lon)) + 0.3, max(c(r$rel_lat, r$recap_lat)) + 0.3)
  })


  # ---------------- Left-pane selection summary (static, updates on Go) ----------------
  pv_rows <- function(...) {
    x <- list(...); div(lapply(seq(1, length(x), by = 2), function(i) div(class = "pv-r", span(x[[i]]), span(x[[i + 1]]))))
  }
  output$side_summary <- renderUI({
    tg <- tags_f(); req(nrow(tg) > 0)
    cp <- cp_f(); n <- nrow(tg)
    det <- n_distinct(cp$pit)
    far <- if (nrow(cp)) max(cp$idx) else NA
    n_far <- if (is.na(far)) 0 else n_distinct(cp$pit[cp$idx == far])
    bon <- cp[cp$idx == match("BON", CHECKPOINTS$cp), , drop = FALSE]
    bon <- merge(bon, tg[, c("pit", "reldt_posix")], by = "pit")
    med <- if (nrow(bon)) median(as.numeric(difftime(bon$first_det, bon$reldt_posix, units = "days"))) else NA
    rc <- recap_f(); nrc <- if (is.null(rc)) NA else n_distinct(rc$pit)
    ys <- sel()$years; yr_txt <- if (length(ys)) paste(sort(ys), collapse = ", ") else "all years"
    div(class = "pv", h4("This selection"),
        div(class = "pv-s", paste0(pop_name(), " | ", yr_txt, if (length(sel()$dates) == 2) paste0(" | ", format(sel()$dates[1]), " to ", format(sel()$dates[2])) else "")),
        pv_rows("Fish released", fmt_int(n),
                "Detected downstream", sprintf("%s (%.1f%%)", fmt_int(det), 100 * det / n),
                "Farthest checkpoint", if (is.na(far)) "none yet" else sprintf("%s (%s)", CHECKPOINTS$label[far], fmt_int(n_far)),
                "Median days to Bonneville Dam", if (is.na(med)) "-" else sprintf("%.1f", med),
                "Recaptured", if (is.na(nrc)) "-" else fmt_int(nrc)))
  })


  # ---------------- Photos (read from GitHub) ----------------
  photos <- reactiveVal(NULL)          # NULL = not loaded, "error" = unreachable
  load_photos <- function() {
    d <- tryCatch(fetch_photo_index(), error = function(e) "error")
    photos(d)
    if (is.data.frame(d))
      session$sendCustomMessage("ss_photos", unname(Map(function(u, c) list(url = u, caption = c), d$url, d$caption)))
    if (is.data.frame(d)) {
      al <- sort(unique(d$album))
      updateSelectInput(session, "photo_album", choices = c("All photos" = "all", setNames(al, al)),
                        selected = if (isolate(input$photo_album) %in% al) isolate(input$photo_album) else "all")
    }
  }
  observeEvent(TRUE, load_photos(), once = TRUE)
  observeEvent(input$photo_refresh, load_photos(), ignoreInit = TRUE)
  observeEvent(input$go, load_photos(), ignoreInit = TRUE)
  photo_n <- reactiveVal(48)
  observeEvent(list(input$photo_album, input$photo_q), photo_n(48), ignoreInit = TRUE)
  observeEvent(input$photo_more, photo_n(photo_n() + 48))
  photos_f <- reactive({
    d <- photos(); req(is.data.frame(d))
    if (!is.null(input$photo_album) && !identical(input$photo_album, "all")) d <- d[d$album == input$photo_album, , drop = FALSE]
    q <- trimws(nz(input$photo_q, ""))
    if (nzchar(q)) d <- d[grepl(q, paste(d$caption, d$album), ignore.case = TRUE, fixed = FALSE), , drop = FALSE]
    d
  })
  output$photo_count <- renderUI({
    d <- photos()
    if (is.null(d)) return(div(class = "sub", "Loading photos..."))
    if (identical(d, "error")) return(div(class = "empty", "Could not reach the photo library. Check the internet connection and press Refresh photos."))
    f <- photos_f()
    if (nrow(d) == 0) return(div(class = "empty", "No photos yet."))
    div(class = "sub", sprintf("Showing %s of %s photos", fmt_int(min(nrow(f), photo_n())), fmt_int(nrow(f))))
  })
  output$photo_grid <- renderUI({
    d <- photos(); req(is.data.frame(d), nrow(d) > 0)
    f <- photos_f(); if (nrow(f) == 0) return(div(class = "empty", "No photos match."))
    f <- head(f, photo_n())
    div(class = "pgrid", lapply(seq_len(nrow(f)), function(i) {
      k <- match(f$path[i], d$path)
      div(class = "pitem", onclick = sprintf("Shiny.setInputValue('photo_open', %d, {priority:'event'})", k),
          tags$img(src = f$url[i], alt = f$caption[i], loading = "lazy"),
          div(class = "cap", f$caption[i], if (f$album[i] != "Photos") span(class = "alb", f$album[i])))
    }))
  })
  output$photo_more <- renderUI({
    d <- photos(); req(is.data.frame(d), nrow(d) > 0)
    if (nrow(photos_f()) > photo_n()) actionButton("photo_more", "Show more", class = "btn-default")
  })
  observeEvent(input$photo_open, {
    d <- photos(); req(is.data.frame(d)); r <- d[input$photo_open, ]; req(nrow(r) == 1)
    showModal(modalDialog(title = r$caption, size = "l", easyClose = TRUE, footer = modalButton("Close"),
                          div(class = "pmodal", tags$img(src = r$url, alt = r$caption),
                              p(style = "margin-top:10px;font-size:12px;", tags$a(href = r$url, target = "_blank", "Open original")))))
  })

  # ---------------- Exports (light theme, versioned, UTC stamp) ----------------
  stamp <- function() tz_now("%m%d%Y_%H%M%S")
  # every exported plot carries a visible footer: population, years, version, date and time (UTC)
  meta  <- function(p, when = tz_now("%Y-%m-%d %H:%M:%S")) {
    yrs <- sel()$years
    yr_txt <- if (length(yrs)) paste(sort(yrs), collapse = ", ") else "all years"
    p + labs(caption = sprintf("%s  |  Release years: %s  |  v%s  |  Generated %s UTC",
                               pop_name(), yr_txt, APP_VERSION, when)) +
      theme(plot.caption = element_text(size = 10, colour = "#333333", hjust = 0,
                                        margin = margin(t = 10)),
            plot.caption.position = "plot")
  }
  dl_png <- function(name, builder, w = 9, h = 5)
    downloadHandler(filename = function() sprintf("%s_v%s_%s.png", name, APP_VERSION, stamp()),
                    content  = function(file) ggsave(file, meta(builder()), width = w, height = h, dpi = 150, bg = "white"))
  dl_csv <- function(name, getter)
    downloadHandler(filename = function() sprintf("%s_v%s_%s.csv", name, APP_VERSION, stamp()),
                    content  = function(file) write.csv(getter(), file, row.names = FALSE))

  output$dl_p_year   <- dl_png("fish_released_by_year", function() gg_release_year(tags_f(), FALSE))
  output$dl_p_rate   <- dl_png("detection_rate",        function() gg_rate(rate_f(), FALSE))
  output$dl_p_count  <- dl_png("fish_detected",         function() gg_count(rate_f(), FALSE))
  output$dl_p_travel <- dl_png("travel_time",           function() gg_travel(cp_f(), tags_f(), FALSE), h = 6.5)
  output$dl_p_weekly <- dl_png("weekly_detections",     function() gg_weekly(cp_f(), FALSE))
  output$dl_t_rate   <- dl_csv("detection_rate_table",  rate_tbl)
  output$dl_t_travel <- dl_csv("travel_time_table",     travel_tbl)
  output$dl_p_surv     <- dl_png("survival_lambda",        function() gg_surv(surv_long(), FALSE), h = max(5, plot_rows_height(surv_plot_rows()) / 80))
  output$dl_p_tt_means <- dl_png("travel_time_means",      function() gg_tt_means(tt_long(), FALSE), h = max(5, plot_rows_height(tt_plot_rows(), per = 34) / 80))
  output$dl_t_surv     <- dl_csv("survival_lambda_table",  surv_tbl)
  output$dl_pvals      <- dl_csv("population_p_values",    pvals_tbl)
  output$dl_t_tt_means <- dl_csv("travel_time_mean_table", tt_tbl)
  output$dl_fish     <- dl_csv("tag_records",           fish_display)
  output$dl_recap <- dl_csv("recaptured_fish", function() recap_tbl())
  output$dl_journeys <- dl_csv("fish_journeys", function() {
    j <- journeys(); data.frame(pit = j$pit, release_location = j$site_full, released = j$reldt_posix,
                                farthest_checkpoint = j$farthest, reached = j$t_last, days = j$days_to_last)
  })
  output$dl_speed <- dl_csv("fish_speed", function() {
    s <- speed_f(); s[, c("pit", "site_full", "from", "to", "km", "days", "pace")]
  })


  # ---------------- Nerki, the built-in helper ----------------
  nk_chips_for <- function(tab) { ch <- NERKI_CHIPS[[tab]]; if (is.null(ch)) NERKI_CHIPS$default else ch }
  nk_first_q <- function(ids) { ids <- ids[ids %in% names(NERKI_KB)]
    vapply(ids, function(i) { s <- NERKI_KB[[i]]$q[1]; paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), "?") }, "", USE.NAMES = FALSE) }
  nk_go <- function(tabname) if (is.null(tabname)) "" else sprintf("<br><button type=button class=nk-go data-tab=\"%s\">Open the %s tab</button>", tabname, tabname)
  nk_sel_txt <- function() {
    s <- sel(); yrs <- if (length(s$years)) paste(sort(s$years), collapse = ", ") else "all years"
    sprintf("<b>%s</b>, %s", pop_name(), yrs)
  }
  nk_cp_from <- function(tok, default = "BON") { hit <- intersect(tok, names(NK_CP)); if (length(hit)) NK_CP[[hit[1]]] else default }
  nk_pop_from <- function(tok) { hit <- intersect(tok, names(NK_POP)); if (length(hit)) unique(unname(NK_POP[hit])) else character() }
  nk_live <- function(kind, tok) {
    b <- raw(); tg <- tags_f()
    if (is.null(b) || !nrow(tg)) return("There are no fish in the current selection. Pick a population and years in the left panel and press <b>Go</b>, then ask me again.")
    pops <- nk_pop_from(tok)
    if (length(pops)) { tg2 <- tg[tg$relsite %in% pops, , drop = FALSE]; if (nrow(tg2)) tg <- tg2 }
    cp <- cp_f(); cp <- cp[cp$pit %in% tg$pit, , drop = FALSE]
    switch(kind,
      released = {
        n <- table(tg$site_full)
        paste0("In your selection (", nk_sel_txt(), ") <b>", fmt_int(nrow(tg)), "</b> fish were tagged and released",
               if (length(n) > 1) paste0(": ", paste(sprintf("%s %s", names(n), fmt_int(as.integer(n))), collapse = "; ")) else "", ".")
      },
      reached = {
        code <- nk_cp_from(tok); note <- ""
        if (code == "EST" && !any(cp$cp == "EST")) { code <- "BON"; note <- "Few fish are read in the estuary, so here is Bonneville Dam, the last dam before the ocean. " }
        k <- match(code, CHECKPOINTS$cp); lab <- CHECKPOINTS$label[k]
        r <- rate_by_site(tg, cp); r <- r[r$cp_idx == k, , drop = FALSE]
        if (!nrow(r) || sum(r$n_applicable) == 0) return(sprintf("None of the fish in your selection were released upstream of %s, so I can't count them there.", lab))
        tot <- sprintf("<b>%s</b> of %s fish (%.1f%%)", fmt_int(sum(r$n_detected)), fmt_int(sum(r$n_applicable)), 100 * sum(r$n_detected) / sum(r$n_applicable))
        by <- if (nrow(r) > 1) paste0(" By release site: ", paste(sprintf("%s %s of %s", r$site_full, fmt_int(r$n_detected), fmt_int(r$n_applicable)), collapse = "; "), ".") else ""
        paste0(note, "In your selection, ", tot, " were detected at <b>", lab, "</b>.", by,
               " Detected means read by the antennas; some fish pass without being read, so the true number that got there is likely higher.")
      },
      days = {
        code <- nk_cp_from(tok); note <- ""
        if (code == "EST" && !any(cp$cp == "EST")) { code <- "BON"; note <- "Few fish are read in the estuary, so here is Bonneville Dam, the last dam before the ocean. " }
        k <- match(code, CHECKPOINTS$cp); lab <- CHECKPOINTS$label[k]
        a <- merge(cp[cp$idx == k, c("pit", "first_det")], tg[, c("pit", "reldt_posix", "site_full")], by = "pit")
        if (!nrow(a)) return(sprintf("No fish in your selection have been detected at %s yet, so I can't work out a travel time.", lab))
        a$d <- as.numeric(difftime(a$first_det, a$reldt_posix, units = "days"))
        one <- function(x) sprintf("median <b>%.1f days</b> (middle half %.0f to %.0f days, %s fish)", median(x), quantile(x, .25), quantile(x, .75), fmt_int(length(x)))
        g <- split(a$d, a$site_full)
        paste0(note, "From release to the first detection at <b>", lab, "</b>: ",
               if (length(g) > 1) paste(sprintf("%s %s", names(g), vapply(g, one, "")), collapse = "; ") else one(a$d), ".",
               " That counts only fish that were detected there.")
      },
      fast = {
        s <- speed_f(); s <- s[s$pit %in% tg$pit, , drop = FALSE]
        if (!nrow(s)) return("No fish in your selection were seen at two or more checkpoints yet, so there is no pace to compare.")
        slow <- "slow" %in% tok; i <- if (slow) which.min(s$pace) else which.max(s$pace)
        sprintf("The %s fish in your selection is <b>%s</b> from %s: %s km from %s to %s in %.1f days, about <b>%.1f km per day</b>.",
                if (slow) "slowest" else "fastest", s$pit[i], s$site_full[i], fmt_int(s$km[i]), s$from[i], s$to[i], s$days[i], s$pace[i])
      },
      selection = {
        s <- sel(); dts <- if (length(s$dates) == 2) sprintf(", released %s to %s", format(s$dates[1]), format(s$dates[2])) else ""
        sprintf("You are looking at %s%s: %s fish. You are on the <b>%s</b> tab. Change the selection in the left panel and press Go.",
                nk_sel_txt(), dts, fmt_int(nrow(tg)), nk_tab)
      },
      survival = {
        d <- surv_long(); d <- d[d$type == "Survival" & !d$unstable, , drop = FALSE]
        if (!nrow(d)) return("I don't have a stable survival estimate for this selection yet; usually too few fish were detected. Try more fish or a different year.")
        d <- head(d, 6)
        paste0("Estimated survival for your selection: ", paste(sprintf("%s, %s: <b>%.0f%%</b> (give or take %.0f%%)", d$group_label, sub("^Survival: ", "", d$parameter), 100 * d$estimate, 200 * d$se), collapse = "; "),
               ". These come from the Cormack-Jolly-Seber model, which corrects for fish the antennas missed.")
      },
      pvalue = {
        if (n_pops_sel() < 2) {
          if (length(unique(tg$site_full)) >= 2 && nrow(mig_flagged()))
            return("Your selection has groups that may still be migrating, so they are left out of the p-values and fewer than two remain to compare. Tick <b>Include release groups that may still be migrating</b> on the Survival tab to compare them anyway, keeping the timing caveat in mind.")
          return(paste0("A p-value needs two or more populations to compare, and your selection has one. ", NERKI_KB$pvalue$a))
        }
        r <- pop_cmp()$detect; paste0(pop_blurb(r), " See the Survival tab for the travel-time and pace comparisons too.")
      },
      data = {
        last <- suppressWarnings(max(b$cp$last_det, na.rm = TRUE))
        paste0("Data loaded: ", paste(sprintf("%s (exported %s)", b$datasets$dataset, format(b$datasets$date, "%b %d, %Y")), collapse = "; "), ".",
               if (is.finite(last)) sprintf(" The latest detection in the data is %s (UTC).", format(last, "%b %d, %Y")) else "",
               " Detection records reach PTAGIS after a delay, so the most recent weeks may be incomplete.")
      },
      zero = {
        f <- mig_flagged()
        if (nrow(f)) paste0("Some groups in your selection may still be migrating: ",
                             paste(sprintf("%s %d", f$site_full, f$year), collapse = "; "),
                             ". Their fish usually take longer to reach Rocky Reach than the data cover, so low or zero detections there likely mean they haven't left yet, not that they died.")
        else paste0(NERKI_KB$why_undetected$a, " In your selection, ", fmt_int(n_distinct(cp$pit)), " of ", fmt_int(nrow(tg)), " fish have been detected downstream so far.")
      },
      "I'm not sure about that one yet.")
  }
  nk_tab <- "Overview"
  nerki_answer <- function(text, tab) {
    nk_tab <<- if (nzchar(tab)) tab else "Overview"
    chips <- nk_chips_for(tab)
    if (identical(text, "__hello__"))
      return(list(html = "Hi, I am <b>Nerki</b>, your AI helper! I can explain this dashboard, the numbers on your screen and how fish tracking works. Tap a question below or type your own.", chips = chips))
    if (identical(text, "__chips__")) return(list(chips = chips))
    low <- tolower(trimws(text))
    if (grepl("^(hi|hello|hey|good (morning|afternoon|evening))[ !.]*$", low))
      return(list(html = "Hello! Ask me anything about the dashboard or about following sockeye down the river.", chips = chips))
    if (grepl("^(thanks|thank you|thx|ty|cheers|great|awesome|cool|ok|okay)[ !.]*$", low))
      return(list(html = "You're welcome! Ask me anything else any time.", chips = chips))
    m <- nk_match(text)
    if (!is.na(m$best) && m$score >= 0.42) {
      e <- NERKI_KB[[m$best]]
      html <- if (!is.null(e$live)) nk_live(e$live, m$tokens) else e$a
      if (e$id == "who_made" && nzchar(NERKI_CONTACT)) html <- paste0(html, " For questions: ", NERKI_CONTACT, ".")
      rel <- nk_first_q(e$rel)
      return(list(html = paste0(html, nk_go(e$goto)), chips = if (length(rel)) rel else chips))
    }
    message("NERKI_UNANSWERED: ", substr(gsub("[\r\n]+", " ", text), 1, 200))
    if (length(m$top) && !is.na(m$top[1]) && m$score >= 0.2)
      return(list(html = "I'm not quite sure what you mean. Did you mean one of these?", chips = nk_first_q(m$top)))
    list(html = paste0("I don't know that one yet, sorry. I'm a built-in guide, so try asking another way, or tap one of the questions below.",
                       if (nzchar(NERKI_CONTACT)) paste0(" You can also ask a person: ", NERKI_CONTACT, ".") else ""),
         chips = chips)
  }
  observeEvent(input$nerki_q, {
    qq <- input$nerki_q
    txt <- substr(as.character(nz(qq$text, "")), 1, 300); tab <- as.character(nz(qq$tab, ""))
    res <- tryCatch(nerki_answer(txt, tab), error = function(e) {
      message("NERKI_ERROR: ", conditionMessage(e))
      list(html = "Sorry, I tripped over something there. Please try asking another way.", chips = nk_chips_for(tab)) })
    session$sendCustomMessage("nerki_a", res)
  })

  # ---------------- Summary report (one-click, self-contained HTML) ----------------
  # Everything for the current selection in one file: headline numbers, key charts,
  # survival and travel time, p-values with their plain-language notes, and the
  # data sources. Open it in any browser; File > Print > Save as PDF gives a PDF.
  rep_img <- function(p, w = 9, h = 5) {
    f <- tempfile(fileext = ".png"); on.exit(unlink(f))
    ggsave(f, p, width = w, height = h, dpi = 110, bg = "white")
    tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))))
  }
  rep_tbl <- function(d, digits = 3) {
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(tags$p(class = "muted", "Nothing to show for this selection."))
    cells <- lapply(d, function(x) {
      v <- if (is.numeric(x)) format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE) else as.character(x)
      v[is.na(x) | v %in% c("NA", "NaN")] <- "-"; v })
    tags$table(tags$thead(tags$tr(lapply(names(d), tags$th))),
               tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(lapply(cells, function(v) tags$td(v[i]))))))
  }
  rep_safe <- function(expr) tryCatch(expr, error = function(e) tags$p(class = "muted", "This section could not be built for the current selection."))
  REPORT_CSS <- "
body{font-family:'Segoe UI',system-ui,-apple-system,sans-serif;color:#1f2a2b;background:#f3efe8;margin:0;}
.page{max-width:960px;margin:24px auto;background:#fffdf8;border:1px solid #e7e1d5;border-radius:14px;padding:28px 36px;}
h1{font-size:24px;margin:0;} h2{font-size:18px;margin:28px 0 8px;border-bottom:2px solid #1f7a6d;padding-bottom:4px;}
h3{font-size:14px;margin:16px 0 6px;}
.beta{display:inline-block;background:#d4a24c;color:#1f1a0e;font-size:10px;font-weight:800;letter-spacing:.12em;padding:2px 8px;border-radius:999px;vertical-align:middle;margin-left:6px;}
.meta{color:#6b7776;font-size:12.5px;margin-top:6px;line-height:1.5;}
.glance{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin-top:8px;}
.glance div{background:#efe9dc;border-radius:10px;padding:10px 12px;} .glance b{display:block;font-size:20px;} .glance span{font-size:12px;color:#6b7776;}
img{width:100%;height:auto;border:1px solid #e7e1d5;border-radius:8px;margin:6px 0;}
table{border-collapse:collapse;width:100%;font-size:12px;margin:6px 0 10px;} th,td{border-bottom:1px solid #e7e1d5;padding:4px 8px;text-align:left;} th{background:#efe9dc;}
.note{background:#efe9dc;border-left:4px solid #1f7a6d;border-radius:8px;padding:8px 12px;font-size:13px;line-height:1.5;margin:8px 0;}
.warn{background:#fbf1dc;border-left:4px solid #d4a24c;border-radius:8px;padding:8px 12px;font-size:13px;line-height:1.5;margin:8px 0;}
.muted{color:#6b7776;font-size:12.5px;} dl{font-size:12.5px;} dt{font-weight:700;margin-top:6px;} dd{margin:0 0 0 0;color:#333;}
@media print{body{background:#fff;} .page{border:0;margin:0;max-width:none;padding:0;} h2{break-after:avoid;} img,table,.note,.warn{break-inside:avoid;}}
"
  build_report <- function() {
    b <- raw(); tg <- tags_f(); cp <- cp_f(); s <- sel()
    when <- tz_now("%Y-%m-%d %H:%M:%S")
    yr_txt <- if (length(s$years)) paste(sort(s$years), collapse = ", ") else "all years"
    dt_txt <- if (length(s$dates) == 2) paste(format(s$dates[1]), "to", format(s$dates[2])) else "all dates"
    head <- div(h1("Let's Follow the Sockeye: summary report", span(class = "beta", "BETA")),
                div(class = "meta", tags$b("Selection: "), paste0(pop_name(), " | release years: ", yr_txt, " | release dates: ", dt_txt), tags$br(),
                    paste0("Generated ", when, " UTC | dashboard v", APP_VERSION, " | Okanagan Nation Alliance | prepared by Macus Ong")))
    if (is.null(b) || nrow(tg) == 0)
      return(div(class = "page", head, tags$p("There are no fish in this selection. Change the filters, press Go and export again.")))
    n <- nrow(tg); det <- n_distinct(cp$pit)
    far <- if (nrow(cp)) max(cp$idx) else NA
    bon <- merge(cp[cp$idx == match("BON", CHECKPOINTS$cp), , drop = FALSE], tg[, c("pit", "reldt_posix")], by = "pit")
    med <- if (nrow(bon)) median(as.numeric(difftime(bon$first_det, bon$reldt_posix, units = "days"))) else NA
    glance <- div(class = "glance",
      div(tags$b(fmt_int(n)), span("fish tagged and released")),
      div(tags$b(sprintf("%s (%.1f%%)", fmt_int(det), 100 * det / n)), span("detected downstream")),
      div(tags$b(length(unique(tg$site_full))), span("release locations")),
      div(tags$b(if (is.na(far)) "none yet" else CHECKPOINTS$label[far]), span("farthest checkpoint reached")),
      div(tags$b(if (is.na(med)) "-" else sprintf("%.1f", med)), span("median days to Bonneville Dam")),
      div(tags$b(paste(range(tg$year, na.rm = TRUE), collapse = " - ")), span("release years in the data")))
    mig <- rep_safe({ f <- mig_flagged()
      if (nrow(f)) div(class = "warn", tags$b("May still be migrating: "), paste0(
        paste(sprintf("%s %d (released %s; data run to %s)", f$site_full, f$year, format(f$released, "%b %d, %Y"), format(f$cutoff, "%b %d, %Y")), collapse = "; "),
        ". Their low detection rates may reflect timing rather than survival. They are ",
        if (incl_mig()) "included in" else "left out of", " the p-values below.")) })
    n_pop_all <- length(unique(tg$site_full))
    why_none <- if (n_pop_all >= 2) "The selected populations that may still be migrating are left out (see above), so fewer than two remain to compare."
                else "Only one population is selected. Select two or more and press Go to include comparisons."
    comp <- rep_safe({
      if (n_pops_sel() < 2) tags$p(class = "muted", why_none)
      else { r <- pop_cmp(); tagList(lapply(r, function(x) div(tags$h3(paste0(x$title, " (", fmt_p(x$p), ")")), div(class = "note", pop_blurb(x))))) } })
    sv <- rep_safe({ x <- surv_cmp()
      if (length(x)) tagList(lapply(x, function(y) div(tags$h3(paste0("Survival p-values: ", sub("^Survival: ", "", y$parameter))), div(class = "note", surv_blurb(y)))))
      else tags$p(class = "muted", "No survival comparison: it needs two or more release groups with the same stable survival estimate (groups that may still be migrating are left out).") })
    used <- GLOSSARY[c("p-value", "lambda", "Cormack-Jolly-Seber", "harmonic mean", "detection probability", "standard error", "Holm-adjusted")]
    div(class = "page", head,
        h2("At a glance"), glance,
        h2("Releases"), rep_safe(rep_img(gg_release_year(tg, FALSE), 9, 4)),
        h2("Detection at downstream checkpoints"),
        tags$p(class = "muted", "Share of fish detected at each checkpoint, counting only fish released upstream of it. Detection mixes survival with the chance of being read by the antennas."),
        rep_safe(rep_img(gg_rate(rate_f(), FALSE))),
        rep_safe({ rt <- rate_tbl(); if (nrow(rt)) { hit <- tapply(rt[["Fish detected"]], rt$Checkpoint, sum); rt <- rt[rt$Checkpoint %in% names(hit)[hit > 0], , drop = FALSE] }
                   tagList(rep_tbl(rt, 1), tags$p(class = "muted", "Checkpoints where no fish in this selection were detected are left out of the table.")) }),
        h2("Travel time"), rep_safe(rep_img(gg_travel(cp, tg, FALSE), 9, 6)),
        rep_safe(rep_tbl(tt_tbl(), 2)),
        h2("Survival (Cormack-Jolly-Seber)"),
        rep_safe(rep_img(gg_surv(surv_long(), FALSE), 9, max(4, plot_rows_height(surv_plot_rows()) / 80))),
        rep_safe(rep_tbl(surv_tbl(), 3)), sv,
        h2("Do the selected populations differ?"), mig, comp,
        h2("Notes"),
        tags$ul(class = "muted",
          tags$li("Beta: features are still being added and numbers can change as new data arrive."),
          tags$li(paste0("Data: PTAGIS PitPro tagging and interrogation exports. Loaded: ",
                  paste(sprintf("%s (exported %s)", b$datasets$dataset, format(b$datasets$date, "%b %d, %Y")), collapse = "; "), ".")),
          tags$li("All times are UTC. Release years and release dates both filter the selection."),
          tags$li("A p-value says whether a gap is likely to be chance; it does not say how big or important the gap is, or what caused it.")),
        h3("Terms used"),
        tags$dl(lapply(names(used), function(k) tagList(tags$dt(k), tags$dd(used[[k]])))))
  }
  output$dl_report <- downloadHandler(
    filename = function() sprintf("sockeye_summary_report_v%s_%s.html", APP_VERSION, stamp()),
    content = function(file) {
      withProgress(message = "Building the summary report", value = 0.3, {
        body <- build_report(); incProgress(0.6)
        writeLines(c("<!DOCTYPE html>", "<html lang=\"en\"><head><meta charset=\"utf-8\">",
                     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
                     "<title>Sockeye summary report (Beta)</title>", "<style>", REPORT_CSS, "</style></head><body>",
                     as.character(body), "</body></html>"), file, useBytes = TRUE)
      })
    })
}

shinyApp(ui = ui, server = server)
