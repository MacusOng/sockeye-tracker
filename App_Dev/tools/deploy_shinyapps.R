# Publish "Let's Follow the Sockeye" to shinyapps.io (the live website).
# Run from the PROJECT folder (the one that contains App_Dev and data_public):
#   source("App_Dev/tools/deploy_shinyapps.R")
# Uploads ONLY app.R and the files in data_public/ as the app "sockeye-tracker".

APP_NAME  <- "sockeye-tracker"
APP_TITLE <- "Let's Follow the Sockeye"
SRC_APP   <- file.path("App_Dev", "app.R")
PUB_DATA  <- "data_public"

if (!requireNamespace("rsconnect", quietly = TRUE))
  stop("Install rsconnect first: install.packages(\"rsconnect\")")
if (!file.exists(SRC_APP))
  stop("Run this from the project folder (App_Dev/app.R was not found).")
if (!dir.exists(PUB_DATA))
  stop("Create a folder named data_public next to App_Dev and copy into it ",
       "ONLY the data files approved for the public website.")

pub_files <- list.files(PUB_DATA, pattern = "\\.(csv|xlsx)$",
                        full.names = TRUE, ignore.case = TRUE)
if (!length(pub_files)) stop("data_public has no csv or xlsx files.")

# Read the version from app.R so the message shows what is being published.
src_lines <- readLines(SRC_APP, warn = FALSE)
ver_line  <- grep("^APP_VERSION\\s*<-", src_lines, value = TRUE)[1]
app_ver   <- sub('.*"([0-9.]+)".*', "\\1", ver_line)

cat("\nAbout to publish version", app_ver, "\n")
cat("Public data files (", length(pub_files), "):\n", sep = "")
cat(paste0("  ", basename(pub_files), collapse = "\n"), "\n\n")
ans <- readline("These files become publicly viewable through the website. Continue? (y/n) ")
if (!tolower(trimws(ans)) %in% c("y", "yes")) stop("Cancelled. Nothing was uploaded.")

stage <- file.path(tempdir(), "sockeye_deploy")
unlink(stage, recursive = TRUE)
dir.create(file.path(stage, "data"), recursive = TRUE)
file.copy(SRC_APP, file.path(stage, "app.R"))
file.copy(pub_files, file.path(stage, "data"))

rsconnect::deployApp(appDir = stage, appName = APP_NAME, appTitle = APP_TITLE,
                     forceUpdate = TRUE, launch.browser = TRUE)
cat("\nDone. Version", app_ver, "is live.\n")