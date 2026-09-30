# Confirms app.R's APP_VERSION matches the newest CHANGELOG.md entry.
app <- readLines("app.R", warn = FALSE)
v_app <- sub('.*APP_VERSION\\s*<-\\s*"([^"]+)".*', "\\1", app[grep("^APP_VERSION\\s*<-", app)][1])
cl <- readLines("CHANGELOG.md", warn = FALSE)
v_log <- sub("^## ([0-9.]+).*", "\\1", cl[grep("^## [0-9]", cl)][1])
cat(sprintf("app.R: %s   CHANGELOG.md: %s\n", v_app, v_log))
if (!identical(v_app, v_log)) { cat("MISMATCH - update app.R or CHANGELOG.md\n"); quit(status = 1) }
cat("OK\n")
