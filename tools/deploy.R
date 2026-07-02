# =============================================================================
# tools/deploy.R  --  Deploy the dashboard to shinyapps.io
# -----------------------------------------------------------------------------
# Reads shinyapps.io credentials from the environment (never hard-coded):
#   SHINYAPPS_NAME, SHINYAPPS_TOKEN, SHINYAPPS_SECRET
# Get these from https://www.shinyapps.io/admin/#/tokens .
#
# Two variants (choose with the FF_VARIANT env var, default "public" for safety):
#   FF_VARIANT=public   -> anonymized names, app "fantasy-football-analytics-public"
#   FF_VARIANT=private  -> real team names; app name comes from SHINYAPPS_APP in
#                          .Renviron (never hard-coded, so the private URL stays
#                          unguessable even after this repo is public).
#
# BOTH variants deploy from a clean staging copy that contains ONLY the files the
# app needs, with the chosen data placed in data-public/ (the dir app.R loads by
# default). The real-names data/ is therefore *physically absent* from the public
# bundle -- privacy does not depend on .rscignore. The public bundle gets the
# anonymized data-public/; the private bundle gets the real data/.
#
# Build the anonymized data first:  Rscript data-raw/05_make_public_data.R
# App names can be overridden with SHINYAPPS_APP_PUBLIC / SHINYAPPS_APP.
#
# Usage (from project root):
#   Rscript tools/deploy.R                                    # public (default)
#   PowerShell:  $env:FF_VARIANT="private"; Rscript tools/deploy.R   # private
# =============================================================================

library(rsconnect)

name    <- Sys.getenv("SHINYAPPS_NAME")
token   <- Sys.getenv("SHINYAPPS_TOKEN")
secret  <- Sys.getenv("SHINYAPPS_SECRET")
variant <- Sys.getenv("FF_VARIANT", "public")

if (name == "" || token == "" || secret == "") {
  stop("Set SHINYAPPS_NAME, SHINYAPPS_TOKEN, SHINYAPPS_SECRET in the environment.")
}
if (!variant %in% c("private", "public")) {
  stop("FF_VARIANT must be 'public' or 'private' (got '", variant, "').")
}

#' Build a clean staging dir: only the runtime files, with `data_src`'s .rds files
#' placed in data-public/ (the dir app.R loads by default). Returns the dir path.
#' Real names never enter the public staging dir because it is built from scratch.
stage_app <- function(data_src) {
  if (!dir.exists(data_src) || length(list.files(data_src, "[.]rds$")) == 0) {
    stop("No .rds files in '", data_src, "/'. ",
         if (data_src == "data-public") "Run: Rscript data-raw/05_make_public_data.R"
         else "Run: Rscript data-raw/03_clean.R")
  }
  app_dir <- file.path(tempdir(), "ff_deploy_build")
  unlink(app_dir, recursive = TRUE)
  dir.create(app_dir)

  file.copy("app.R", app_dir)
  file.copy("R", app_dir, recursive = TRUE)
  file.copy("renv.lock", app_dir)
  if (file.exists(".Rprofile")) file.copy(".Rprofile", app_dir)
  dir.create(file.path(app_dir, "renv"), showWarnings = FALSE)
  file.copy("renv/activate.R", file.path(app_dir, "renv"))
  if (file.exists("renv/settings.json")) file.copy("renv/settings.json", file.path(app_dir, "renv"))

  dir.create(file.path(app_dir, "data-public"), showWarnings = FALSE)
  file.copy(list.files(data_src, pattern = "[.]rds$", full.names = TRUE),
            file.path(app_dir, "data-public"))
  app_dir
}

if (variant == "public") {
  app_name <- Sys.getenv("SHINYAPPS_APP_PUBLIC", "fantasy-football-analytics-public")
  app_dir  <- stage_app("data-public")            # anonymized names
} else {
  # The private app name is intentionally NOT defaulted here: it lives only in
  # .Renviron (git-ignored) so the private URL stays unguessable even after this
  # repo is published. Its only protection on the free tier is an obscure URL.
  app_name <- Sys.getenv("SHINYAPPS_APP")
  if (app_name == "") {
    stop("Set SHINYAPPS_APP in .Renviron to the private app's (non-obvious) name. ",
         "It's kept out of the repo so the private URL isn't discoverable.")
  }
  app_dir <- stage_app("data")                    # real names (link-shared app only)
}

message("Deploying ", toupper(variant), " variant as '", app_name,
        "' from staging ", app_dir, " ...")

setAccountInfo(name = name, token = token, secret = secret)
deployApp(
  appDir = app_dir,
  appName = app_name,
  appTitle = "Fantasy Football Analytics",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
