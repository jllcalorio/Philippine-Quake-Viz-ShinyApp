# =============================================================================
# Deploy the app to shinyapps.io
# -----------------------------------------------------------------------------
# Used by the monthly workflow. Reads the account details from environment
# variables (GitHub stores them as repository secrets):
#   SHINYAPPS_ACCOUNT   your shinyapps.io account name
#   SHINYAPPS_TOKEN     token  } from shinyapps.io > Account > Tokens
#   SHINYAPPS_SECRET    secret }
#   SHINYAPPS_APP_NAME  optional; defaults to the name below
#
# To deploy from your own computer instead, you can still use RStudio's
# Publish button, or run this file after setting the variables above.
# =============================================================================

APP_NAME <- Sys.getenv("SHINYAPPS_APP_NAME")
if (!nzchar(APP_NAME)) APP_NAME <- "philippine-quake-viz-shinyapp"

env <- Sys.getenv(c("SHINYAPPS_ACCOUNT", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET"))
if (any(!nzchar(env))) {
  stop("Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET before deploying.",
       call. = FALSE)
}

rsconnect::setAccountInfo(name = env[["SHINYAPPS_ACCOUNT"]],
                          token = env[["SHINYAPPS_TOKEN"]],
                          secret = env[["SHINYAPPS_SECRET"]])

rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", list.files("R", full.names = TRUE),
               "default_data.csv", "ph_map.rds", "renv.lock"),
  appName = APP_NAME,
  account = env[["SHINYAPPS_ACCOUNT"]],
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)
