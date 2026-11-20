# =============================================================================
# Monthly data refresh
# -----------------------------------------------------------------------------
# Brings default_data.csv up to the end of the last complete month, using the
# PHIVOLCS monthly archive pages. Run by .github/workflows/monthly-refresh.yml,
# and can also be run by hand from the project folder:
#
#   Rscript scripts/refresh_data.R                 # update default_data.csv
#   Rscript scripts/refresh_data.R --dry-run       # check only, write nothing
#   Rscript scripts/refresh_data.R --output x.csv  # write somewhere else
#   (also --input file, --today YYYY-MM-DD, --last-retry-day N, for testing)
#
# What it does
#   1. Reads default_data.csv.
#   2. Works out which months need refreshing: from the month of the latest
#      event in the file up to the last complete month. (Re-fetching the
#      latest month replaces preliminary "latest events" values with the
#      archive's consolidated listing.)
#   3. Downloads each month's archive page and checks it looks right
#      (see check_month()). If PHIVOLCS changes its page format, these checks
#      fail and nothing is written.
#   4. Replaces those months in the data, re-runs the cleaning steps, checks
#      the result, and writes default_data.csv.
#
# Exit status
#   0  data updated, already up to date, or the archive for last month is not
#      published yet (the workflow retries on later days)
#   1  a check failed, or the archive is still missing on the last retry day.
#      Nothing is written. GitHub e-mails the repository owner.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(rvest)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- Settings -----------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
DATA_FILE   <- arg_value("--input", "default_data.csv")
OUTPUT_FILE <- arg_value("--output", DATA_FILE)
DRY_RUN     <- "--dry-run" %in% args
TODAY       <- as.Date(arg_value("--today", format(Sys.time(), "%Y-%m-%d", tz = PH_TZ)))
# The workflow runs on several days early in the month, because PHIVOLCS
# publishes the archive for a month a few days after it ends. On or after this
# day, a missing archive is treated as an error rather than "try again later".
LAST_RETRY_DAY <- as.integer(arg_value("--last-retry-day", "10"))

MIN_EVENTS_PER_MONTH <- 150   # lowest month since 2017 had 307 events

# ---- Reporting helpers ----------------------------------------------------------
summary_lines <- character()
say <- function(...) {
  line <- paste0(...)
  cat(line, "\n", sep = "")
  summary_lines <<- c(summary_lines, line)
}
finish <- function(status, changed = FALSE, exit_code = 0) {
  # Outputs and a run summary for GitHub Actions (ignored when run by hand).
  out <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(out)) {
    cat(sprintf("changed=%s\nstatus=%s\n", tolower(changed), status),
        file = out, append = TRUE)
  }
  smry <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(smry)) {
    cat("## Monthly data refresh\n\n", paste0(summary_lines, "  \n"), "\n",
        file = smry, append = TRUE, sep = "")
  }
  quit(save = "no", status = exit_code)
}
fail <- function(...) {
  say("**FAILED:** ", ...)
  say("Nothing was written. The app keeps using the existing data.")
  finish("failed", exit_code = 1)
}

# ---- Checks ---------------------------------------------------------------------
# Returns a character vector of problems (empty if the month looks right).
check_month <- function(d, year, month, n_existing) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  last <- ceiling_date(first, "month") - 1
  share <- function(x) mean(x)
  problems <- c(
    if (nrow(d) < MIN_EVENTS_PER_MONTH)
      sprintf("only %d events (expected at least %d)", nrow(d), MIN_EVENTS_PER_MONTH),
    if (n_existing > 0 && nrow(d) < 0.8 * n_existing)
      sprintf("%d events, fewer than the %d already in the data for this month",
              nrow(d), n_existing),
    if (min(d$Date) > first + 2 || max(d$Date) < last - 2)
      sprintf("covers only %s to %s", min(d$Date), max(d$Date)),
    if (share(!is.na(d$Time)) < 0.99) "times could not be read",
    if (share(!is.na(d$Mag) & d$Mag >= 0 & d$Mag < 10) < 0.99) "magnitudes could not be read",
    if (share(!is.na(d$Latitude) & !is.na(d$Longitude)) < 0.99) "coordinates could not be read",
    if (share(!is.na(d$Depth_km)) < 0.95) "depths could not be read",
    if (share(!is.na(d$Province)) < 0.90) "locations (province) could not be read"
  )
  problems
}

# ---- 1. Current data ------------------------------------------------------------
say("Run date (PhST): ", format(TODAY))
old_std <- tryCatch(read_default_data(DATA_FILE),
                    error = function(e) fail("could not read ", DATA_FILE, ": ", conditionMessage(e)))
say(sprintf("Current data: %s events, %s to %s.",
            format(nrow(old_std), big.mark = ","), min(old_std$Date), max(old_std$Date)))

# ---- 2. Months to refresh -------------------------------------------------------
last_complete_end <- floor_date(TODAY, "month") - 1
from <- floor_date(max(old_std$Date), "month")
to <- floor_date(last_complete_end, "month")
if (from > to) {
  say("Data already reaches the current month; nothing to refresh.")
  finish("up-to-date")
}
months <- seq(from, to, by = "month")
say("Months to fetch from the PHIVOLCS archive: ",
    paste(format(months, "%B %Y"), collapse = ", "), ".")

# ---- 3. Download and check each month ------------------------------------------
fresh <- list()
for (i in seq_along(months)) {
  m <- months[i]
  yr <- year(m); mo <- month(m)
  label <- format(m, "%B %Y")
  d <- tryCatch(scrape_month_archive(yr, mo), error = function(e) NULL)

  if (is.null(d) || nrow(d) == 0) {
    is_latest_month <- i == length(months)
    if (is_latest_month && day(TODAY) < LAST_RETRY_DAY) {
      say(sprintf("The archive page for %s is not published yet. ", label),
          sprintf("No changes made; the workflow will try again (last try on day %d).",
                  LAST_RETRY_DAY))
      finish("waiting")
    }
    fail(sprintf("the archive page for %s could not be read (%s). ", label,
                 phivolcs_month_url(yr, mo)),
         "PHIVOLCS may have moved the page or changed its format.")
  }

  in_month <- old_std$Date >= m & old_std$Date < m %m+% months(1)
  problems <- check_month(d, yr, mo, sum(in_month))
  if (length(problems) > 0) {
    fail(sprintf("the archive page for %s does not look right: ", label),
         paste(problems, collapse = "; "), ". ",
         "PHIVOLCS may have changed its page format; check ", phivolcs_month_url(yr, mo))
  }
  say(sprintf("- %s: %s events (previously %s).", label,
              format(nrow(d), big.mark = ","), format(sum(in_month), big.mark = ",")))
  fresh[[i]] <- d
}

# ---- 4. Combine, clean, check, write -------------------------------------------
# Keep everything before the first refreshed month; replace the refreshed
# months; drop anything after the last complete month (partial months are
# added live in the app with "Add latest events from PHIVOLCS").
kept <- old_std[old_std$Date < from, ]
new_std <- bind_rows(kept, bind_rows(fresh))
new_clean <- clean_eq_data(new_std)
log <- attr(new_clean, "cleaning_log")

unknown_prov <- log$Records[log$Step == "Province could not be identified"]
if (length(unknown_prov) && unknown_prov > 0.02 * nrow(new_clean))
  fail(sprintf("%d events have a province that could not be identified.", unknown_prov))
if (nrow(new_clean) < nrow(old_std) - sum(old_std$Date >= from) )
  fail("the cleaned data has fewer events than expected.")
if (max(new_clean$Date) < last_complete_end - 2)
  fail("the cleaned data ends on ", max(new_clean$Date), ", before ", last_complete_end, ".")

new_export <- export_eq_data(new_clean)
old_export <- export_eq_data(clean_eq_data(old_std))
changed <- !identical(
  select(new_export, -Source), select(old_export, -Source)
)
say(sprintf("New data: %s events, %s to %s.",
            format(nrow(new_clean), big.mark = ","), min(new_clean$Date), max(new_clean$Date)))

if (!changed) {
  say("No change compared with the current data.")
  finish("up-to-date")
}
if (DRY_RUN) {
  say("Dry run: nothing written.")
  finish("dry-run")
}
write_utf8_csv(new_export, OUTPUT_FILE)
say("Wrote ", OUTPUT_FILE, ".")
finish("updated", changed = TRUE)
