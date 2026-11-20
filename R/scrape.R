# =============================================================================
# Live update from the PHIVOLCS website
# -----------------------------------------------------------------------------
# PHIVOLCS publishes earthquake information at
#   https://earthquake.phivolcs.dost.gov.ph/                       (latest events)
#   https://earthquake.phivolcs.dost.gov.ph/EQLatest-Monthly/YYYY/YYYY_Month.html
#
# scrape_phivolcs() downloads every monthly page from the month of `since` up to
# the current month, plus the main page, and returns them in the standardised
# format used by clean_eq_data() (see R/data_prep.R).
# =============================================================================

PHIVOLCS_BASE_URL <- "https://earthquake.phivolcs.dost.gov.ph/"

phivolcs_month_url <- function(year, month) {
  sprintf("%sEQLatest-Monthly/%d/%d_%s.html", PHIVOLCS_BASE_URL, year, year, month.name[month])
}

# Reads the earthquake table from one PHIVOLCS page. Returns a data frame with
# the six raw columns, or NULL if the page could not be read.
read_phivolcs_table <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)

  tables <- html_table(page, fill = TRUE)
  if (length(tables) == 0) return(NULL)
  tbl <- as.data.frame(tables[[which.max(vapply(tables, nrow, integer(1)))]])

  raw_cols <- c("DateTime_raw", "Latitude", "Longitude", "Depth_raw", "Mag", "Location")
  # Some older archive pages lay the table out as two six-column blocks side by side.
  blocks <- if (ncol(tbl) >= 12) list(tbl[, 1:6], tbl[, 7:12]) else if (ncol(tbl) >= 6) list(tbl[, 1:6]) else list()
  if (length(blocks) == 0) return(NULL)
  bind_rows(lapply(blocks, function(b) {
    b[] <- lapply(b, function(col) str_squish(as.character(col)))
    setNames(b, raw_cols)
  }))
}

# Converts raw PHIVOLCS rows into the standardised format.
standardise_phivolcs <- function(raw, source = NULL) {
  if (is.null(source)) source <- paste0("PHIVOLCS website (retrieved ", format(Sys.Date()), ")")
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  # "31 October 2026 - 11:46 PM"
  raw <- raw |>
    filter(str_detect(DateTime_raw, "^\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}"),
           # The bundled data set excludes events PHIVOLCS lists only as
           # "Taiwan Region"; do the same here for consistency.
           !str_detect(Location, regex("^Taiwan Region$", ignore_case = TRUE)))
  if (nrow(raw) == 0) return(NULL)

  depth_txt <- str_replace_all(raw$Depth_raw, "\\s", "")
  depth <- suppressWarnings(as.numeric(depth_txt))
  depth[depth_txt %in% c("<001", "<1")] <- 0   # "less than 1 km"
  # "--" (not reported) stays NA

  loc <- parse_location(raw$Location)
  tibble(
    Date = parse_day_month_year(raw$DateTime_raw),
    Time = parse_time_hhmm(str_extract(raw$DateTime_raw, "\\d{1,2}:\\d{2}\\s*[APap][Mm]")),
    Latitude = suppressWarnings(as.numeric(raw$Latitude)),
    Longitude = suppressWarnings(as.numeric(raw$Longitude)),
    Depth_km = depth,
    Mag = suppressWarnings(as.numeric(raw$Mag)),
    Source = source
  ) |>
    bind_cols(loc)
}

#' Download PHIVOLCS events that occurred after `since`.
#'
#' @param since POSIXct (Philippine Time). Only events after this are returned.
#' @param progress optional function(value, detail) used to report progress.
#' @return list(data = standardised data frame or NULL, pages = page status table)
scrape_phivolcs <- function(since, progress = NULL) {
  now <- as.POSIXlt(Sys.time(), tz = PH_TZ)
  start <- as.POSIXlt(since, tz = PH_TZ)
  first <- (start$year + 1900) * 12 + start$mon
  last <- (now$year + 1900) * 12 + now$mon
  months <- seq(first, last)

  urls <- c(vapply(months, function(m) phivolcs_month_url(m %/% 12, m %% 12 + 1), ""),
            PHIVOLCS_BASE_URL)
  labels <- c(vapply(months, function(m) paste(month.name[m %% 12 + 1], m %/% 12), ""),
              "Latest events page")

  results <- vector("list", length(urls))
  status <- character(length(urls))
  for (i in seq_along(urls)) {
    if (!is.null(progress)) progress(i / length(urls), labels[i])
    results[[i]] <- standardise_phivolcs(read_phivolcs_table(urls[i]))
    status[i] <- if (is.null(results[[i]])) "Not available" else sprintf("%d records", nrow(results[[i]]))
  }

  data <- bind_rows(results)
  if (nrow(data) > 0) {
    data_dt <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M", tz = PH_TZ)
    data <- data[!is.na(data_dt) & data_dt > since, ]
  }
  list(
    data = if (nrow(data) > 0) data else NULL,
    pages = data.frame(Page = labels, URL = urls, Result = status)
  )
}

#' Download one complete month from the PHIVOLCS monthly archive.
#'
#' Used by scripts/refresh_data.R. Archive pages are PHIVOLCS's consolidated
#' monthly listing, so they are preferred over the "latest events" page, whose
#' values are preliminary.
#' @return standardised data frame (only events dated in that month), or NULL
#'   if the page is not available or has no recognisable earthquake table.
scrape_month_archive <- function(year, month) {
  src <- sprintf("PHIVOLCS monthly archive, %s %d (retrieved %s)",
                 month.name[month], year, format(Sys.Date()))
  d <- standardise_phivolcs(read_phivolcs_table(phivolcs_month_url(year, month)), source = src)
  if (is.null(d)) return(NULL)
  d[!is.na(d$Date) & format(d$Date, "%Y-%m") == sprintf("%04d-%02d", year, month), ]
}
