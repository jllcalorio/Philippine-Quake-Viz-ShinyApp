# =============================================================================
# Data loading, standardisation and cleaning
# -----------------------------------------------------------------------------
# Every record shown in the app (the bundled CSV, plus anything scraped live
# from PHIVOLCS) goes through the same pipeline:
#
#   read_default_data() / scrape_phivolcs()   ->  "standardised" data frame
#   clean_eq_data()                           ->  analysis-ready data frame
#
# The standardised format is one row per event with these columns:
#   Date (Date), Time ("HH:MM", Philippine Time), Latitude, Longitude,
#   Depth_km, Mag, Distance_to_Reference_km, Direction_to_Reference,
#   City_Municipality, Province, Source
#
# All dates and times are Philippine Standard Time (UTC+8), as published.
# Shiny sources this file automatically because it lives in R/.
# =============================================================================

PH_TZ <- "Asia/Manila"

# -----------------------------------------------------------------------------
# Classification schemes
# -----------------------------------------------------------------------------

# Magnitude classes. These are the conventional magnitude descriptors used by
# e.g. the USGS and Michigan Tech's UPSeis (micro < 2, minor 2-3.9, light 4-4.9,
# moderate 5-5.9, strong 6-6.9, major 7-7.9, great 8+). They describe the size
# of the earthquake at its source. They are NOT the PHIVOLCS Earthquake Intensity
# Scale (PEIS), which describes the shaking felt at a given place.
MAG_CLASS_BREAKS <- c(-Inf, 2, 3, 4, 5, 6, 7, 8, Inf)
MAG_CLASS_LABELS <- c(
  "Micro (< 2.0)",
  "Minor (2.0–2.9)",
  "Minor (3.0–3.9)",
  "Light (4.0–4.9)",
  "Moderate (5.0–5.9)",
  "Strong (6.0–6.9)",
  "Major (7.0–7.9)",
  "Great (≥ 8.0)"
)

# A coarser grouping used for colour in charts. More than five ordered colours
# cannot be told apart reliably, so the charts use these five groups.
MAG_GROUP_BREAKS <- c(-Inf, 3, 4, 5, 6, Inf)
MAG_GROUP_LABELS <- c("M < 3.0", "M 3.0–3.9", "M 4.0–4.9",
                      "M 5.0–5.9", "M ≥ 6.0")

# Depth classes (standard seismological convention, e.g. USGS).
DEPTH_CLASS_BREAKS <- c(-Inf, 70, 300, Inf)
DEPTH_CLASS_LABELS <- c("Shallow (< 70 km)", "Intermediate (70–300 km)",
                        "Deep (> 300 km)")

# The deepest earthquakes ever recorded are about 700-750 km deep. Anything
# deeper is treated as a data-entry error.
MAX_PLAUSIBLE_DEPTH_KM <- 750

# A generous box around the region PHIVOLCS reports on. Points outside it are
# almost certainly typing errors (e.g. longitude 12.68 instead of 126.8).
COORD_BOX <- list(lat = c(0, 26), lon = c(112, 135))

classify_magnitude <- function(mag) {
  cut(mag, MAG_CLASS_BREAKS, MAG_CLASS_LABELS, right = FALSE)
}

group_magnitude <- function(mag) {
  cut(mag, MAG_GROUP_BREAKS, MAG_GROUP_LABELS, right = FALSE)
}

classify_depth <- function(depth) {
  cut(depth, DEPTH_CLASS_BREAKS, DEPTH_CLASS_LABELS, right = FALSE)
}

# -----------------------------------------------------------------------------
# Provinces
# -----------------------------------------------------------------------------

# The 82 provinces + Metro Manila, grouped by major island group.
# "Maguindanao" is kept as a separate value: PHIVOLCS used it for events before
# the province was split into Maguindanao del Norte and del Sur (2022).
PROVINCES_BY_ISLAND_GROUP <- list(
  Luzon = c(
    "Abra", "Albay", "Apayao", "Aurora", "Bataan", "Batanes", "Batangas",
    "Benguet", "Bulacan", "Cagayan", "Camarines Norte", "Camarines Sur",
    "Catanduanes", "Cavite", "Ifugao", "Ilocos Norte", "Ilocos Sur", "Isabela",
    "Kalinga", "La Union", "Laguna", "Marinduque", "Masbate", "Metro Manila",
    "Mountain Province", "Nueva Ecija", "Nueva Vizcaya", "Occidental Mindoro",
    "Oriental Mindoro", "Palawan", "Pampanga", "Pangasinan", "Quezon",
    "Quirino", "Rizal", "Romblon", "Sorsogon", "Tarlac", "Zambales"
  ),
  Visayas = c(
    "Aklan", "Antique", "Biliran", "Bohol", "Capiz", "Cebu", "Eastern Samar",
    "Guimaras", "Iloilo", "Leyte", "Negros Occidental", "Negros Oriental",
    "Northern Samar", "Samar", "Siquijor", "Southern Leyte"
  ),
  Mindanao = c(
    "Agusan del Norte", "Agusan del Sur", "Basilan", "Bukidnon", "Camiguin",
    "Cotabato", "Davao de Oro", "Davao del Norte", "Davao del Sur",
    "Davao Occidental", "Davao Oriental", "Dinagat Islands", "Lanao del Norte",
    "Lanao del Sur", "Maguindanao", "Maguindanao del Norte",
    "Maguindanao del Sur", "Misamis Occidental", "Misamis Oriental",
    "Sarangani", "South Cotabato", "Sultan Kudarat", "Sulu",
    "Surigao del Norte", "Surigao del Sur", "Tawi-Tawi",
    "Zamboanga del Norte", "Zamboanga del Sur", "Zamboanga Sibugay"
  )
)
VALID_PROVINCES <- unlist(PROVINCES_BY_ISLAND_GROUP, use.names = FALSE)
ISLAND_GROUP_LOOKUP <- setNames(
  rep(names(PROVINCES_BY_ISLAND_GROUP), lengths(PROVINCES_BY_ISLAND_GROUP)),
  VALID_PROVINCES
)

# Raw value (lower case) -> correct province. Covers three kinds of problems:
#   1. Typos in the PHIVOLCS listings ("Batanagas", "Eastren Samar", ...)
#   2. Old or alternative names ("Compostela Valley" was renamed Davao de Oro in
#      2019; "North Cotabato" is officially "Cotabato"; "Western Samar" is "Samar")
#   3. A town, island or old municipality name that an earlier version of the
#      scraper took from the wrong pair of brackets, e.g.
#      "Fuga Island (Aparri) (Cagayan)" was stored with Province = "Aparri".
PROVINCE_FIXES <- c(
  # -- typos ------------------------------------------------------------------
  "agusan de sur" = "Agusan del Sur", "batanagas" = "Batangas",
  "camarines surl" = "Camarines Sur", "camarinessur" = "Camarines Sur",
  "cataduanes" = "Catanduanes", "davao del occidental" = "Davao Occidental",
  "davao occicental" = "Davao Occidental", "davao occidenta" = "Davao Occidental",
  "davao occidentall" = "Davao Occidental", "davao oreintal" = "Davao Oriental",
  "davao orientall" = "Davao Oriental", "davao orinetal" = "Davao Oriental",
  "easter samar" = "Eastern Samar", "easterm samar" = "Eastern Samar",
  "eastren samar" = "Eastern Samar", "eatern samar" = "Eastern Samar",
  "lanao de sur" = "Lanao del Sur", "lanao del nortel" = "Lanao del Norte",
  "negros occedental" = "Negros Occidental", "northen samar" = "Northern Samar",
  "nuevq ecija" = "Nueva Ecija", "nueva viscaya" = "Nueva Vizcaya",
  "occ.mindoro" = "Occidental Mindoro", "oriemtal mindoro" = "Oriental Mindoro",
  "s.leyte" = "Southern Leyte", "saragani" = "Sarangani",
  "saranggani" = "Sarangani", "south cotobato" = "South Cotabato",
  "sultan kudaratl" = "Sultan Kudarat", "sutan kudarat" = "Sultan Kudarat",
  "surigao de norte" = "Surigao del Norte", "surigao de sur" = "Surigao del Sur",
  "surigao sel sur" = "Surigao del Sur", "zambaoanga del norte" = "Zamboanga del Norte",
  "zambonga del sur" = "Zamboanga del Sur", "suquijor" = "Siquijor",
  "davao de oro" = "Davao de Oro", "dinagat" = "Dinagat Islands",
  "sibugay" = "Zamboanga Sibugay",
  # -- renamed / alternative names ---------------------------------------------
  "compostela valley" = "Davao de Oro", "compostella valley" = "Davao de Oro",
  "north cotabato" = "Cotabato", "western samar" = "Samar",
  "national capital region" = "Metro Manila", "ncr" = "Metro Manila",
  # -- town / island / old-name values (from the bracket bug) ------------------
  "albor" = "Dinagat Islands",            # Libjo (Albor)
  "amduntog" = "Ifugao",                  # Asipulo (Amduntog)
  "anao-aon" = "Surigao del Norte",       # San Francisco (Anao-aon)
  "anao-oan" = "Surigao del Norte",
  "angkaki" = "Ilocos Sur",               # Quirino (Angkaki)
  "aparri" = "Cagayan",                   # Fuga Island (Aparri)
  "balatan" = "Camarines Sur",
  "batangas city" = "Batangas",           # Verde Island
  "binangonan" = "Rizal",                 # Talim Island
  "bitulok & sabani" = "Nueva Ecija",     # Gabaldon (Bitulok & Sabani)
  "bolinao" = "Pangasinan",
  "bugho" = "Leyte",                      # Javier (Bugho)
  "calanasan" = "Apayao",                 # Calanasan (Bayag)
  "calayan" = "Cagayan",                  # Dalupiri/Babuyan/Camiguin Island
  "capital" = "La Union",                 # San Fernando (Capital)
  "city of cavite" = "Cavite",            # Corregidor Island
  "don mariano marcos" = "Zamboanga del Sur",  # Sominot (Don Mariano Marcos)
  "doña alicia" = "Davao de Oro",    # Mabini (Dona Alicia)
  "general luna" = "Surigao del Norte",
  "guiuan" = "Eastern Samar",             # Homonhon Island
  "imelda" = "Camarines Norte",           # San Lorenzo Ruiz (Imelda)
  "island garden city of samal" = "Davao del Norte",
  "island garden city of samal (davao del norte" = "Davao del Norte",
  "lambayong" = "Maguindanao del Sur",    # Sultan sa Barongis (Lambayong)
  "libog" = "Albay",                      # Santo Domingo (Libog)
  "licuan" = "Abra",                      # Licuan-Baay (Licuan)
  "limbuhan" = "Masbate",                 # Pio V. Corpuz (Limbuhan)
  "linugos" = "Misamis Oriental",         # Magsaysay (Linugos)
  "liwan" = "Kalinga",                    # Rizal (Liwan)
  "luzurriaga" = "Negros Oriental",       # Valencia (Luzurriaga)
  "magallon" = "Negros Occidental",       # Moises Padilla (Magallon)
  "maguing" = "Lanao del Sur",            # Lumba-Bayabao (Maguing)
  "mariano marcos" = "Sultan Kudarat",    # Lambayong (Mariano Marcos)
  "masbate (ticao island" = "Masbate",
  "masinloc" = "Zambales",                # Bajo de Masinloc
  "mauban" = "Quezon",                    # Cagbalete Island
  "municipality of sarangani" = "Davao Occidental",  # Balut / Sarangani Island
  "nuling" = "Maguindanao del Norte",     # Sultan Kudarat (Nuling)
  "opon" = "Cebu",                        # Lapu-Lapu City (Opon)
  "paranas" = "Samar",                    # Wright (Paranas)
  "parang" = "Maguindanao del Norte",     # Bongo Island
  "parubcan" = "Camarines Sur",           # Presentacion (Parubcan)
  "payabon" = "Negros Oriental",          # Bindoy (Payabon)
  "payo" = "Catanduanes",                 # Panganiban (Payo)
  "polillo" = "Quezon",                   # Balesin Island
  "ponot" = "Zamboanga del Norte",        # Jose Dalman (Ponot)
  "potia" = "Ifugao",                     # Alfonso Lista (Potia)
  "salcedo" = "Ilocos Sur",               # Salcedo (Baugen)
  "san andres" = "Catanduanes",           # San Andres (Calolbon)
  "san isidro" = "Surigao del Norte",
  "san mariano" = "Davao de Oro",         # Maragusan (San Mariano)
  "san nicolas / talisay" = "Batangas",   # Taal Volcano Island
  "talisay" = "Batangas",                 # Taal Volcano Island
  "san vicente" = "Davao de Oro",         # Laak (San Vicente)
  "santo domingo" = "Albay",              # Libog (Santo Domingo)
  "sapao" = "Surigao del Norte",          # Santa Monica (Sapao)
  "saug" = "Davao del Norte",             # Asuncion (Saug)
  "shariff aguak" = "Maguindanao del Sur",# Shariff Aguak (Maganoy)
  "sugbay" = "Zamboanga del Sur",         # Pitogo (Sugbay)
  "ticao island" = "Masbate",
  "tongkil" = "Sulu",                     # Banguingui (Tongkil)
  "tulong" = "Negros Oriental",           # Bayawan (Tulong)
  "valencia" = "Negros Oriental",         # Luzurriaga (Valencia)
  "vinzons" = "Camarines Norte",          # Tinaga Island
  "watu" = "Lanao del Sur"                # Balindong (Watu)
)

# Cases where the correct province depends on the town as well.
fix_province_by_town <- function(province, town) {
  town_lc <- tolower(town)
  case_when(
    # Balut Island and Sarangani Island belong to the *Municipality* of
    # Sarangani, Davao Occidental -- not to Sarangani province.
    province %in% c("Sarangani", "Davao Occidental") &
      town_lc %in% c("balut island", "sarangani island", "sarangani") ~ "Davao Occidental",
    # Old municipality names that happen to match a province name:
    # "San Francisco (Aurora) (Quezon)" and "Basilisa (Rizal) (Dinagat Islands)".
    province == "Aurora" & town_lc == "san francisco" ~ "Quezon",
    province == "Rizal" & town_lc == "basilisa" ~ "Dinagat Islands",
    province == "Mindoro" & town_lc == "puerto galera" ~ "Oriental Mindoro",
    province == "Mindoro" ~ "Occidental Mindoro",          # Paluan
    province == "San Juan" & town_lc == "cabalian" ~ "Southern Leyte",
    province == "San Juan" ~ "Ilocos Sur",                 # Lapog / Lopog
    is.na(province) & town_lc == "licuan" ~ "Abra",
    is.na(province) & town_lc == "city of davao" ~ "Davao del Sur",
    is.na(province) & town_lc == "city of cagayan de oro" ~ "Misamis Oriental",
    is.na(province) & town_lc == "city of general santos" ~ "South Cotabato",
    is.na(province) & town_lc == "city of zamboanga" ~ "Zamboanga del Sur",
    TRUE ~ province
  )
}

standardise_province <- function(province, town) {
  p <- str_squish(province)
  p_lc <- tolower(p)

  # 1. Case-insensitive match to an official name ("Surigao Del Sur" -> "Surigao del Sur")
  official <- VALID_PROVINCES[match(p_lc, tolower(VALID_PROVINCES))]
  # 2. Known typo / old name / town-name fixes
  fixed <- unname(PROVINCE_FIXES[p_lc])
  p <- coalesce(official, fixed, p)
  # 3. Town-dependent fixes
  fix_province_by_town(p, town)
}

# -----------------------------------------------------------------------------
# Parsing helpers (locale-independent, so they behave the same on Windows,
# macOS and the Linux server at shinyapps.io)
# -----------------------------------------------------------------------------

# Parses "27-Jun-26", "27 June 2026", "1 Jan 2017" etc. into a Date.
parse_day_month_year <- function(x) {
  m <- str_match(str_squish(x), "^(\\d{1,2})[- /]([A-Za-z]+)[- /](\\d{2,4})")
  month <- match(tolower(substr(m[, 3], 1, 3)), tolower(month.abb))
  year <- as.integer(m[, 4])
  year <- ifelse(!is.na(year) & year < 100, 2000L + year, year)
  as.Date(sprintf("%04d-%02d-%02d", year, month, as.integer(m[, 2])),
          format = "%Y-%m-%d")
}

# Normalises "3:20" -> "03:20" and "11:46 PM" -> "23:46".
parse_time_hhmm <- function(x) {
  m <- str_match(str_squish(x), "^(\\d{1,2}):(\\d{2})(?:\\s*([AaPp])[Mm])?")
  hour <- as.integer(m[, 2])
  minute <- as.integer(m[, 3])
  ampm <- toupper(m[, 4])
  hour <- ifelse(!is.na(ampm), hour %% 12L + ifelse(ampm == "P", 12L, 0L), hour)
  ifelse(is.na(hour) | is.na(minute) | hour > 23 | minute > 59, NA_character_,
         sprintf("%02d:%02d", hour, minute))
}

# Text columns in the bundled CSV mix UTF-8 and Latin-1 bytes (the degree sign
# in "N 37° E"). Convert everything to valid UTF-8.
fix_encoding <- function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  x
}

# Splits a PHIVOLCS location string such as
#   "021 km N 82° W of Balut Island (Municipality Of Sarangani) (Davao Occidental)"
# into distance, direction, reference town and province. The province is always
# the LAST bracketed name.
parse_location <- function(location) {
  loc <- str_squish(location)
  distance <- as.numeric(str_match(loc, "^(\\d+)\\s*km")[, 2])
  direction <- str_match(loc, "^\\d+\\s*km\\s+(.+?)\\s+of\\s+")[, 2]
  place <- str_replace(loc, "^\\d+\\s*km\\s+.+?\\s+of\\s+", "")
  province <- coalesce(
    str_match(place, "\\(([^()]*)\\)\\s*$")[, 2],   # "... (Province)"
    str_match(place, "\\(([^()]*)$")[, 2]           # "... (Province"  (unclosed)
  )
  town <- str_trim(str_replace(place, "\\s*\\(.*$", ""))
  tibble(
    Distance_to_Reference_km = distance,
    Direction_to_Reference = direction,
    City_Municipality = town,
    Province = str_trim(province)
  )
}

# Great-circle distance in km.
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

# -----------------------------------------------------------------------------
# Reading the bundled data set
# -----------------------------------------------------------------------------

#' Read the bundled CSV into the standardised format.
#'
#' Accepts either layout:
#'   * "raw"    - the original scraper output (Date like "27-Jun-26", Mag,
#'                City_Municipality, Province, ...), or
#'   * "export" - a CSV downloaded from the app (Date "YYYY-MM-DD", Magnitude,
#'                Reference_town, Reference_province, ...; see export_eq_data()).
#' So a file downloaded with "All data (CSV)" can replace default_data.csv as is.
read_default_data <- function(path = "default_data.csv") {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), colClasses = "character")
  # Strip a UTF-8 byte-order mark from the first header (app downloads add one
  # so Excel opens them correctly), and drop empty trailing columns.
  names(raw) <- trimws(sub("^(\ufeff|\u00ef\u00bb\u00bf)", "", names(raw)))
  keep <- nzchar(names(raw)) & !grepl("^X(\\.\\d+)?$|^V\\d+$", names(raw))
  raw <- raw[, keep, drop = FALSE]
  raw[] <- lapply(raw, fix_encoding)

  layouts <- list(
    raw = c(Date = "Date", Time = "Time", Latitude = "Latitude",
            Longitude = "Longitude", Depth_km = "Depth_km", Mag = "Mag",
            Distance_to_Reference_km = "Distance_to_Reference_km",
            Direction_to_Reference = "Direction_to_Reference",
            City_Municipality = "City_Municipality", Province = "Province"),
    export = c(Date = "Date", Time = "Time_PhST", Latitude = "Latitude_N",
               Longitude = "Longitude_E", Depth_km = "Depth_km", Mag = "Magnitude",
               Distance_to_Reference_km = "Distance_to_reference_km",
               Direction_to_Reference = "Direction_to_reference",
               City_Municipality = "Reference_town", Province = "Reference_province")
  )
  fits <- vapply(layouts, function(cols) all(cols %in% names(raw)), logical(1))
  if (!any(fits)) {
    stop(sprintf(paste0(
      "'%s' does not have the expected columns. It must be either the original ",
      "scraper layout (Date, Time, Latitude, Longitude, Depth_km, Mag, ...) or a ",
      "CSV downloaded from the app (Date, Time_PhST, Latitude_N, Longitude_E, ",
      "Depth_km, Magnitude, ...). Columns found: %s"),
      path, paste(names(raw), collapse = ", ")), call. = FALSE)
  }
  cols <- layouts[[which(fits)[1]]]
  get <- function(std) raw[[cols[[std]]]]
  num <- function(std) suppressWarnings(as.numeric(get(std)))

  # Dates: ISO "2026-06-27" (export) or "27-Jun-26" (raw); try both per value.
  date_iso <- as.Date(str_extract(str_squish(get("Date")), "^\\d{4}-\\d{2}-\\d{2}"),
                      format = "%Y-%m-%d")
  dates <- coalesce(date_iso, parse_day_month_year(get("Date")))

  source <- if ("Source" %in% names(raw)) {
    coalesce(raw$Source, "Bundled data set")
  } else {
    "Bundled data set"
  }

  tibble(
    Date = dates,
    Time = parse_time_hhmm(get("Time")),
    Latitude = num("Latitude"),
    Longitude = num("Longitude"),
    Depth_km = num("Depth_km"),
    Mag = num("Mag"),
    Distance_to_Reference_km = num("Distance_to_Reference_km"),
    Direction_to_Reference = get("Direction_to_Reference"),
    City_Municipality = get("City_Municipality"),
    Province = get("Province"),
    Source = source
  )
}

# -----------------------------------------------------------------------------
# Cleaning
# -----------------------------------------------------------------------------

#' Clean a standardised earthquake data frame.
#'
#' Returns the cleaned data with a "cleaning_log" attribute: a data frame with
#' one row per cleaning step and the number of rows it affected.
clean_eq_data <- function(df) {
  log <- list()
  add_log <- function(step, n, action) {
    log[[length(log) + 1]] <<- data.frame(Step = step, Records = n, Action = action)
  }
  n_in <- nrow(df)

  df <- df |>
    mutate(
      Direction_to_Reference = str_squish(str_replace_all(
        fix_encoding(Direction_to_Reference), "[º˚]", "°")),
      City_Municipality = str_squish(fix_encoding(City_Municipality)),
      Province = fix_encoding(Province)
    )

  # 1. Records without a usable date or magnitude cannot be analysed.
  bad <- is.na(df$Date) | is.na(df$Mag) | df$Mag < 0
  add_log("Missing or invalid date or magnitude", sum(bad), "Removed")
  df <- df[!bad, ]
  add_log("Time of day not recorded", sum(is.na(df$Time)),
          "Kept (excluded only from time-of-day sorting)")

  df <- df |>
    mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M", tz = PH_TZ))

  # 2. Exact duplicates (same time, place, depth and magnitude). These occur in
  #    the source listings and where monthly archives overlap the latest page.
  dup <- duplicated(df[, c("Date", "Time", "Latitude", "Longitude", "Depth_km", "Mag")])
  add_log("Duplicate records", sum(dup), "Removed")
  df <- df[!dup, ]

  # 3. Province names
  before <- df$Province
  df$Province <- standardise_province(df$Province, df$City_Municipality)
  add_log("Province name corrected (typo, renamed province, or town recorded as province)",
          sum(coalesce(before != df$Province, is.na(before) != is.na(df$Province))),
          "Corrected")
  unknown <- !is.na(df$Province) & !(df$Province %in% VALID_PROVINCES)
  add_log("Province could not be identified", sum(unknown) + sum(is.na(df$Province)),
          "Set to missing")
  df$Province[unknown] <- NA_character_

  # 4. Depth
  #    Older scrapes stored "--" (not reported) and "<001" (less than 1 km) as
  #    0.00001 km; they cannot be told apart, so both become missing.
  sentinel <- !is.na(df$Depth_km) & df$Depth_km > 0 & df$Depth_km < 0.001
  too_deep <- !is.na(df$Depth_km) & df$Depth_km > MAX_PLAUSIBLE_DEPTH_KM
  add_log("Depth not reported (placeholder value 0.00001 km)", sum(sentinel), "Set to missing")
  add_log(sprintf("Depth deeper than %d km (physically implausible)", MAX_PLAUSIBLE_DEPTH_KM),
          sum(too_deep), "Set to missing")
  df$Depth_km[sentinel | too_deep] <- NA_real_

  # 5. Coordinates
  #    a) outside a generous box around the Philippine region, or
  #    b) much farther from the province's other events than the stated distance
  #       to the reference town allows (catches swapped digits like 132.18 for 123.18).
  in_box <- !is.na(df$Latitude) & !is.na(df$Longitude) &
    between(df$Latitude, COORD_BOX$lat[1], COORD_BOX$lat[2]) &
    between(df$Longitude, COORD_BOX$lon[1], COORD_BOX$lon[2])
  centres <- df[in_box & !is.na(df$Province), ] |>
    group_by(Province) |>
    summarise(c_lat = median(Latitude), c_lon = median(Longitude), .groups = "drop")
  idx <- match(df$Province, centres$Province)
  dist_from_centre <- haversine_km(df$Latitude, df$Longitude,
                                   centres$c_lat[idx], centres$c_lon[idx])
  far <- in_box & !is.na(dist_from_centre) &
    dist_from_centre > coalesce(df$Distance_to_Reference_km, 0) + 300
  df$Coord_valid <- in_box & !far
  add_log("Coordinates outside the Philippine region", sum(!in_box),
          "Kept, but excluded from the map")
  add_log("Coordinates inconsistent with the stated location (> 300 km off)", sum(far),
          "Kept, but excluded from the map")

  df <- df |>
    mutate(
      Year = as.integer(format(Date, "%Y")),
      Month = factor(month.name[as.integer(format(Date, "%m"))], levels = month.name),
      Mag_class = classify_magnitude(Mag),
      Mag_group = group_magnitude(Mag),
      Depth_class = classify_depth(Depth_km),
      Island_group = factor(unname(ISLAND_GROUP_LOOKUP[Province]),
                            levels = names(PROVINCES_BY_ISLAND_GROUP))
    ) |>
    arrange(desc(Date), desc(DateTime)) |>
    select(DateTime, Date, Year, Month, Time, Latitude, Longitude, Coord_valid,
           Depth_km, Depth_class, Mag, Mag_class, Mag_group,
           Distance_to_Reference_km, Direction_to_Reference, City_Municipality,
           Province, Island_group, Source)

  log_df <- do.call(rbind, log)
  log_df <- rbind(
    data.frame(Step = "Records read", Records = n_in, Action = ""),
    log_df,
    data.frame(Step = "Records available for analysis", Records = nrow(df), Action = "")
  )
  attr(df, "cleaning_log") <- log_df
  df
}

#' Data in a tidy, documented shape for CSV export.
export_eq_data <- function(df) {
  df |>
    transmute(
      Date = format(Date, "%Y-%m-%d"),
      Time_PhST = Time,
      Latitude_N = Latitude,
      Longitude_E = Longitude,
      Coordinates_valid = Coord_valid,
      Depth_km,
      Depth_class = as.character(Depth_class),
      Magnitude = Mag,
      Magnitude_class = as.character(Mag_class),
      Distance_to_reference_km = Distance_to_Reference_km,
      Direction_to_reference = Direction_to_Reference,
      Reference_town = City_Municipality,
      Reference_province = Province,
      Island_group = as.character(Island_group),
      Source
    )
}

#' Write a data frame as UTF-8 CSV with a byte-order mark, so Excel shows
#' "°" and "ñ" correctly. read_default_data() reads these files back.
write_utf8_csv <- function(df, file) {
  con <- file(file, open = "wb", encoding = "UTF-8")
  on.exit(close(con))
  writeChar("\ufeff", con, eos = NULL)
  write.csv(df, con, row.names = FALSE, na = "")
}
