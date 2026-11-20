# =============================================================================
# "About & methods" page: documentation, definitions, citations
# =============================================================================

APP_URL <- "https://jllcalorio.shinyapps.io/philippine-quake-viz-shinyapp/"
REPO_URL <- "https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp"
PHIVOLCS_URL <- "https://earthquake.phivolcs.dost.gov.ph/"

APP_PACKAGES <- c("shiny", "bslib", "dplyr", "stringr", "lubridate", "rvest",
                  "ggplot2", "sf", "DT", "scales")

# Builds plain-text citations from the installed packages, so versions and
# references are always correct for the deployed app. Computed once at start-up.
format_citation <- function(pkg = NULL) {
  cit <- tryCatch(if (is.null(pkg)) citation() else citation(pkg), error = function(e) NULL)
  if (is.null(cit)) return(pkg)
  txt <- paste(format(cit[1], style = "text"), collapse = " ")
  txt <- gsub("[*_]", "", txt)          # drop Markdown emphasis
  txt <- gsub("<(https?://[^>]+)>", "\\1", txt)
  # Base R only: files in R/ are sourced before app.R attaches packages.
  trimws(gsub("\\s+", " ", txt))
}
PACKAGE_CITATIONS <- c(
  format_citation(),
  vapply(APP_PACKAGES, function(p) {
    v <- tryCatch(as.character(packageVersion(p)), error = function(e) "?")
    paste0(p, " (version ", v, "): ", format_citation(p))
  }, character(1))
)

simple_table <- function(df) {
  tags$table(
    class = "table table-sm table-striped",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v))))
    }))
  )
}

ext_link <- function(text, href) tags$a(text, href = href, target = "_blank", rel = "noopener")

about_page <- function(cleaning_log, scrape_pages, data_range) {
  log_tbl <- cleaning_log
  log_tbl$Records <- fmt_n(log_tbl$Records)

  tagList(
    h2("About this app"),
    p(
      "The Philippines lies on the Pacific Ring of Fire and records thousands of",
      "earthquakes every year. This app lets you explore every earthquake listed",
      "by the", strong("Philippine Institute of Volcanology and Seismology (PHIVOLCS-DOST)"),
      sprintf("from %s to %s:", format(data_range[1], "%d %B %Y"), format(data_range[2], "%d %B %Y")),
      "where earthquakes occur, how large and how deep they are, how activity changes",
      "over time, and which provinces record the most events."
    ),
    div(
      class = "alert alert-warning",
      strong("Not for emergency use. "),
      "This is an independent educational and research tool. It is not an official",
      "PHIVOLCS product. The data may be incomplete, delayed, or later revised.",
      "For official earthquake information, advisories and tsunami warnings, see",
      ext_link("PHIVOLCS", PHIVOLCS_URL), "."
    ),

    h3("How to use"),
    tags$ol(
      tags$li(strong("Filter"), " with the sidebar: date range (or a quick period), magnitude,",
              "island group and province. Every tab uses the same filters."),
      tags$li(strong("Overview"), " gives headline numbers, the magnitude and depth",
              "distributions, and a summary by year."),
      tags$li(strong("Map"), " shows epicentres. Click a circle to see the event's details.",
              "Use the expand icon in the corner of any chart to view it full screen."),
      tags$li(strong("Trends"), " shows the number of events, magnitude or depth by week,",
              "month, quarter or year."),
      tags$li(strong("Provinces"), " ranks provinces by number of events and gives a sortable summary table."),
      tags$li(strong("Data"), " lists every record and lets you download the filtered or full data as CSV."),
      tags$li(strong("Download charts"), " with the PNG button on each chart. Set the image size",
              "under ", em("Chart download size"), " in the sidebar."),
      tags$li(strong("Get newer data"), " with ", em("Add latest events from PHIVOLCS"),
              ". The bundled data is updated automatically after each month ends;",
              "this button adds the events published since then, in your session only.")
    ),

    h3("Data source"),
    p(
      "All earthquake records come from the PHIVOLCS earthquake information pages",
      "(", ext_link(PHIVOLCS_URL, PHIVOLCS_URL), "), which list the date and time",
      "(Philippine Standard Time, UTC+8), latitude, longitude, depth, magnitude, and",
      "a location relative to a reference town for each event. The bundled data set",
      "was compiled by web scraping the monthly archive pages and was then cleaned",
      "(see", em("Data processing"), "below). It is extended automatically a few days",
      "after each month ends, once PHIVOLCS publishes that month's archive page.",
      "Events from the current month (added with the button) come from the",
      em("latest events"), "page and are preliminary."
    ),
    p("Suggested citation for the data:"),
    tags$blockquote(
      class = "small",
      "Philippine Institute of Volcanology and Seismology (PHIVOLCS-DOST).",
      "Earthquake Information.", ext_link(PHIVOLCS_URL, PHIVOLCS_URL),
      sprintf("(data from %s to %s, accessed through the Philippine Earthquake Explorer).",
              format(data_range[1], "%d %B %Y"), format(data_range[2], "%d %B %Y"))
    ),
    p(
      "The coastline on the map is from", ext_link("Natural Earth", "https://www.naturalearthdata.com/"),
      "(1:10m Admin 0 countries), which is in the public domain."
    ),

    h3("Definitions"),
    h5("Magnitude"),
    p(
      "Magnitude measures the size of an earthquake at its source. It is",
      "logarithmic: each whole step is about 10 times more ground motion and about",
      "32 times more energy released. The app shows magnitude as published by PHIVOLCS.",
      "The PHIVOLCS listings do not state the magnitude type. Values for recent events",
      "are preliminary and may be revised."
    ),
    p("Magnitude classes used in the app (conventional descriptors, e.g. USGS and Michigan Tech UPSeis):"),
    simple_table(data.frame(
      Class = MAG_CLASS_LABELS,
      `Chart colour group` = c("M < 3.0", "M < 3.0", "M 3.0–3.9", "M 4.0–4.9",
                               "M 5.0–5.9", "M ≥ 6.0", "M ≥ 6.0", "M ≥ 6.0"),
      check.names = FALSE
    )),
    div(
      class = "alert alert-info small",
      strong("Magnitude is not intensity. "),
      "The ", ext_link("PHIVOLCS Earthquake Intensity Scale (PEIS)",
                      "https://www.phivolcs.dost.gov.ph/index.php/earthquake/earthquake-intensity-scale"),
      " (I Scarcely Perceptible to X Completely Devastating) describes how strongly",
      "shaking is felt at a particular place. One earthquake has a single magnitude but",
      "different intensities at different places. Earlier versions of this app labelled",
      "magnitude ranges with PEIS intensity names; that labelling was incorrect and has been replaced."
    ),
    h5("Depth"),
    p("Depth is the depth of the hypocentre (where the rupture starts) below the surface. Standard classes (USGS):"),
    simple_table(data.frame(
      Class = DEPTH_CLASS_LABELS,
      Meaning = c("Most earthquakes, and most damaging ones, are shallow.",
                  "Occur mainly within tectonic plates that are sinking (subducting) beneath others.",
                  "Occur only within deeply subducted plates. Usually less damaging at the surface for the same magnitude.")
    )),
    h5("Location, province and island group"),
    p(
      "PHIVOLCS describes each epicentre relative to a reference town, for example",
      em("\"013 km N 37° E of Bayabas (Surigao del Sur)\"."),
      "In this app,", strong("Province"), "is the province of that reference town. For",
      "offshore events this is the nearest reference town's province, not the province",
      "the epicentre lies in.", strong("Island group"),
      "(Luzon, Visayas, Mindanao) follows the usual administrative grouping. Mindoro,",
      "Palawan, Romblon, Marinduque and Masbate are counted under Luzon."
    ),
    h5("Column dictionary (CSV downloads)"),
    simple_table(data.frame(
      Column = c("Date", "Time_PhST", "Latitude_N", "Longitude_E", "Coordinates_valid",
                 "Depth_km", "Depth_class", "Magnitude", "Magnitude_class",
                 "Distance_to_reference_km", "Direction_to_reference", "Reference_town",
                 "Reference_province", "Island_group", "Source"),
      Description = c(
        "Date of the event (YYYY-MM-DD, Philippine Standard Time)",
        "Time of the event, 24-hour clock, Philippine Standard Time (UTC+8). Blank if not recorded",
        "Epicentre latitude, degrees north",
        "Epicentre longitude, degrees east",
        "FALSE if the coordinates failed the plausibility checks (see Data processing)",
        "Hypocentre depth in km. Blank if not reported or implausible",
        "Shallow, Intermediate or Deep (see above)",
        "Magnitude as published by PHIVOLCS",
        "Magnitude class (see above)",
        "Distance from the epicentre to the reference town, km",
        "Bearing from the reference town to the epicentre",
        "Town, city or island PHIVOLCS uses as the reference point",
        "Province of the reference town, after cleaning",
        "Luzon, Visayas or Mindanao",
        "Where the record came from: the original bundled data set, a PHIVOLCS monthly archive page, or the PHIVOLCS website (latest events), with retrieval date"
      )
    )),

    h3("Data processing"),
    p("The same documented steps (in ", code("R/data_prep.R"), ") clean both the bundled data and any events downloaded live:"),
    tags$ul(
      tags$li("Dates, times and text encodings are standardised. Times are kept in Philippine Standard Time."),
      tags$li("Exact duplicate records are removed. These come from repeated rows in the source and from overlap between monthly and latest-event pages."),
      tags$li("Province names are corrected: typing errors (e.g. \"Batanagas\"), renamed",
              "provinces (Compostela Valley became Davao de Oro in 2019; North Cotabato is",
              "officially Cotabato), and cases where an earlier scraper stored a town or old",
              "municipality name as the province. For example, Balut Island and Sarangani",
              "Island events belong to the Municipality of Sarangani, Davao Occidental,",
              "not to Sarangani province."),
      tags$li("Depths recorded with the placeholder 0.00001 km (originally \"--\" or \"<001\" on the",
              "website) and depths deeper than 750 km are set to missing."),
      tags$li("Coordinates outside the Philippine region, or more than 300 km from where the",
              "stated reference town and distance place the event, are flagged. They are kept",
              "in tables and counts but not drawn on the map."),
      tags$li("Magnitude class, depth class and island group are derived as described above.")
    ),
    p("Effect of each step on the data currently loaded:"),
    simple_table(log_tbl),
    if (!is.null(scrape_pages)) tagList(
      p("Pages read in your latest PHIVOLCS update:"),
      simple_table(scrape_pages[, c("Page", "Result")])
    ),

    h3("Limitations and cautions"),
    tags$ul(
      tags$li(strong("Detection changes over time. "),
              "The number of small earthquakes recorded depends on how many seismic",
              "stations are operating. Rising counts of small events may partly reflect",
              "better monitoring, not more earthquakes. Comparisons over time are most",
              "reliable for larger events (for example, M 4.0 and above)."),
      tags$li(strong("Aftershock sequences "), "after a large earthquake produce bursts of events",
              "and can dominate monthly counts and provincial rankings."),
      tags$li(strong("Preliminary values. "), "Magnitudes, depths and locations of recent events",
              "may be revised by PHIVOLCS after further analysis. Downloaded data reflects the",
              "website at the time of retrieval."),
      tags$li(strong("Province attribution "), "follows the reference town, not legal boundaries",
              "(see Definitions)."),
      tags$li(strong("Coverage. "), "Events PHIVOLCS lists only as \"Taiwan Region\" are excluded.",
              "Distant events outside the Philippines are included only when PHIVOLCS",
              "describes them relative to a Philippine reference town."),
      tags$li(strong("Small number of residual errors. "), "The checks catch obvious errors only.",
              "Some misplaced records may remain.")
    ),

    h3("How to cite this app"),
    tags$blockquote(
      class = "small",
      sprintf("Calorio, J. L. L. (%s). Philippine Earthquake Explorer [Shiny web application].",
              format(Sys.Date(), "%Y")),
      ext_link(APP_URL, APP_URL), ". Source code: ", ext_link(REPO_URL, REPO_URL)
    ),
    p("Please also cite PHIVOLCS as the data source (see above)."),

    h3("Software"),
    p("Built with R and the packages below. Citations are generated from the installed versions."),
    tags$ul(class = "small", lapply(PACKAGE_CITATIONS, tags$li)),

    h3("Author and contact"),
    p(strong("John Lennon L. Calorio"), br(),
      "Health Data Scientist and Statistician", br(),
      "Senior Statistician, ",
      ext_link("Center for Research and Development", "https://web.facebook.com/dmsfiCRD"), ", ",
      ext_link("Davao Medical School Foundation, Inc.", "https://web.facebook.com/DavaoMedical"), br(),
      "Email: ", tags$a("jllcalorio@gmail.com", href = "mailto:jllcalorio@gmail.com")),
    p("Report a problem or suggest a feature on the ",
      ext_link("GitHub issue tracker", paste0(REPO_URL, "/issues")), ". Contributions are welcome."),
    p("Thanks to PHIVOLCS for making earthquake information openly available. Stay safe!"),
    p(class = "text-muted small", "License: MIT.")
  )
}
