# =============================================================================
# Philippine Earthquake Explorer
# -----------------------------------------------------------------------------
# A Shiny app for exploring earthquakes recorded by the Philippine Institute of
# Volcanology and Seismology (PHIVOLCS-DOST) from January 2017 onwards.
#
# Author : John Lennon L. Calorio
# Source : https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp
# License: MIT
#
# Project layout
#   app.R             user interface and server logic (this file)
#   R/data_prep.R     reading, cleaning and classifying the data
#   R/scrape.R        downloading the latest events from the PHIVOLCS website
#   R/plots.R         charts and summary tables
#   default_data.csv  bundled earthquake catalogue (2017 to mid-2026)
#   ph_map.rds        Philippine coastline (Natural Earth, 1:10m), sf object
#
# Shiny loads every file in R/ automatically before running this file.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(lubridate)
library(rvest)
library(ggplot2)
library(sf)
library(DT)

# Links (APP_URL, REPO_URL, PHIVOLCS_URL) are defined in R/about.R.

# -----------------------------------------------------------------------------
# Load data once per R process (shared by all user sessions)
# -----------------------------------------------------------------------------
DEFAULT_STD <- read_default_data("default_data.csv")
DEFAULT_DATA <- clean_eq_data(DEFAULT_STD)
PH_OUTLINE <- sf_to_polygon_df(readRDS("ph_map.rds"))

MAG_MAX <- ceiling(max(DEFAULT_DATA$Mag) * 10) / 10 + 0.1
DEFAULT_MIN_MAG <- 1

# -----------------------------------------------------------------------------
# Small UI helpers
# -----------------------------------------------------------------------------
plot_card <- function(title, output_id, download_id, height = "560px", controls = NULL,
                      click = NULL, footer = NULL) {
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      title,
      downloadButton(download_id, "PNG", class = "btn-sm btn-outline-secondary",
                     title = "Download this chart as a PNG image (size set in the sidebar)")
    ),
    controls,
    plotOutput(output_id, height = height, click = click),
    if (!is.null(footer)) card_footer(footer)
  )
}

help_text <- function(...) p(class = "text-muted small mb-2", ...)

# -----------------------------------------------------------------------------
# User interface
# -----------------------------------------------------------------------------
ui <- page_navbar(
  title = "Philippine Earthquake Explorer",
  id = "nav",
  window_title = "Philippine Earthquake Explorer",
  theme = bs_theme(version = 5, primary = "#2a78d6", "navbar-bg" = "#0d366b"),
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    .value-box-title { font-size: .85rem; }
    .bslib-value-box .value-box-value { font-size: 1.6rem; }
    .about h3 { margin-top: 1.5rem; }
    .about li { margin-bottom: .25rem; }
    .about table { font-size: .9rem; }
    /* Dropdown filters in table headers: wide enough for province and source names */
    table.dataTable thead .selectize-dropdown { width: auto !important; min-width: 16rem !important; }
    table.dataTable thead .selectize-dropdown .option { white-space: nowrap; }
    /* Open the menus of the last columns to the left so they stay inside the table */
    table.dataTable thead td:nth-last-child(-n+4) .selectize-dropdown { left: auto !important; right: 0; }
  "))),

  sidebar = sidebar(
    width = 320,
    title = "Filters",
    help_text("Filters apply to every tab. Charts, tables and downloads update automatically."),

    dateRangeInput("date_range", "Date range (Philippine Time)",
                   start = min(DEFAULT_DATA$Date), end = max(DEFAULT_DATA$Date),
                   min = min(DEFAULT_DATA$Date), max = max(DEFAULT_DATA$Date),
                   format = "d M yyyy", separator = "to"),
    selectInput("quick_period", NULL,
                choices = c("Quick select a period..." = "",
                            "Last 30 days of data" = "30d",
                            "Last 12 months of data" = "12m",
                            setNames(rev(as.character(sort(unique(DEFAULT_DATA$Year)))),
                                     paste("Year", rev(sort(unique(DEFAULT_DATA$Year))))),
                            "All years" = "all")),

    sliderInput("mag_range", "Magnitude", min = 1, max = MAG_MAX,
                value = c(DEFAULT_MIN_MAG, MAG_MAX), step = 0.1),

    checkboxGroupInput("island_group", "Island group",
                       choices = names(PROVINCES_BY_ISLAND_GROUP),
                       selected = names(PROVINCES_BY_ISLAND_GROUP), inline = TRUE),

    selectizeInput("province_filter", "Province(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All provinces (type to search)",
                                  plugins = list("remove_button"))),

    actionButton("reset_filters", "Reset all filters", icon = icon("rotate-left"),
                 class = "btn-outline-secondary btn-sm"),

    hr(),
    h6("Data"),
    uiOutput("data_status"),
    actionButton("update_data", "Add latest events from PHIVOLCS",
                 icon = icon("cloud-arrow-down"), class = "btn-primary btn-sm"),
    help_text("The data is updated automatically after each month ends. This button",
              "adds the events published since then (usually under 10 seconds).",
              "Only your session is updated."),

    accordion(
      open = FALSE,
      accordion_panel(
        "Chart download size", icon = icon("image"),
        numericInput("plot_width", "Width (inches)", value = 11, min = 3, max = 30, step = 0.5),
        numericInput("plot_height", "Height (inches)", value = 7, min = 3, max = 30, step = 0.5),
        numericInput("plot_dpi", "Resolution (dots per inch)", value = 300, min = 72, max = 600, step = 50)
      )
    )
  ),

  # --- Overview -----------------------------------------------------------------
  nav_panel(
    "Overview", icon = icon("chart-simple"),
    help_text(
      "A summary of the earthquakes that match the filters on the left. All magnitudes,",
      "depths and locations are as published by PHIVOLCS. See", strong("About & methods"),
      "for definitions, data cleaning and limitations."
    ),
    uiOutput("value_boxes"),
    plot_card("Magnitude distribution", "mag_dist_plot", "dl_mag_dist", height = "460px"),
    plot_card("Depth distribution", "depth_dist_plot", "dl_depth_dist", height = "520px"),
    card(
      card_header("Summary by year"),
      help_text("The current year may be incomplete. Median depth excludes events with no reported depth."),
      DTOutput("year_table", fill = FALSE)
    )
  ),

  # --- Map ----------------------------------------------------------------------
  nav_panel(
    "Map", icon = icon("map-location-dot"),
    layout_columns(
      col_widths = c(8, 4),
      plot_card(
        "Epicentre map", "map_plot", "dl_map", height = "780px",
        click = "map_click",
        controls = card_body(
          class = "py-2",
          radioButtons("map_color", "Colour circles by", inline = TRUE,
                       choices = c("Magnitude" = "magnitude", "Depth" = "depth"))
        )
      ),
      card(
        card_header("Event details"),
        help_text("Click a circle on the map to see the event(s) at that spot."),
        uiOutput("map_click_info")
      )
    )
  ),

  # --- Trends -------------------------------------------------------------------
  nav_panel(
    "Trends", icon = icon("chart-line"),
    plot_card(
      "Change over time", "trend_plot", "dl_trend", height = "620px",
      controls = card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(4, 4, 4),
          radioButtons("trend_var", "Show",
                       choices = c("Number of events" = "count",
                                   "Magnitude" = "Mag",
                                   "Depth" = "Depth_km")),
          radioButtons("trend_unit", "Group by", choices = TIME_UNITS, selected = "month"),
          conditionalPanel(
            "input.trend_var == 'count'",
            checkboxInput("trend_separate", "One panel per magnitude group (each with its own scale)",
                          value = TRUE)
          )
        )
      )
    )
  ),

  # --- Provinces ----------------------------------------------------------------
  nav_panel(
    "Provinces", icon = icon("location-dot"),
    plot_card(
      "Earthquakes by province", "province_plot", "dl_province", height = "640px",
      controls = card_body(
        class = "py-2",
        sliderInput("top_n", "Number of provinces to show", min = 5, max = 85, value = 20,
                    step = 1, width = "320px")
      )
    ),
    card(
      card_header("Summary by province"),
      help_text(
        "Province is the province of the reference town PHIVOLCS names in the event's",
        "location (for example, \"13 km N 37° E of Bayabas (Surigao del Sur)\").",
        "Offshore events count toward the province of the nearest reference town.",
        "\"Share of events\" is the share of all events in the current selection."
      ),
      div(style = "overflow-x: auto;", DTOutput("province_table", fill = FALSE))
    )
  ),

  # --- Data ---------------------------------------------------------------------
  nav_panel(
    "Data", icon = icon("table"),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        "Earthquake records",
        div(
          downloadButton("dl_filtered", "Filtered data (CSV)", class = "btn-sm btn-primary"),
          downloadButton("dl_all", "All data (CSV)", class = "btn-sm btn-outline-secondary")
        )
      ),
      help_text(
        "One row per earthquake, newest first. Use the search box or the boxes above",
        "each column to search within the current selection. \"Filtered data\" downloads",
        "exactly the rows that match the sidebar filters. \"All data\" downloads the full,",
        "cleaned data set, including any events you added from PHIVOLCS. Column",
        "definitions are in About & methods."
      ),
      div(style = "overflow-x: auto;", DTOutput("data_table", fill = FALSE))
    )
  ),

  # --- About --------------------------------------------------------------------
  nav_panel(
    "About & methods", icon = icon("circle-info"),
    div(class = "about container-fluid", style = "max-width: 960px;", uiOutput("about"))
  ),

  nav_spacer(),
  nav_item(tags$a(icon("github"), "Source code", href = REPO_URL, target = "_blank"))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Data state (per session) ------------------------------------------------
  rv <- reactiveValues(
    std = DEFAULT_STD,        # standardised (uncleaned) records
    data = DEFAULT_DATA,      # cleaned records used everywhere
    n_added = 0,
    scrape_pages = NULL
  )

  # Fill the province list from the data.
  observe({
    provinces <- sort(unique(na.omit(rv$data$Province)))
    updateSelectizeInput(session, "province_filter", choices = provinces,
                         selected = intersect(isolate(input$province_filter), provinces),
                         server = FALSE)
  })

  # ---- Filters -----------------------------------------------------------------
  data_dates <- reactive(range(rv$data$Date))

  observeEvent(input$quick_period, {
    req(nzchar(input$quick_period))
    last <- data_dates()[2]
    range <- switch(
      input$quick_period,
      "30d" = c(last - 29, last),
      "12m" = c(last %m-% months(12) + 1, last),
      "all" = data_dates(),
      {
        yr <- as.integer(input$quick_period)
        c(max(as.Date(sprintf("%d-01-01", yr)), data_dates()[1]),
          min(as.Date(sprintf("%d-12-31", yr)), last))
      }
    )
    updateDateRangeInput(session, "date_range", start = range[1], end = range[2])
    updateSelectInput(session, "quick_period", selected = "")
  })

  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range", start = data_dates()[1], end = data_dates()[2])
    updateSliderInput(session, "mag_range", value = c(DEFAULT_MIN_MAG, MAG_MAX))
    updateCheckboxGroupInput(session, "island_group", selected = names(PROVINCES_BY_ISLAND_GROUP))
    updateSelectizeInput(session, "province_filter", selected = character(0))
  })

  filtered <- reactive({
    dr <- input$date_range
    validate(
      need(length(dr) == 2 && !anyNA(dr), "Please choose a start and an end date."),
      need(dr[1] <= dr[2], "The start date must be on or before the end date."),
      need(length(input$island_group) > 0, "Select at least one island group.")
    )
    d <- rv$data |>
      filter(Date >= dr[1], Date <= dr[2],
             Mag >= input$mag_range[1] - 1e-9, Mag <= input$mag_range[2] + 1e-9)
    # Only filter by island group if not all are ticked, so events whose
    # province could not be identified are kept by default.
    if (length(input$island_group) < length(PROVINCES_BY_ISLAND_GROUP)) {
      d <- d |> filter(Island_group %in% input$island_group)
    }
    if (length(input$province_filter) > 0) {
      d <- d |> filter(Province %in% input$province_filter)
    }
    d
  })

  # A key that changes whenever the filtered data changes (used for caching).
  filter_key <- reactive(list(input$date_range, input$mag_range, input$island_group,
                              input$province_filter, rv$n_added))

  # ---- Data status and live update ---------------------------------------------
  output$data_status <- renderUI({
    d <- rv$data
    p(class = "small mb-2",
      sprintf("%s events, %s to %s.", fmt_n(nrow(d)),
              format(min(d$Date), "%d %b %Y"), format(max(d$Date), "%d %b %Y")),
      if (rv$n_added > 0) tags$span(class = "text-success",
                                    sprintf("Includes %s events added from PHIVOLCS.", fmt_n(rv$n_added))))
  })

  observeEvent(input$update_data, {
    updateActionButton(session, "update_data", disabled = TRUE)
    on.exit(updateActionButton(session, "update_data", disabled = FALSE))

    since <- max(rv$data$DateTime, na.rm = TRUE)
    result <- tryCatch(
      withProgress(message = "Downloading from PHIVOLCS", value = 0, {
        scrape_phivolcs(since, progress = function(value, detail) setProgress(value, detail = detail))
      }),
      error = function(e) {
        showNotification(paste("Could not reach PHIVOLCS:", conditionMessage(e)),
                         type = "error", duration = 15)
        NULL
      }
    )
    req(result)
    rv$scrape_pages <- result$pages

    if (is.null(result$data)) {
      showNotification(
        "No new events found. Either the data is already up to date or the PHIVOLCS website could not be read. Please try again later.",
        type = "warning", duration = 12)
      return()
    }

    old_n <- nrow(rv$data)
    new_std <- bind_rows(rv$std, result$data)
    new_data <- clean_eq_data(new_std)
    rv$std <- new_std
    rv$data <- new_data
    added <- nrow(new_data) - old_n
    rv$n_added <- rv$n_added + added

    # Extend the date filter to include the new events.
    new_range <- range(new_data$Date)
    updateDateRangeInput(session, "date_range", max = new_range[2], end = new_range[2],
                         start = input$date_range[1])
    showNotification(
      sprintf("Added %s new events, up to %s.", fmt_n(added), format(new_range[2], "%d %b %Y")),
      type = "message", duration = 10)
  })

  # ---- Overview ----------------------------------------------------------------
  output$value_boxes <- renderUI({
    d <- filtered()
    if (nrow(d) == 0) {
      return(div(class = "alert alert-warning", "No earthquakes match the current filters."))
    }
    big <- d[which.max(d$Mag), ]
    big_where <- paste0(
      if (!is.na(big$City_Municipality)) big$City_Municipality else "",
      if (!is.na(big$Province)) paste0(", ", big$Province) else ""
    )
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Earthquakes recorded", value = fmt_n(nrow(d)),
        showcase = icon("wave-square"), theme = "primary",
        p(sprintf("%s to %s", format(min(d$Date), "%d %b %Y"), format(max(d$Date), "%d %b %Y")))
      ),
      value_box(
        title = "Largest magnitude", value = sprintf("M %.1f", big$Mag),
        showcase = icon("arrow-up-wide-short"), theme = "light",
        p(format(big$Date, "%d %b %Y"), if (nzchar(big_where)) paste0("near ", big_where))
      ),
      value_box(
        title = "Magnitude 5.0 or higher", value = fmt_n(sum(d$Mag >= 5)),
        showcase = icon("triangle-exclamation"), theme = "light",
        p(sprintf("%s of magnitude 6.0 or higher", fmt_n(sum(d$Mag >= 6))))
      ),
      value_box(
        title = "Median depth", value = sprintf("%.0f km", median(d$Depth_km, na.rm = TRUE)),
        showcase = icon("arrow-down"), theme = "light",
        p(sprintf("%.0f%% shallow (less than 70 km deep)",
                  100 * mean(d$Depth_km < 70, na.rm = TRUE)))
      )
    )
  })

  mag_dist <- reactive(plot_mag_distribution(filtered()))
  depth_dist <- reactive(plot_depth_distribution(filtered()))
  output$mag_dist_plot <- renderPlot(mag_dist(), res = 96) |> bindCache(filter_key())
  output$depth_dist_plot <- renderPlot(depth_dist(), res = 96) |> bindCache(filter_key())

  output$year_table <- renderDT({
    tbl <- summarise_by_year(filtered())
    if ("Message" %in% names(tbl)) return(datatable(tbl, rownames = FALSE, options = list(dom = "t")))
    datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE),
      caption = "Counts by magnitude threshold are cumulative (an M 6.2 event is counted in all three columns)."
    ) |>
      formatRound(c("Events", "M ≥ 4.0", "M ≥ 5.0", "M ≥ 6.0"), digits = 0) |>
      formatRound("Largest magnitude", 1) |>
      formatRound("Median depth (km)", 0)
  })

  # ---- Map ---------------------------------------------------------------------
  map_plot <- reactive(plot_map(filtered(), PH_OUTLINE, input$map_color))
  output$map_plot <- renderPlot(map_plot(), res = 96) |>
    bindCache(filter_key(), input$map_color)

  output$map_click_info <- renderUI({
    click <- input$map_click
    if (is.null(click)) return(NULL)
    pts <- map_points(filtered())
    near <- nearPoints(pts, click, xvar = "Longitude", yvar = "Latitude",
                       threshold = 8, maxpoints = 10)
    if (nrow(near) == 0) return(help_text("No event at that spot. Try clicking closer to a circle's centre."))
    near <- near |> arrange(desc(Mag))
    tagList(lapply(seq_len(nrow(near)), function(i) {
      e <- near[i, ]
      div(class = "border-bottom pb-2 mb-2",
          strong(sprintf("M %.1f", e$Mag)), " · ",
          format(e$Date, "%d %b %Y"), if (!is.na(e$Time)) paste(e$Time, "PhST"), br(),
          sprintf("%s km %s of %s%s", fmt_n(e$Distance_to_Reference_km),
                  coalesce(e$Direction_to_Reference, ""), coalesce(e$City_Municipality, "?"),
                  if (!is.na(e$Province)) paste0(" (", e$Province, ")") else ""), br(),
          span(class = "text-muted small",
               sprintf("Depth: %s · %.2f°N, %.2f°E · %s",
                       if (is.na(e$Depth_km)) "not reported" else paste(e$Depth_km, "km"),
                       e$Latitude, e$Longitude, as.character(e$Mag_class))))
    }))
  })

  # ---- Trends ------------------------------------------------------------------
  trend_plot <- reactive({
    if (input$trend_var == "count") {
      plot_counts_over_time(filtered(), input$trend_unit, separate = isTRUE(input$trend_separate))
    } else {
      plot_metric_trend(filtered(), input$trend_unit, input$trend_var)
    }
  })
  output$trend_plot <- renderPlot(trend_plot(), res = 96) |>
    bindCache(filter_key(), input$trend_var, input$trend_unit, input$trend_separate)

  # ---- Provinces ---------------------------------------------------------------
  province_plot <- reactive(plot_provinces(filtered(), input$top_n))
  output$province_plot <- renderPlot(province_plot(), res = 96) |>
    bindCache(filter_key(), input$top_n)

  output$province_table <- renderDT({
    tbl <- summarise_by_province(filtered())
    if ("Message" %in% names(tbl)) return(datatable(tbl, rownames = FALSE, options = list(dom = "t")))
    datatable(tbl, rownames = FALSE, filter = "top",
              options = list(pageLength = 15)) |>
      formatRound("Events", 0) |>
      formatPercentage("Share of events", 1) |>
      formatRound("Largest magnitude", 1) |>
      formatRound("Median depth (km)", 0)
  })

  # ---- Data table --------------------------------------------------------------
  output$data_table <- renderDT({
    d <- filtered() |>
      transmute(
        Date = Date,
        `Time (PhST)` = Time,
        Magnitude = Mag,
        `Magnitude class` = Mag_class,
        `Depth (km)` = Depth_km,
        `Depth class` = Depth_class,
        `Latitude (°N)` = Latitude,
        `Longitude (°E)` = Longitude,
        `Reference location` = sprintf("%s km %s of %s", Distance_to_Reference_km,
                                       coalesce(Direction_to_Reference, "?"),
                                       coalesce(City_Municipality, "?")),
        # Factors get a dropdown filter in the table header.
        Province = factor(Province),
        `Island group` = Island_group,
        `Coordinates valid` = ifelse(Coord_valid, "Yes", "No"),
        Source = factor(Source)
      )
    # No scrollX: DataTables' scrollX clones the header, and DT then attaches
    # two range sliders to each numeric/date filter. The wrapping div in the
    # UI scrolls horizontally instead.
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, autoWidth = FALSE, order = list())) |>
      formatRound("Magnitude", 1)
  }, server = TRUE)

  # ---- Downloads ---------------------------------------------------------------
  stamp <- function() format(Sys.time(), "%Y%m%d-%H%M", tz = PH_TZ)

  output$dl_filtered <- downloadHandler(
    filename = function() paste0("ph-earthquakes-filtered-", stamp(), ".csv"),
    content = function(file) write_utf8_csv(export_eq_data(filtered()), file)
  )
  output$dl_all <- downloadHandler(
    filename = function() paste0("ph-earthquakes-all-", stamp(), ".csv"),
    content = function(file) write_utf8_csv(export_eq_data(rv$data), file)
  )

  save_plot <- function(plot, file) {
    clamp <- function(x, lo, hi, default) if (is.numeric(x) && !is.na(x)) min(max(x, lo), hi) else default
    ggsave(file, plot = plot, device = "png", bg = "white", units = "in",
           width = clamp(input$plot_width, 3, 30, 11),
           height = clamp(input$plot_height, 3, 30, 7),
           dpi = clamp(input$plot_dpi, 72, 600, 300))
  }
  plot_download <- function(name, plot_reactive) {
    downloadHandler(
      filename = function() paste0("ph-earthquakes-", name, "-", stamp(), ".png"),
      content = function(file) save_plot(plot_reactive(), file)
    )
  }
  output$dl_map <- plot_download("map", map_plot)
  output$dl_trend <- plot_download("trend", trend_plot)
  output$dl_province <- plot_download("provinces", province_plot)
  output$dl_mag_dist <- plot_download("magnitude-distribution", mag_dist)
  output$dl_depth_dist <- plot_download("depth-distribution", depth_dist)

  # ---- About -------------------------------------------------------------------
  output$about <- renderUI(about_page(
    cleaning_log = attr(rv$data, "cleaning_log"),
    scrape_pages = rv$scrape_pages,
    data_range = range(rv$data$Date)
  ))
}

shinyApp(ui = ui, server = server)
