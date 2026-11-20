# =============================================================================
# Charts and summary tables
# -----------------------------------------------------------------------------
# Each chart is built by one function, used both for the on-screen plot and for
# the PNG download, so the two can never drift apart.
# =============================================================================

# -----------------------------------------------------------------------------
# Colours
# -----------------------------------------------------------------------------
# Magnitude and depth classes are ordered, so each uses a single-hue ramp that
# goes from light (small/shallow) to dark (large/deep). Both ramps were checked
# for monotone lightness and visible steps between classes. Nothing is
# shown by colour alone: every chart also has a legend, and the map uses size too.
MAG_GROUP_COLORS <- setNames(c("#86b6ef", "#3987e5", "#256abf", "#184f95", "#0d366b"),
                             MAG_GROUP_LABELS)
DEPTH_CLASS_COLORS <- setNames(c("#f0a070", "#d95926", "#8a3412"), DEPTH_CLASS_LABELS)
SERIES_COLOR <- "#2a78d6"
SERIES_LIGHT <- "#b7d3f6"
TEXT_PRIMARY <- "#0b0b0b"
TEXT_SECONDARY <- "#52514e"
GRID_COLOR <- "#e5e4e0"
LAND_FILL <- "#f0efec"
LAND_BORDER <- "#a8a7a1"

DATA_CREDIT <- "Data: PHIVOLCS-DOST earthquake information (earthquake.phivolcs.dost.gov.ph)"

theme_eq <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = TEXT_PRIMARY),
      plot.title = element_text(face = "bold", size = rel(1.2), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = TEXT_SECONDARY, margin = margin(b = 10)),
      plot.caption = element_text(colour = TEXT_SECONDARY, size = rel(0.75), hjust = 0),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      axis.text = element_text(colour = TEXT_SECONDARY),
      axis.title = element_text(colour = TEXT_SECONDARY),
      panel.grid.major = element_line(colour = GRID_COLOR, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = rel(0.85), colour = TEXT_SECONDARY),
      legend.text = element_text(size = rel(0.85)),
      strip.text = element_text(face = "bold", hjust = 0),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(12, 16, 12, 12)
    )
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

# One-line description of what is plotted, appended to every caption.
describe_selection <- function(df) {
  if (nrow(df) == 0) return("No events match the current filters.")
  sprintf("%s events, %s to %s, magnitude %.1f to %.1f.",
          fmt_n(nrow(df)), format(min(df$Date), "%d %b %Y"),
          format(max(df$Date), "%d %b %Y"), min(df$Mag), max(df$Mag))
}

# Wraps long text so it fits the plot (ggplot2 does not wrap titles itself).
wrap_text <- function(x, width = 105) paste(stringr::str_wrap(x, width = width), collapse = "\n")

make_caption <- function(df, extra = NULL, width = 120) {
  lines <- c(describe_selection(df), extra, DATA_CREDIT)
  paste(vapply(lines, wrap_text, "", width = width), collapse = "\n")
}

empty_plot <- function(msg = "No earthquakes match the current filters.\nTry widening the date range, magnitude range or province selection.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, colour = TEXT_SECONDARY, size = 5) +
    theme_void()
}

TIME_UNITS <- c("Week" = "week", "Month" = "month", "Quarter" = "quarter", "Year" = "year")

period_label <- function(unit) names(TIME_UNITS)[match(unit, TIME_UNITS)]

date_scale <- function(unit) {
  if (unit == "year") {
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = expansion(mult = 0.01))
  } else {
    scale_x_date(date_labels = "%b\n%Y", breaks = scales::breaks_pretty(10),
                 expand = expansion(mult = 0.01))
  }
}

# -----------------------------------------------------------------------------
# Map
# -----------------------------------------------------------------------------

# Converts the sf outline into a plain polygon data frame so the map can use
# coord_quickmap(), which lets Shiny translate mouse clicks into lon/lat.
sf_to_polygon_df <- function(sf_obj) {
  xy <- as.data.frame(sf::st_coordinates(sf_obj))
  group_cols <- intersect(c("L1", "L2", "L3"), names(xy))
  xy$group <- interaction(xy[group_cols], drop = TRUE)
  xy[, c("X", "Y", "group")]
}

map_extent <- function(df) {
  lon <- c(116.5, 127, df$Longitude)
  lat <- c(4.5, 21.5, df$Latitude)
  list(x = range(lon, na.rm = TRUE) + c(-0.5, 0.5), y = range(lat, na.rm = TRUE) + c(-0.5, 0.5))
}

# Drawing 100,000+ circles takes 15+ seconds, so the map shows at most this many
# events (the largest ones). The caption says when this happens.
MAP_MAX_POINTS <- 15000

# Events actually drawn on the map (valid coordinates, largest MAP_MAX_POINTS),
# ordered so the largest events are drawn last, on top.
map_points <- function(df) {
  pts <- df |> filter(Coord_valid)
  if (nrow(pts) > MAP_MAX_POINTS) pts <- pts |> slice_max(Mag, n = MAP_MAX_POINTS, with_ties = FALSE)
  pts |> arrange(Mag)
}

plot_map <- function(df, outline, color_by = c("magnitude", "depth")) {
  color_by <- match.arg(color_by)
  pts <- map_points(df)
  if (nrow(pts) == 0) return(empty_plot())
  n_invalid <- sum(!df$Coord_valid)
  n_capped <- sum(df$Coord_valid) - nrow(pts)
  ext <- map_extent(pts)

  if (color_by == "magnitude") {
    pts$Fill <- pts$Mag_group
    fill_scale <- scale_fill_manual(values = MAG_GROUP_COLORS, name = "Magnitude (colour)", drop = TRUE)
  } else {
    not_reported <- "Depth not reported"
    pts$Fill <- factor(ifelse(is.na(pts$Depth_class), not_reported, as.character(pts$Depth_class)),
                       levels = c(DEPTH_CLASS_LABELS, not_reported))
    fill_scale <- scale_fill_manual(values = c(DEPTH_CLASS_COLORS, "Depth not reported" = "#bdbcb6"),
                                    name = "Depth (colour)", drop = TRUE)
  }

  extra <- c(
    if (color_by == "magnitude") "Each circle is one epicentre. Circle size and colour both show magnitude."
    else "Each circle is one epicentre. Circle size shows magnitude; colour shows depth.",
    if (n_capped > 0) sprintf(
      "For speed, only the %s largest events are drawn (%s smaller ones hidden). Raise the minimum magnitude or narrow the dates to see all events.",
      fmt_n(nrow(pts)), fmt_n(n_capped)),
    if (n_invalid > 0) sprintf("%s event(s) with invalid coordinates are not shown.", fmt_n(n_invalid))
  )

  ggplot() +
    geom_polygon(data = outline, aes(X, Y, group = group),
                 fill = LAND_FILL, colour = LAND_BORDER, linewidth = 0.25) +
    geom_point(data = pts, aes(Longitude, Latitude, size = Mag, fill = Fill),
               shape = 21, colour = "white", stroke = 0.15, alpha = 0.85) +
    fill_scale +
    scale_size(range = c(0.6, 7), limits = c(1, 8.5), breaks = c(3, 4, 5, 6, 7),
               name = "Magnitude (size)") +
    guides(fill = guide_legend(override.aes = list(size = 4.5, alpha = 1), order = 1),
           size = guide_legend(override.aes = list(fill = "#8a8984"), order = 2)) +
    coord_quickmap(xlim = ext$x, ylim = ext$y, expand = FALSE) +
    labs(title = "Earthquake epicentres",
         x = "Longitude (°E)", y = "Latitude (°N)",
         caption = paste(make_caption(df, extra, width = 90),
                         "Outline: Natural Earth (naturalearthdata.com), public domain.", sep = "\n")) +
    theme_eq() +
    theme(panel.background = element_rect(fill = "white", colour = GRID_COLOR),
          legend.position = "right", legend.justification = "top",
          legend.box = "vertical")
}

# -----------------------------------------------------------------------------
# Time series
# -----------------------------------------------------------------------------

plot_counts_over_time <- function(df, unit = "month", separate = FALSE) {
  if (nrow(df) == 0) return(empty_plot())
  counts <- df |>
    mutate(Period = as.Date(floor_date(Date, unit, week_start = 1))) |>
    count(Period, Mag_group, .drop = FALSE) |>
    filter(!is.na(Period))

  p <- ggplot(counts, aes(Period, n, fill = Mag_group)) +
    geom_col(width = switch(unit, week = 6, month = 26, quarter = 80, year = 330)) +
    scale_fill_manual(values = MAG_GROUP_COLORS, name = "Magnitude", drop = TRUE) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.05))) +
    date_scale(unit) +
    labs(title = sprintf("Number of recorded earthquakes per %s", tolower(period_label(unit))),
         x = NULL, y = "Number of events",
         caption = make_caption(df, paste(
           "Counts are of events in the PHIVOLCS catalogue. Changes over time may partly reflect",
           "changes in the monitoring network and in reporting, as well as aftershock sequences."))) +
    theme_eq()

  if (separate) {
    p <- p + facet_wrap(~Mag_group, ncol = 1, scales = "free_y") +
      theme(legend.position = "none")
  }
  p
}

plot_metric_trend <- function(df, unit = "month", variable = c("Mag", "Depth_km")) {
  variable <- match.arg(variable)
  d <- df |> filter(!is.na(.data[[variable]]))
  if (nrow(d) == 0) return(empty_plot())

  stats <- d |>
    mutate(Period = as.Date(floor_date(Date, unit, week_start = 1))) |>
    group_by(Period) |>
    summarise(
      n = n(),
      q25 = quantile(.data[[variable]], 0.25),
      median = median(.data[[variable]]),
      q75 = quantile(.data[[variable]], 0.75),
      max = max(.data[[variable]]),
      .groups = "drop"
    )

  is_mag <- variable == "Mag"
  label <- if (is_mag) "Magnitude" else "Depth (km)"

  # The subtitle explains each layer, so no legend is needed.
  p <- ggplot(stats, aes(Period)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = SERIES_LIGHT) +
    geom_line(aes(y = median), colour = SERIES_COLOR, linewidth = 0.8) +
    geom_point(aes(y = max), colour = "#0d366b", size = 1.6) +
    date_scale(unit) +
    labs(title = sprintf("%s of recorded earthquakes, by %s", label, tolower(period_label(unit))),
         x = NULL, y = label) +
    theme_eq()

  if (is_mag) {
    p + labs(
      subtitle = wrap_text("Line: median magnitude. Band: middle 50% of events (25th to 75th percentile). Dots: largest event in each period."),
      caption = make_caption(df, paste(
        "Small earthquakes are detected more often as monitoring improves. A falling median can therefore",
        "reflect better detection, not smaller earthquakes. Compare like with like by raising the minimum magnitude."
      )))
  } else {
    p + scale_y_reverse() + labs(
      subtitle = wrap_text("Depth increases downwards (0 km = surface). Line: median depth. Band: middle 50% of events. Dots: deepest event."),
      caption = make_caption(df, "Events with no reported depth are excluded."))
  }
}

# -----------------------------------------------------------------------------
# Distributions
# -----------------------------------------------------------------------------

plot_mag_distribution <- function(df) {
  if (nrow(df) == 0) return(empty_plot())
  counts <- df |> count(Mag = round(Mag, 1), Mag_group)
  # Bars are drawn from 0.5 (not 1) so that a single event is still visible
  # on the logarithmic axis.
  ggplot(counts, aes(xmin = Mag - 0.04, xmax = Mag + 0.04, ymin = 0.5, ymax = n, fill = Mag_group)) +
    geom_rect() +
    scale_fill_manual(values = MAG_GROUP_COLORS, name = "Magnitude") +
    scale_y_log10(labels = scales::label_comma(), breaks = 10^(0:6),
                  expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = 1:9) +
    labs(title = "How many earthquakes of each magnitude?",
         subtitle = wrap_text("Number of events per 0.1 magnitude step. The y-axis is logarithmic: each gridline is 10 times the one below."),
         x = "Magnitude", y = "Number of events (log scale)",
         caption = make_caption(df, "Below about M 2.5, the drop-off mostly shows that small events are harder to detect.")) +
    theme_eq()
}

plot_depth_distribution <- function(df) {
  d <- df |> filter(!is.na(Depth_km))
  if (nrow(d) == 0) return(empty_plot())
  counts <- d |>
    mutate(Depth_bin = floor(Depth_km / 10) * 10 + 5) |>
    count(Depth_bin) |>
    mutate(Depth_class = classify_depth(Depth_bin))
  ggplot(counts, aes(xmin = 0.5, xmax = n, ymin = Depth_bin - 4.25, ymax = Depth_bin + 4.25,
                     fill = Depth_class)) +
    geom_rect() +
    scale_fill_manual(values = DEPTH_CLASS_COLORS, name = "Depth class", drop = FALSE) +
    scale_y_reverse(breaks = seq(0, 700, 100)) +
    scale_x_log10(labels = scales::label_comma(), breaks = 10^(0:6),
                  expand = expansion(mult = c(0, 0.05))) +
    labs(title = "How deep are the earthquakes?",
         subtitle = wrap_text("Number of events per 10 km of depth (0 km = surface). The x-axis is logarithmic."),
         x = "Number of events (log scale)", y = "Depth (km)",
         caption = make_caption(d, "Events with no reported depth are excluded.")) +
    theme_eq()
}

# -----------------------------------------------------------------------------
# Provinces
# -----------------------------------------------------------------------------

plot_provinces <- function(df, top_n = 20) {
  d <- df |> filter(!is.na(Province))
  if (nrow(d) == 0) return(empty_plot())
  counts <- d |> count(Province, sort = TRUE)
  n_prov <- nrow(counts)
  counts <- counts |> slice_head(n = top_n) |>
    mutate(Province = factor(Province, levels = rev(Province)))

  ggplot(counts, aes(n, Province)) +
    geom_col(fill = SERIES_COLOR, width = 0.75) +
    geom_text(aes(label = fmt_n(n)), hjust = -0.15, size = 3.6, colour = TEXT_SECONDARY) +
    scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.12))) +
    labs(title = sprintf("Provinces with the most recorded earthquakes (top %d of %d)",
                         min(top_n, n_prov), n_prov),
         subtitle = wrap_text("Province of the reference town PHIVOLCS uses to describe the epicentre. Offshore events count toward the nearest reference town's province."),
         x = "Number of events", y = NULL,
         caption = make_caption(d)) +
    theme_eq() +
    theme(panel.grid.major.y = element_blank())
}

# -----------------------------------------------------------------------------
# Summary tables
# -----------------------------------------------------------------------------

summarise_by_year <- function(df) {
  if (nrow(df) == 0) return(data.frame(Message = "No earthquakes match the current filters."))
  df |>
    group_by(Year) |>
    summarise(
      Events = n(),
      `M ≥ 4.0` = sum(Mag >= 4),
      `M ≥ 5.0` = sum(Mag >= 5),
      `M ≥ 6.0` = sum(Mag >= 6),
      `Largest magnitude` = max(Mag),
      `Median depth (km)` = median(Depth_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(Year))
}

summarise_by_province <- function(df) {
  total <- nrow(df)
  df <- df |> filter(!is.na(Province))
  if (nrow(df) == 0) return(data.frame(Message = "No earthquakes match the current filters."))
  df |>
    group_by(Province, `Island group` = Island_group) |>
    summarise(
      Events = n(),
      `Share of events` = n() / total,
      `M ≥ 5.0` = sum(Mag >= 5),
      `Largest magnitude` = max(Mag),
      `Date of largest` = Date[which.max(Mag)],
      `Median depth (km)` = median(Depth_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(Events))
}
