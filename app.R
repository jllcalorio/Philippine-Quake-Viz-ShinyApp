# PHIVOLCS Earthquake Data Visualization Shiny App
# Install and load required packages
pacman::p_load(shiny, rvest, dplyr, stringr, purrr, lubridate,
               ggplot2, sf, rnaturalearth, rnaturalearthdata,
               rnaturalearthhires, DT, zip)

# Define UI
ui <- fluidPage(
  titlePanel("PHIVOLCS Earthquake Data Visualization (2017-2025)"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Data source buttons
      h4("Data Source"),
      downloadButton("download_default", "Download Default Data",
                     class = "btn-primary", style = "margin-bottom: 10px; width: 100%;"),
      actionButton("use_latest", "Use Latest Data",
                   class = "btn-warning", style = "margin-bottom: 10px; width: 100%;"),
      conditionalPanel(
        condition = "input.use_latest > 0",
        downloadButton("download_latest", "Download Latest Data",
                       class = "btn-success", style = "margin-bottom: 20px; width: 100%;")
      ),

      hr(),

      # Filters
      h4("Filters"),
      checkboxGroupInput("year_filter", "Select Year(s):",
                         choices = c("All"),
                         selected = "All"),

      selectizeInput("province_filter", "Select Province(s):",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(placeholder = "Type or select provinces...")),

      sliderInput("mag_filter", "Magnitude Range:",
                  min = 1, max = 10, value = c(4, 8), step = 0.1),

      hr(),

      # Download plot buttons
      h4("Download Plots"),

      # Added input fields for plot dimensions and resolution
      numericInput("plot_width", "Plot Width (inches):", value = 15, min = 1, step = 1),
      numericInput("plot_height", "Plot Height (inches):", value = 15, min = 1, step = 1),
      numericInput("plot_dpi", "Plot DPI (Resolution):", value = 300, min = 50, step = 50),

      downloadButton("download_map", "Map Plot", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_daily_trend", "Daily Trend", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_monthly_trend", "Monthly Trend", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_daily_depth", "Daily Depth", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_monthly_depth", "Monthly Depth", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_freq_province", "Frequency by Province", style = "margin-bottom: 5px; width: 100%;"),
      downloadButton("download_all_plots", "Download All Plots (ZIP)",
                     class = "btn-danger", style = "margin-top: 10px; width: 100%;")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Earthquakes Map", plotOutput("map_plot", height = "1000px")),
        tabPanel("Daily Magnitude Trend", plotOutput("daily_trend_plot", height = "1000px")),
        tabPanel("Monthly Magnitude Trend", plotOutput("monthly_trend_plot", height = "1000px")),
        tabPanel("Daily Depth", plotOutput("daily_depth_plot", height = "1000px")),
        tabPanel("Monthly Depth", plotOutput("monthly_depth_plot", height = "1000px")),
        tabPanel("EQ Frequency by Province", plotOutput("freq_province_plot", height = "1000px")),
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("About & Contribute",
                 fluidRow(
                   column(12,
                          h3("About This Shiny App"),
                          p(strong("The Philippines, situated along the volatile Pacific Ring of Fire, experiences frequent and ongoing seismic activity.")),
                          p("This dashboard provides a comprehensive visualization and analysis of earthquake data sourced directly from the Philippine Institute of Volcanology and Seismology (PHIVOLCS) from 2017 to the present."),
                          p("It allows users to filter, visualize, and download trends in magnitude, depth, and frequency across various provinces, aiding in geological study and risk awareness."),
                          hr(),

                          h3("Author & Contact"),
                          p(strong("John Lennon L. Calorio")),
                          p("Health Data Scientist and Bioinformatician"),
                          p("Email: ", a(href = "mailto:jllcalorio@gmail.com", "jllcalorio@gmail.com")),
                          p("Current Position: Senior Statistician at Center for Research and Development, Davao Medical School Foundation, Inc."),

                          h3("Contribute to this Shiny App"),
                          p("GitHub repository of this Shiny App: ", a(href = "https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp", "https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp")),

                          hr(),

                          h3("Acknowledgements & Methods"),
                          p("Thanks to ", strong("PHIVOLCS"), " for making the data available for everyone. The data in this app is obtained by web scraping from their website: ", a(href = "https://earthquake.phivolcs.dost.gov.ph/", "https://earthquake.phivolcs.dost.gov.ph/")),

                          h4("Things I did to make this happen:"),
                          tags$ul(
                            tags$li("Web scraping"),
                            tags$li("Data manipulation (auto and manual)"),
                            tags$li("Data analysis and visualization using R"),
                            tags$li("Deployment via ShinyApps")
                          ),

                          h4("R packages used:"),
                          tags$ul(
                            tags$li("shiny, rvest, dplyr, stringr, purrr, lubridate,"),
                            tags$li("ggplot2, sf, rnaturalearth, rnaturalearthdata,"),
                            tags$li("rnaturalearthhires, DT, zip")
                          ),

                          h4("Stay safe everyone!")
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store data
  rv <- reactiveValues(
    earthquake_data = NULL,
    latest_data = NULL,
    use_latest = FALSE
  )

  # Load default data on startup
  observe({
    req(file.exists("default_data.csv"))
    rv$earthquake_data <- read.csv("default_data.csv", stringsAsFactors = FALSE) %>%
      mutate(
        Date = as.Date(Date),
        Mag_scale = factor(Mag_scale, levels = sort(unique(Mag_scale)))
      )

    # Update year filter choices
    years <- sort(unique(rv$earthquake_data$Year))
    updateCheckboxGroupInput(session, "year_filter",
                             choices = c("All", as.character(years)),
                             selected = "All")

    # Update province filter choices
    provinces <- sort(unique(rv$earthquake_data$Province[!is.na(rv$earthquake_data$Province)]))
    updateSelectizeInput(session, "province_filter", choices = provinces)

    # Update magnitude slider
    mag_range <- range(rv$earthquake_data$Mag, na.rm = TRUE)
    updateSliderInput(session, "mag_filter",
                      min = 1,
                      max = max(10, ceiling(mag_range[2])),
                      value = c(4, 8))
  })

  # Web scraping function (same as original code)
  scrape_phivolcs_data <- function() {
    showModal(modalDialog("Scraping data from PHIVOLCS... This may take a few minutes.",
                          footer = NULL, easyClose = FALSE))

    # Define standardized column names
    standard_cols <- c("Date - Time (Philippine Time)",
                       "Latitude",
                       "Longitude",
                       "Depth_km",
                       "Mag",
                       "Location")

    # Helper function to clean text
    clean_text <- function(x) {
      x %>%
        str_replace_all("\\s+", " ") %>%
        str_squish()
    }

    # Helper function to safely extract table from a URL
    extract_phivolcs_table <- function(url, year = NA, month = NA) {
      tryCatch({
        page <- read_html(url)
        tables <- page %>% html_table(fill = TRUE)
        if (length(tables) == 0) return(NULL)

        tbl <- tables[[which.max(sapply(tables, nrow))]]
        colnames(tbl) <- make.names(colnames(tbl), unique = TRUE)
        tbl[] <- lapply(tbl, clean_text)

        if (ncol(tbl) == 12) {
          tbl1 <- tbl[, 1:6]
          tbl2 <- tbl[, 7:12]
          tbl_list <- list(tbl1, tbl2)
          tbl_list <- lapply(tbl_list, function(t) {
            if (ncol(t) >= 6) {
              t <- t[, 1:6]
              colnames(t) <- standard_cols
              return(t)
            } else {
              return(NULL)
            }
          })
          tbl <- bind_rows(tbl_list)
        } else if (ncol(tbl) >= 6) {
          tbl <- tbl[, 1:6]
          colnames(tbl) <- standard_cols
        } else {
          return(NULL)
        }

        tbl %>% mutate(Year = year, Month = month)
      },
      error = function(e) {
        NULL
      })
    }

    years <- 2017:2025
    months <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")

    all_data <- list()

    # Loop through archive pages
    for (year in years) {
      for (month in months) {
        url <- sprintf("https://earthquake.phivolcs.dost.gov.ph/EQLatest-Monthly/%d/%d_%s.html",
                       year, year, month)
        tbl <- extract_phivolcs_table(url, year, month)
        if (!is.null(tbl)) all_data[[paste0(year, "_", month)]] <- tbl
      }
    }

    # Add latest data
    latest_url <- "https://earthquake.phivolcs.dost.gov.ph/"
    latest_tbl <- extract_phivolcs_table(latest_url)

    if (!is.null(latest_tbl)) {
      inferred_date <- latest_tbl[1, 1] %>%
        str_extract("\\b\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}\\b") %>%
        na.omit()

      if (length(inferred_date) > 0) {
        parsed_date <- as.Date(inferred_date[1], format = "%d %B %Y")
        inferred_year <- format(parsed_date, "%Y")
        inferred_month <- format(parsed_date, "%B")
        latest_tbl <- latest_tbl %>%
          mutate(Year = as.integer(inferred_year), Month = inferred_month)
      } else {
        latest_tbl <- latest_tbl %>%
          mutate(Year = max(years), Month = NA)
      }
      all_data[["latest"]] <- latest_tbl
    }

    # Combine and clean data
    earthquake_data <- bind_rows(all_data) %>%
      dplyr::filter(
        Mag >= 0,
        Location != "Location",
        Location != "Taiwan Region",
        !is.na(as.numeric(Latitude)),
        !is.na(as.numeric(Longitude))
      ) %>%
      mutate(
        Date_str = str_extract(`Date - Time (Philippine Time)`, "^\\d{1,2}\\s+\\w+\\s+\\d{4}"),
        Date = parse_date_time(Date_str, orders = c("d b Y", "d B Y")),
        Day = as.integer(str_extract(`Date - Time (Philippine Time)`, "^\\d{1,2}")),
        Time_12h = str_extract(`Date - Time (Philippine Time)`, "\\d{1,2}:\\d{2}\\s*[APap][Mm]"),
        Time = format(parse_date_time(Time_12h, orders = "I:M p"), "%H:%M"),
        Year = as.integer(Year),
        `Depth_km` = ifelse(`Depth_km` %in% c("--", "< 001", "<001"), 0.00001, `Depth_km`),
        Latitude = as.numeric(Latitude),
        Longitude = as.numeric(Longitude),
        `Depth_km` = as.numeric(`Depth_km`),
        Mag = as.numeric(Mag)
      )

    # PHIVOLCS Earthquake Intensity Scale
    peis <- c("1-1.9: Scarcely Perceptible", "2-2.9: Slightly Felt",
              "3-3.9: Weak", "4-4.9: Moderately Strong",
              "5-5.9: Strong", "6-6.9: Very Strong",
              "7-7.9: Destructive", "8-8.9: Very Destructive",
              "9-9.9: Devastating", ">10: Very Devastating")

    earthquake_data <- earthquake_data %>%
      mutate(
        Mag_scale = case_when(
          Mag < 2 ~ peis[1], Mag < 3 ~ peis[2], Mag < 4 ~ peis[3],
          Mag < 5 ~ peis[4], Mag < 6 ~ peis[5], Mag < 7 ~ peis[6],
          Mag < 8 ~ peis[7], Mag < 9 ~ peis[8], Mag < 10 ~ peis[9],
          Mag >= 10 ~ peis[10], TRUE ~ NA_character_
        )
      )

    # Fix locations
    earthquake_data <- earthquake_data %>%
      mutate(
        Location = Location %>%
          str_replace_all("\\(\\s*\\(", "(") %>%
          str_replace_all("\\s*\\)\\s*\\)", ")") %>%
          str_replace_all("\\s{2,}", " ") %>%
          str_squish(),
        `Distance to Reference (km)` = as.numeric(str_extract(Location, "^\\s*\\d+")),
        `Direction to Reference` = str_extract(Location, "(?<=km\\s)(.*?)(?=\\s+of)") %>%
          str_replace_all("\\s+", " ") %>%
          str_squish(),
        Location2 = str_extract(Location, "(?<= of )[^of]+\\([^\\)]+\\)$") %>%
          str_squish(),
        Location2 = if_else(is.na(Location2),
                            str_extract(Location, "(?<= of ).*$") %>% str_squish(),
                            Location2),
        City_Municipality = str_extract(Location2, "^[^(]+") %>% str_trim(),
        Province = str_extract(Location2, "(?<=\\()[^)]+(?=\\))") %>% str_trim()
      ) %>%
      mutate(Mag_scale = factor(Mag_scale, sort(unique(Mag_scale))))

    # Province corrections
    corrections <- c(
      "Anao-oan" = "Anao-aon", "Agusan Del Norte" = "Agusan del Norte",
      "Agusan De Sur" = "Agusan del Sur", "Agusan Del Sur" = "Agusan del Sur",
      "Batanagas" = "Batangas", "Camarines Surl" = "Camarines Sur",
      "CamarinesSur" = "Camarines Sur", "Cataduanes" = "Catanduanes",
      "Compostella Valley" = "Compostela Valley", "Davao De Oro" = "Davao de Oro",
      "Davao Del Norte" = "Davao del Norte", "Davao Del Sur" = "Davao del Sur",
      "Davao Del Occidental" = "Davao Occidental", "Davao Occicental" = "Davao Occidental",
      "Davao Occidenta" = "Davao Occidental", "Davao Occidentall" = "Davao Occidental",
      "Davao Oreintal" = "Davao Oriental", "Davao Orientall" = "Davao Oriental",
      "Davao Orinetal" = "Davao Oriental", "Dinagat" = "Dinagat Islands",
      "Easter Samar" = "Eastern Samar", "Easterm Samar" = "Eastern Samar",
      "Eastren Samar" = "Eastern Samar", "Eatern Samar" = "Eastern Samar",
      "Island Garden City of Samal (Davao Del Norte" = "Island Garden City Of Samal",
      "La union" = "La Union", "Lanao De Sur" = "Lanao del Sur",
      "Lanao Del Sur" = "Lanao del Sur", "Lanao Del Nortel" = "Lanao del Norte",
      "Lanao Del Norte" = "Lanao del Norte", "Masbate (Ticao Island" = "Masbate",
      "Maguindanao Del Norte" = "Maguindanao del Norte",
      "Maguindanao Del Sur" = "Maguindanao del Sur",
      "Negros Occedental" = "Negros Occidental", "Northen Samar" = "Northern Samar",
      "Nuevq Ecija" = "Nueva Ecija", "Nueva Viscaya" = "Nueva Vizcaya",
      "Occ.mindoro" = "Occidental Mindoro", "Occidental" = "Occidental Mindoro",
      "Oriemtal Mindoro" = "Oriental Mindoro", "Oriental" = "Davao Oriental",
      "S.leyte" = "Southern Leyte", "Municipality Of Sarangani" = "Sarangani",
      "Saragani" = "Sarangani", "Saranggani" = "Sarangani",
      "South Cotobato" = "South Cotabato", "Sultan Kudaratl" = "Sultan Kudarat",
      "Surigao De Norte" = "Surigao del Norte", "Surigao Del Norte" = "Surigao del Norte",
      "Surigao de Sur" = "Surigao del Sur", "Surigao Sel Sur" = "Surigao del Sur",
      "Surigao Del Sur" = "Surigao del Sur", "Tawi-Tawi" = "Tawi-tawi",
      "Zambaoanga Del Norte" = "Zamboanga del Norte",
      "Zamboanga Del Norte" = "Zamboanga del Norte",
      "Zambonga Del Sur" = "Zamboanga del Sur",
      "Zamboanga Del Sur" = "Zamboanga del Sur"
    )

    earthquake_data <- earthquake_data %>%
      mutate(Province = dplyr::recode(Province, !!!corrections))

    # Select final columns
    earthquake_data <- earthquake_data %>%
      select(Date, Month, Day, Year, Time, Latitude, Longitude,
             `Depth_km`, Mag, Mag_scale, `Distance to Reference (km)`,
             `Direction to Reference`, City_Municipality, Province)

    removeModal()
    return(earthquake_data)
  }

  # Handle "Use Latest Data" button
  observeEvent(input$use_latest, {
    rv$latest_data <- scrape_phivolcs_data()
    rv$use_latest <- TRUE
    rv$earthquake_data <- rv$latest_data

    # Update filters
    years <- sort(unique(rv$earthquake_data$Year))
    updateCheckboxGroupInput(session, "year_filter",
                             choices = c("All", as.character(years)),
                             selected = "All")

    provinces <- sort(unique(rv$earthquake_data$Province[!is.na(rv$earthquake_data$Province)]))
    updateSelectizeInput(session, "province_filter", choices = provinces, selected = NULL)

    mag_range <- range(rv$earthquake_data$Mag, na.rm = TRUE)
    updateSliderInput(session, "mag_filter",
                      min = 1,
                      max = max(10, ceiling(mag_range[2])),
                      value = c(4, 8))

    showNotification("Latest data loaded successfully!", type = "message")
  })

  # Logic to manage the "All" selection in the year filter
  observe({
    selected_years <- input$year_filter

    # Condition 1: If specific years are selected along with "All", unselect "All"
    if (length(selected_years) > 1 && "All" %in% selected_years) {
      # Remove "All" from the selection
      new_selected <- selected_years[selected_years != "All"]
      updateCheckboxGroupInput(session, "year_filter", selected = new_selected)
    }

    # Condition 2: If nothing is selected (NULL or empty array), select "All"
    if (is.null(selected_years) || length(selected_years) == 0) {
      updateCheckboxGroupInput(session, "year_filter", selected = "All")
    }
  })

  # Filtered data
  filtered_data <- reactive({
    req(rv$earthquake_data)

    data <- rv$earthquake_data

    # Filter by year
    if (is.null(input$year_filter) || length(input$year_filter) == 0) {
      # If nothing is checked, return NULL (no plot)
      return(NULL)
    } else if (!"All" %in% input$year_filter) {
      # If specific years are selected (not "All"), filter by those years
      data <- data %>% filter(Year %in% as.integer(input$year_filter))
    }
    # If "All" is selected, use all data (no filtering needed)

    # Filter by province
    if (!is.null(input$province_filter) && length(input$province_filter) > 0) {
      data <- data %>% filter(Province %in% input$province_filter)
    }

    # Filter by magnitude
    data <- data %>% filter(Mag >= input$mag_filter[1] & Mag <= input$mag_filter[2])

    return(data)
  })

  # Load Philippines map
  ph_map <- ne_countries(scale = "large", country = "Philippines", returnclass = "sf")

  # Map plot
  output$map_plot <- renderPlot({
    req(filtered_data())

    eq_plot_data <- filtered_data() %>%
      rename(latitude = Latitude,
             longitude = Longitude,
             magnitude = Mag) %>%
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude),
             magnitude = as.numeric(magnitude)) %>%
      filter(!is.na(latitude) & !is.na(longitude) & !is.na(magnitude))

    min_mag <- min(eq_plot_data$magnitude, na.rm = TRUE)
    max_mag <- max(eq_plot_data$magnitude, na.rm = TRUE)
    max_label <- sprintf("%.1f (max)", max_mag)

    ggplot() +
      geom_sf(data = ph_map, fill = "white", color = "black", size = 0.4) +
      geom_point(data = eq_plot_data,
                 aes(x = longitude, y = latitude, color = magnitude, size = magnitude),
                 alpha = 0.7) +
      scale_color_gradientn(
        colors = c("blue", "green", "yellow", "orange", "red"),
        limits = c(min_mag, max_mag),
        breaks = c(min_mag, (min_mag + max_mag) / 2, max_mag),
        labels = c(sprintf("%.1f", min_mag),
                   sprintf("%.1f", (min_mag + max_mag) / 2),
                   max_label),
        name = paste0("Magnitude (", sprintf("%.1f", min_mag), "–", sprintf("%.1f", max_mag), ")")
      ) +
      guides(size = "none") +
      coord_sf(xlim = c(115, 132), ylim = c(0, 25)) +
      theme_minimal() +
      labs(
        title = "Earthquakes in the Philippines",
        subtitle = paste("Magnitude categories:", paste(levels(eq_plot_data$Mag_scale), collapse = ", ")),
        caption = paste0("Earthquake magnitude thresholds: ", sprintf("%.1f", min_mag), " to ", sprintf("%.1f", max_mag)),
        x = "Longitude (ºE)",
        y = "Latitude (ºN)"
      ) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10)
      )
  })

  # Daily trend plot
  output$daily_trend_plot <- renderPlot({
    req(filtered_data())

    daily_trend <- filtered_data() %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Date) %>%
      summarise(mean_mag = mean(Mag, na.rm = TRUE),
                max_mag = max(Mag, na.rm = TRUE),
                min_mag = min(Mag, na.rm = TRUE))

    ggplot(daily_trend, aes(x = Date)) +
      geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 0.8) +
      geom_point(aes(y = max_mag), color = "red", alpha = 0.6, size = 1.2) +
      geom_point(aes(y = min_mag), color = "darkgreen", alpha = 0.6, size = 1.2) +
      geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                  linewidth = 1.2, linetype = "longdash") +
      labs(
        title = "Daily Earthquake Magnitude Trends in the Philippines",
        subtitle = "Blue line = average/day, orange = trend, red = highest magnitude, green = lowest magnitude per day",
        x = "Year",
        y = "Magnitude"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })

  # Monthly trend plot
  output$monthly_trend_plot <- renderPlot({
    req(filtered_data())

    monthly_trend <- filtered_data() %>%
      mutate(Date = as.Date(Date),
             YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(mean_mag = mean(Mag, na.rm = TRUE),
                max_mag = max(Mag, na.rm = TRUE),
                min_mag = min(Mag, na.rm = TRUE))

    ggplot(monthly_trend, aes(x = YearMonth)) +
      geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 1) +
      geom_point(aes(y = max_mag), color = "red", size = 2, alpha = 0.8) +
      geom_point(aes(y = min_mag), color = "darkgreen", size = 2, alpha = 0.8) +
      geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                  linewidth = 1.2, linetype = "longdash") +
      labs(
        title = "Monthly Earthquake Magnitude Trends in the Philippines",
        subtitle = "Blue line = average/month, orange = trend, red = highest magnitude, green = lowest magnitude per month",
        x = "Year",
        y = "Magnitude"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })

  # Daily depth plot
  output$daily_depth_plot <- renderPlot({
    req(filtered_data())

    daily_depth <- filtered_data() %>%
      rename(depth_km = "Depth_km") %>%
      group_by(Date) %>%
      summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                min_depth = min(depth_km, na.rm = TRUE),
                max_depth = max(depth_km, na.rm = TRUE),
                avg_mag = mean(Mag, na.rm = TRUE),
                n_quakes = n())

    ggplot(daily_depth, aes(x = Date)) +
      geom_point(aes(y = avg_depth, color = avg_mag), size = 2, alpha = 0.7) +
      geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 1.5) +
      geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 1.5) +
      scale_y_reverse() +
      scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
      labs(
        title = "Daily Earthquake Depths in the Philippines",
        subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
        x = "Date",
        y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })

  # Monthly depth plot
  output$monthly_depth_plot <- renderPlot({
    req(filtered_data())

    monthly_depth <- filtered_data() %>%
      rename(depth_km = "Depth_km") %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                min_depth = min(depth_km, na.rm = TRUE),
                max_depth = max(depth_km, na.rm = TRUE),
                avg_mag = mean(Mag, na.rm = TRUE),
                n_quakes = n())

    ggplot(monthly_depth, aes(x = YearMonth)) +
      geom_point(aes(y = avg_depth, color = avg_mag), size = 3, alpha = 0.8) +
      geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 2) +
      geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 2) +
      scale_y_reverse() +
      scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
      labs(
        title = "Monthly Earthquake Depths in the Philippines",
        subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
        x = "Month",
        y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })

  # Frequency by province plot
  output$freq_province_plot <- renderPlot({
    req(filtered_data())

    daily_counts <- filtered_data() %>%
      filter(!is.na(Province)) %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Province, Date) %>%
      summarise(Daily_Count = n(), .groups = "drop")

    province_order <- daily_counts %>%
      group_by(Province) %>%
      summarise(Total = sum(Daily_Count, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      pull(Province)

    ggplot(daily_counts, aes(x = Daily_Count, y = factor(Province, levels = rev(province_order)))) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Earthquake Frequency by Province",
        # subtitle = "January 2017 to October 11, 2025, 5:00 PM",
        x = "Earthquake Frequency",
        y = "Province"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 25, scrollX = TRUE),
              filter = "top")
  })

  # Download handlers for data
  output$download_default <- downloadHandler(
    filename = function() {
      paste0("default_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(read.csv("default_data.csv"), file, row.names = FALSE)
    }
  )

  output$download_latest <- downloadHandler(
    filename = function() {
      paste0("latest_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$latest_data)
      write.csv(rv$latest_data, file, row.names = FALSE)
    }
  )

  # Download handlers for plots
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("earthquake_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      eq_plot_data <- filtered_data() %>%
        rename(latitude = Latitude,
               longitude = Longitude,
               magnitude = Mag) %>%
        mutate(latitude = as.numeric(latitude),
               longitude = as.numeric(longitude),
               magnitude = as.numeric(magnitude)) %>%
        filter(!is.na(latitude) & !is.na(longitude) & !is.na(magnitude))

      min_mag <- min(eq_plot_data$magnitude, na.rm = TRUE)
      max_mag <- max(eq_plot_data$magnitude, na.rm = TRUE)
      max_label <- sprintf("%.1f (max)", max_mag)

      p <- ggplot() +
        geom_sf(data = ph_map, fill = "white", color = "black", size = 0.4) +
        geom_point(data = eq_plot_data,
                   aes(x = longitude, y = latitude, color = magnitude, size = magnitude),
                   alpha = 0.7) +
        scale_color_gradientn(
          colors = c("blue", "green", "yellow", "orange", "red"),
          limits = c(min_mag, max_mag),
          breaks = c(min_mag, (min_mag + max_mag) / 2, max_mag),
          labels = c(sprintf("%.1f", min_mag),
                     sprintf("%.1f", (min_mag + max_mag) / 2),
                     max_label),
          name = paste0("Magnitude (", sprintf("%.1f", min_mag), "–", sprintf("%.1f", max_mag), ")")
        ) +
        guides(size = "none") +
        coord_sf(xlim = c(115, 132), ylim = c(0, 25)) +
        theme_minimal() +
        labs(
          title = "Earthquakes in the Philippines",
          subtitle = paste("Magnitude categories:", paste(levels(eq_plot_data$Mag_scale), collapse = ", ")),
          caption = paste0("Earthquake magnitude thresholds: ", sprintf("%.1f", min_mag), " to ", sprintf("%.1f", max_mag)),
          x = "Longitude (ºE)",
          y = "Latitude (ºN)"
        ) +
        theme(
          legend.position = "right",
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10)
        )

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  output$download_daily_trend <- downloadHandler(
    filename = function() {
      paste0("daily_magnitude_trend_", Sys.Date(), ".png")
    },
    content = function(file) {
      daily_trend <- filtered_data() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        summarise(mean_mag = mean(Mag, na.rm = TRUE),
                  max_mag = max(Mag, na.rm = TRUE),
                  min_mag = min(Mag, na.rm = TRUE))

      p <- ggplot(daily_trend, aes(x = Date)) +
        geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 0.8) +
        geom_point(aes(y = max_mag), color = "red", alpha = 0.6, size = 1.2) +
        geom_point(aes(y = min_mag), color = "darkgreen", alpha = 0.6, size = 1.2) +
        geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                    linewidth = 1.2, linetype = "longdash") +
        labs(
          title = "Daily Earthquake Magnitude Trends in the Philippines",
          subtitle = "Blue line = average/day, orange = trend, red = highest magnitude, green = lowest magnitude per day",
          x = "Year",
          y = "Magnitude"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  output$download_monthly_trend <- downloadHandler(
    filename = function() {
      paste0("monthly_magnitude_trend_", Sys.Date(), ".png")
    },
    content = function(file) {
      monthly_trend <- filtered_data() %>%
        mutate(Date = as.Date(Date),
               YearMonth = floor_date(Date, "month")) %>%
        group_by(YearMonth) %>%
        summarise(mean_mag = mean(Mag, na.rm = TRUE),
                  max_mag = max(Mag, na.rm = TRUE),
                  min_mag = min(Mag, na.rm = TRUE))

      p <- ggplot(monthly_trend, aes(x = YearMonth)) +
        geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 1) +
        geom_point(aes(y = max_mag), color = "red", size = 2, alpha = 0.8) +
        geom_point(aes(y = min_mag), color = "darkgreen", size = 2, alpha = 0.8) +
        geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                    linewidth = 1.2, linetype = "longdash") +
        labs(
          title = "Monthly Earthquake Magnitude Trends in the Philippines",
          subtitle = "Blue line = average/month, orange = trend, red = highest magnitude, green = lowest magnitude per month",
          x = "Year",
          y = "Magnitude"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  output$download_daily_depth <- downloadHandler(
    filename = function() {
      paste0("daily_depth_", Sys.Date(), ".png")
    },
    content = function(file) {
      daily_depth <- filtered_data() %>%
        rename(depth_km = "Depth_km") %>%
        group_by(Date) %>%
        summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                  min_depth = min(depth_km, na.rm = TRUE),
                  max_depth = max(depth_km, na.rm = TRUE),
                  avg_mag = mean(Mag, na.rm = TRUE),
                  n_quakes = n())

      p <- ggplot(daily_depth, aes(x = Date)) +
        geom_point(aes(y = avg_depth, color = avg_mag), size = 2, alpha = 0.7) +
        geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 1.5) +
        geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 1.5) +
        scale_y_reverse() +
        scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
        labs(
          title = "Daily Earthquake Depths in the Philippines",
          subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
          x = "Date",
          y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  output$download_monthly_depth <- downloadHandler(
    filename = function() {
      paste0("monthly_depth_", Sys.Date(), ".png")
    },
    content = function(file) {
      monthly_depth <- filtered_data() %>%
        rename(depth_km = "Depth_km") %>%
        mutate(YearMonth = floor_date(Date, "month")) %>%
        group_by(YearMonth) %>%
        summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                  min_depth = min(depth_km, na.rm = TRUE),
                  max_depth = max(depth_km, na.rm = TRUE),
                  avg_mag = mean(Mag, na.rm = TRUE),
                  n_quakes = n())

      p <- ggplot(monthly_depth, aes(x = YearMonth)) +
        geom_point(aes(y = avg_depth, color = avg_mag), size = 3, alpha = 0.8) +
        geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 2) +
        geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 2) +
        scale_y_reverse() +
        scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
        labs(
          title = "Monthly Earthquake Depths in the Philippines",
          subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
          x = "Month",
          y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  output$download_freq_province <- downloadHandler(
    filename = function() {
      paste0("frequency_by_province_", Sys.Date(), ".png")
    },
    content = function(file) {
      daily_counts <- filtered_data() %>%
        filter(!is.na(Province)) %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Province, Date) %>%
        summarise(Daily_Count = n(), .groups = "drop")

      province_order <- daily_counts %>%
        group_by(Province) %>%
        summarise(Total = sum(Daily_Count, na.rm = TRUE)) %>%
        arrange(desc(Total)) %>%
        pull(Province)

      p <- ggplot(daily_counts, aes(x = Daily_Count, y = factor(Province, levels = rev(province_order)))) +
        geom_col(fill = "steelblue") +
        labs(
          title = "Earthquake Frequency by Province",
          # subtitle = "January 2017 to October 11, 2025, 5:00 PM",
          x = "Earthquake Frequency",
          y = "Province"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())

      ggsave(file, plot = p, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
    }
  )

  # Download all plots as ZIP
  output$download_all_plots <- downloadHandler(
    filename = function() {
      paste0("all_earthquake_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()

      # Generate all plots
      plots <- list(
        list(name = "earthquake_map.png", data = filtered_data()),
        list(name = "daily_magnitude_trend.png", data = filtered_data()),
        list(name = "monthly_magnitude_trend.png", data = filtered_data()),
        list(name = "daily_depth.png", data = filtered_data()),
        list(name = "monthly_depth.png", data = filtered_data()),
        list(name = "frequency_by_province.png", data = filtered_data())
      )

      files_to_zip <- c()

      # Map plot
      eq_plot_data <- filtered_data() %>%
        rename(latitude = Latitude,
               longitude = Longitude,
               magnitude = Mag) %>%
        mutate(latitude = as.numeric(latitude),
               longitude = as.numeric(longitude),
               magnitude = as.numeric(magnitude)) %>%
        filter(!is.na(latitude) & !is.na(longitude) & !is.na(magnitude))

      min_mag <- min(eq_plot_data$magnitude, na.rm = TRUE)
      max_mag <- max(eq_plot_data$magnitude, na.rm = TRUE)
      max_label <- sprintf("%.1f (max)", max_mag)

      p1 <- ggplot() +
        geom_sf(data = ph_map, fill = "white", color = "black", size = 0.4) +
        geom_point(data = eq_plot_data,
                   aes(x = longitude, y = latitude, color = magnitude, size = magnitude),
                   alpha = 0.7) +
        scale_color_gradientn(
          colors = c("blue", "green", "yellow", "orange", "red"),
          limits = c(min_mag, max_mag),
          breaks = c(min_mag, (min_mag + max_mag) / 2, max_mag),
          labels = c(sprintf("%.1f", min_mag),
                     sprintf("%.1f", (min_mag + max_mag) / 2),
                     max_label),
          name = paste0("Magnitude (", sprintf("%.1f", min_mag), "–", sprintf("%.1f", max_mag), ")")
        ) +
        guides(size = "none") +
        coord_sf(xlim = c(115, 132), ylim = c(0, 25)) +
        theme_minimal() +
        labs(
          title = "Earthquakes in the Philippines",
          subtitle = paste("Magnitude categories:", paste(levels(eq_plot_data$Mag_scale), collapse = ", ")),
          caption = paste0("Earthquake magnitude thresholds: ", sprintf("%.1f", min_mag), " to ", sprintf("%.1f", max_mag)),
          x = "Longitude (ºE)",
          y = "Latitude (ºN)"
        ) +
        theme(
          legend.position = "right",
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10)
        )

      file1 <- file.path(temp_dir, "earthquake_map.png")
      ggsave(file1, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file1)

      # Daily trend
      daily_trend <- filtered_data() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        summarise(mean_mag = mean(Mag, na.rm = TRUE),
                  max_mag = max(Mag, na.rm = TRUE),
                  min_mag = min(Mag, na.rm = TRUE))

      p2 <- ggplot(daily_trend, aes(x = Date)) +
        geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 0.8) +
        geom_point(aes(y = max_mag), color = "red", alpha = 0.6, size = 1.2) +
        geom_point(aes(y = min_mag), color = "darkgreen", alpha = 0.6, size = 1.2) +
        geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                    linewidth = 1.2, linetype = "longdash") +
        labs(
          title = "Daily Earthquake Magnitude Trends in the Philippines",
          subtitle = "Blue line = average/day, orange = trend, red = highest magnitude, green = lowest magnitude per day",
          x = "Year",
          y = "Magnitude"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      file2 <- file.path(temp_dir, "daily_magnitude_trend.png")
      ggsave(file2, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file2)

      # Monthly trend
      monthly_trend <- filtered_data() %>%
        mutate(Date = as.Date(Date),
               YearMonth = floor_date(Date, "month")) %>%
        group_by(YearMonth) %>%
        summarise(mean_mag = mean(Mag, na.rm = TRUE),
                  max_mag = max(Mag, na.rm = TRUE),
                  min_mag = min(Mag, na.rm = TRUE))

      p3 <- ggplot(monthly_trend, aes(x = YearMonth)) +
        geom_line(aes(y = mean_mag), color = "steelblue", linewidth = 1) +
        geom_point(aes(y = max_mag), color = "red", size = 2, alpha = 0.8) +
        geom_point(aes(y = min_mag), color = "darkgreen", size = 2, alpha = 0.8) +
        geom_smooth(aes(y = mean_mag), color = "orange", se = FALSE,
                    linewidth = 1.2, linetype = "longdash") +
        labs(
          title = "Monthly Earthquake Magnitude Trends in the Philippines",
          subtitle = "Blue line = average/month, orange = trend, red = highest magnitude, green = lowest magnitude per month",
          x = "Year",
          y = "Magnitude"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      file3 <- file.path(temp_dir, "monthly_magnitude_trend.png")
      ggsave(file3, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file3)

      # Daily depth
      daily_depth <- filtered_data() %>%
        rename(depth_km = "Depth_km") %>%
        group_by(Date) %>%
        summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                  min_depth = min(depth_km, na.rm = TRUE),
                  max_depth = max(depth_km, na.rm = TRUE),
                  avg_mag = mean(Mag, na.rm = TRUE),
                  n_quakes = n())

      p4 <- ggplot(daily_depth, aes(x = Date)) +
        geom_point(aes(y = avg_depth, color = avg_mag), size = 2, alpha = 0.7) +
        geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 1.5) +
        geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 1.5) +
        scale_y_reverse() +
        scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
        labs(
          title = "Daily Earthquake Depths in the Philippines",
          subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
          x = "Date",
          y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      file4 <- file.path(temp_dir, "daily_depth.png")
      ggsave(file4, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file4)

      # Monthly depth
      monthly_depth <- filtered_data() %>%
        rename(depth_km = "Depth_km") %>%
        mutate(YearMonth = floor_date(Date, "month")) %>%
        group_by(YearMonth) %>%
        summarise(avg_depth = mean(depth_km, na.rm = TRUE),
                  min_depth = min(depth_km, na.rm = TRUE),
                  max_depth = max(depth_km, na.rm = TRUE),
                  avg_mag = mean(Mag, na.rm = TRUE),
                  n_quakes = n())

      p5 <- ggplot(monthly_depth, aes(x = YearMonth)) +
        geom_point(aes(y = avg_depth, color = avg_mag), size = 3, alpha = 0.8) +
        geom_point(aes(y = min_depth), shape = 25, fill = "blue", color = "blue", size = 2) +
        geom_point(aes(y = max_depth), shape = 24, fill = "red", color = "red", size = 2) +
        scale_y_reverse() +
        scale_color_gradient(low = "yellow", high = "red", name = "Avg Magnitude") +
        labs(
          title = "Monthly Earthquake Depths in the Philippines",
          subtitle = "Lower points indicate deeper depth of the earthquake's focus (hypocenter)",
          x = "Month",
          y = "Depth (km, inverted) [x-axis = 0 is to mimick Earth's surface]"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              axis.text.x = element_text(angle = 0, hjust = 1))

      file5 <- file.path(temp_dir, "monthly_depth.png")
      ggsave(file5, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file5)

      # Frequency by province
      daily_counts <- filtered_data() %>%
        filter(!is.na(Province)) %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Province, Date) %>%
        summarise(Daily_Count = n(), .groups = "drop")

      province_order <- daily_counts %>%
        group_by(Province) %>%
        summarise(Total = sum(Daily_Count, na.rm = TRUE)) %>%
        arrange(desc(Total)) %>%
        pull(Province)

      p6 <- ggplot(daily_counts, aes(x = Daily_Count, y = factor(Province, levels = rev(province_order)))) +
        geom_col(fill = "steelblue") +
        labs(
          title = "Earthquake Frequency by Province",
          # subtitle = "January 2017 to October 11, 2025, 5:00 PM",
          x = "Earthquake Frequency",
          y = "Province"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())

      file6 <- file.path(temp_dir, "frequency_by_province.png")
      ggsave(file6, plot = pX, width = input$plot_width, height = input$plot_height, dpi = input$plot_dpi, bg = "white")
      files_to_zip <- c(files_to_zip, file6)

      # Create ZIP file
      zip::zip(file, files = basename(files_to_zip), root = temp_dir)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
