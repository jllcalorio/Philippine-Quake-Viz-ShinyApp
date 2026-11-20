# =============================================================================
# Smoke test: does the app start and draw with the current default_data.csv?
# Run from the project folder:  Rscript scripts/check_app.R
# Used by the monthly workflow before deploying; exits with status 1 on failure.
# =============================================================================

suppressPackageStartupMessages(library(shiny))

ok <- tryCatch({
  testServer(".", {
    d <- rv$data
    stopifnot(nrow(d) > 100000, !anyNA(d$Date), !anyNA(d$Mag))
    session$setInputs(
      date_range = range(d$Date), mag_range = c(0, 10),
      island_group = c("Luzon", "Visayas", "Mindanao"), province_filter = NULL,
      map_color = "magnitude", trend_var = "count", trend_unit = "month",
      trend_separate = TRUE, top_n = 20, plot_width = 11, plot_height = 7,
      plot_dpi = 96, quick_period = ""
    )
    stopifnot(nrow(filtered()) > 0)
    for (p in list(map_plot(), trend_plot(), province_plot(), mag_dist(), depth_dist())) {
      stopifnot(inherits(p, "ggplot"))
      ggplot2::ggplot_build(p)
    }
    cat(sprintf("App check passed: %s events, %s to %s.\n",
                format(nrow(d), big.mark = ","), min(d$Date), max(d$Date)))
  })
  TRUE
}, error = function(e) {
  cat("App check FAILED:", conditionMessage(e), "\n")
  FALSE
})

if (!ok) quit(save = "no", status = 1)
