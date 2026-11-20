# Philippine Earthquake Explorer

An interactive Shiny app for exploring every earthquake listed by the
**Philippine Institute of Volcanology and Seismology (PHIVOLCS-DOST)** from
January 2017 onwards: where earthquakes occur, how large and deep they are,
how activity changes over time, and which provinces record the most events.

**Open the app:** <https://jllcalorio.shinyapps.io/philippine-quake-viz-shinyapp/>

> **Not for emergency use.** This is an independent educational and research
> tool, not an official PHIVOLCS product. For official earthquake information,
> advisories and tsunami warnings, go to <https://earthquake.phivolcs.dost.gov.ph/>.

---

## Features

| Tab | What it shows |
|---|---|
| **Overview** | Headline figures (number of events, largest magnitude, events of M 5.0 or higher, median depth), magnitude and depth distributions, summary by year |
| **Map** | Epicentres sized by magnitude and coloured by magnitude or depth. Click a circle to see event details |
| **Trends** | Number of events, magnitude or depth by week, month, quarter or year |
| **Provinces** | Provinces ranked by number of events, plus a sortable summary table |
| **Data** | Every record, searchable. Download the filtered or full data as CSV |
| **About & methods** | Definitions, data processing, limitations, citations |

- **One set of filters** (date range, magnitude, island group, province) drives every tab.
- **PNG download** on every chart, with adjustable size and resolution.
- **Updated every month:** the bundled data is refreshed automatically a few
  days after each month ends (see [Automatic monthly updates](#automatic-monthly-updates)).
- **Live update:** *Add latest events from PHIVOLCS* adds the events published
  since then and runs them through the same cleaning steps. Only your session is updated.

## Data

### Source

Earthquake records are scraped from the PHIVOLCS earthquake information pages:

- Latest events: <https://earthquake.phivolcs.dost.gov.ph/>
- Monthly archives: `https://earthquake.phivolcs.dost.gov.ph/EQLatest-Monthly/<YYYY>/<YYYY>_<Month>.html`

The bundled file `default_data.csv` starts on **1 January 2017** and is extended
automatically after each month ends. The Philippine coastline
(`ph_map.rds`) is from [Natural Earth](https://www.naturalearthdata.com/)
(1:10m Admin 0 countries), which is in the public domain.

### Cleaning

`R/data_prep.R` applies the same documented steps to the bundled data and to
any newly downloaded events. The app reports the effect of each step on the
*About & methods* tab. Because the current `default_data.csv` was exported from
the app, it is already cleaned and most steps report 0 there. The counts below
are from the original scrape (1 January 2017 to 27 June 2026, 133,992 raw rows):

| Step | Records | Action |
|---|---:|---|
| Time of day not recorded | 8 | Kept |
| Exact duplicate records | 54 | Removed |
| Province name corrected | 7,555 | Corrected |
| Depth placeholder `0.00001` km (originally `--` or `<001`) | 19 | Set to missing |
| Depth deeper than 750 km (physically implausible) | 2 | Set to missing |
| Coordinates outside the Philippine region | 4 | Kept, not mapped |
| Coordinates more than 300 km from the stated reference location | 15 | Kept, not mapped |

Province corrections include:

- **typing errors**, e.g. *Batanagas* and *Eastren Samar*
- **renamed provinces**: *Compostela Valley* became *Davao de Oro* in 2019; *North Cotabato* is officially *Cotabato*; *Western Samar* is *Samar*
- **towns or old municipality names stored as the province** by an earlier
  scraper, which took the wrong pair of brackets. For example, *Fuga Island
  (Aparri) (Cagayan)* had been stored as province *Aparri*. Most notably,
  **4,054 events at Balut Island and Sarangani Island were assigned to
  Sarangani province**. These islands belong to the *Municipality* of
  Sarangani, **Davao Occidental**.

### Definitions

- **Magnitude** is the size of the earthquake at its source, as published by
  PHIVOLCS. Classes: Micro < 2.0, Minor 2.0–3.9, Light 4.0–4.9, Moderate 5.0–5.9,
  Strong 6.0–6.9, Major 7.0–7.9, Great ≥ 8.0.
  *Magnitude is not intensity.* The PHIVOLCS Earthquake Intensity Scale (PEIS,
  I to X) describes shaking felt at a place, so it is not used to label magnitudes.
- **Depth classes:** shallow < 70 km, intermediate 70–300 km, deep > 300 km.
- **Province** is the province of the reference town PHIVOLCS uses to describe
  the epicentre (e.g. *13 km N 37° E of Bayabas (Surigao del Sur)*). For offshore
  events this is the nearest reference town's province.
- **Times** are Philippine Standard Time (UTC+8).

### Download columns

| Column | Description |
|---|---|
| `Date` | Event date, `YYYY-MM-DD` (PhST) |
| `Time_PhST` | Event time, 24-hour clock (PhST); blank if not recorded |
| `Latitude_N`, `Longitude_E` | Epicentre coordinates, decimal degrees |
| `Coordinates_valid` | `FALSE` if the coordinates failed the plausibility checks |
| `Depth_km`, `Depth_class` | Hypocentre depth and class; blank if not reported or implausible |
| `Magnitude`, `Magnitude_class` | Magnitude as published, and its class |
| `Distance_to_reference_km`, `Direction_to_reference`, `Reference_town` | PHIVOLCS's location description |
| `Reference_province`, `Island_group` | Cleaned province and its island group (Luzon / Visayas / Mindanao) |
| `Source` | Where the record came from: the original bundled data set, a PHIVOLCS monthly archive page, or the PHIVOLCS website (latest events), with retrieval date |

### Limitations

- **Detection improves over time.** More stations record more small events, so
  rising counts of small earthquakes do not necessarily mean more earthquakes.
  Compare years using larger events (e.g. M 4.0 and above).
- **Aftershock sequences** can dominate monthly counts and province rankings.
- **Recent values are preliminary** and may be revised by PHIVOLCS.
- Events PHIVOLCS lists only as *Taiwan Region* are excluded.
- The automatic checks catch obvious errors only. Some misplaced records may remain.

## Running locally

Requires R (4.5 or later recommended).

```r
# In the project folder. renv restores the exact package versions in renv.lock.
install.packages("renv")
renv::restore()
shiny::runApp()
```

### Project structure

```
app.R              User interface and server logic
R/data_prep.R      Reading, cleaning and classifying the data
R/scrape.R         Downloading the latest events from PHIVOLCS
R/plots.R          Charts and summary tables
R/about.R          "About & methods" page and package citations
default_data.csv   Bundled earthquake data (updated monthly by the workflow)
scripts/           refresh_data.R (monthly update), check_app.R (smoke test),
                   deploy.R (deploy to shinyapps.io)
.github/workflows/monthly-refresh.yml   The monthly update job
ph_map.rds         Philippine coastline (sf object, Natural Earth)
renv.lock          Package versions
```

Shiny sources the files in `R/` automatically, **before** `app.R` attaches
packages. Top-level code in `R/` must therefore use base R only; package
functions are fine inside function bodies.

### Deploying to shinyapps.io

The monthly workflow deploys automatically. To deploy by hand, use RStudio's
**Publish** button, or:

```r
rsconnect::deployApp(
  appFiles = c("app.R", list.files("R", full.names = TRUE),
               "default_data.csv", "ph_map.rds", "renv.lock"),
  appName = "philippine-quake-viz-shinyapp"
)
```

## Automatic monthly updates

A GitHub Actions workflow ([`.github/workflows/monthly-refresh.yml`](.github/workflows/monthly-refresh.yml))
keeps `default_data.csv` up to date. On the 2nd, 4th, 6th, 8th and 10th of each
month at 09:00 Philippine time, it:

1. downloads the month that just ended from the PHIVOLCS **monthly archive**
   page (PHIVOLCS's consolidated listing, which replaces the preliminary values
   from the latest-events page);
2. **checks the page** before using it: at least 150 events, the whole month
   covered, and dates, times, magnitudes, coordinates, depths and provinces all
   readable. If PHIVOLCS changes its page format, these checks fail;
3. re-runs the cleaning steps and checks that the app still starts and draws
   every chart (`scripts/check_app.R`);
4. commits the new `default_data.csv` to `main` and redeploys the app.

PHIVOLCS publishes a month's archive a few days after the month ends, so the
early runs may find nothing yet and simply stop. Once the data is updated, the
remaining runs find no change and stop too. **If anything fails, nothing is
changed**: the app keeps running on the previous data and GitHub e-mails you.
The run's page (*Actions* tab) explains what went wrong.

You can also run it by hand: **Actions → Monthly data refresh → Run workflow**.
Tick *Redeploy the app* to deploy code changes without waiting for new data.
To check the data on your own computer without changing anything, run
`Rscript scripts/refresh_data.R --dry-run`.

### One-time setup

1. On <https://www.shinyapps.io>, open **Account → Tokens**, click **Show** on
   a token (or **Add Token**), then **Show secret**. Note the account name,
   token and secret.
2. On GitHub, open the repository's **Settings → Secrets and variables →
   Actions → New repository secret** and add three secrets:
   `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`.
3. *(Optional)* To deploy to a different app name, add a repository
   **variable** (same page, *Variables* tab) named `SHINYAPPS_APP_NAME`.
   The default is `philippine-quake-viz-shinyapp`.
4. Run the workflow once by hand (with *Redeploy the app* ticked) to confirm it works.

Without the secrets, the workflow still updates the data in the repository but
does not deploy.

### Working on the app after this is set up

The workflow commits to `main`, so your local copy falls behind after each
monthly update. **Pull before you start working** (`git pull`, or *Pull* in
GitHub Desktop / RStudio). Do not edit `default_data.csv` by hand; if you have
uncommitted changes to it, `git pull` will refuse to run until you discard
them (`git checkout -- default_data.csv`).

## How to cite

> Calorio, J. L. L. (2026). *Philippine Earthquake Explorer* [Shiny web application].
> <https://jllcalorio.shinyapps.io/philippine-quake-viz-shinyapp/>. Source code:
> <https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp>

Please also cite the data source:

> Philippine Institute of Volcanology and Seismology (PHIVOLCS-DOST).
> *Earthquake Information.* <https://earthquake.phivolcs.dost.gov.ph/>

Software: R (R Core Team), shiny, bslib, dplyr, stringr, lubridate, rvest,
ggplot2, sf, DT and scales. Full citations for the installed versions are shown
on the app's *About & methods* tab.

## Author

**John Lennon L. Calorio**, Health Data Scientist and Statistician.
Senior Statistician, Center for Research and Development, Davao Medical School
Foundation, Inc. Email: jllcalorio@gmail.com

Bug reports and suggestions are welcome via
[GitHub issues](https://github.com/jllcalorio/Philippine-Quake-Viz-ShinyApp/issues).

## License

[MIT](LICENSE). Earthquake data © PHIVOLCS-DOST. Map outline: Natural Earth (public domain).
