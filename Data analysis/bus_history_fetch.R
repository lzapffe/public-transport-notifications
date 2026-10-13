# =============================================================================
# bus_history_fetch.R - run by hand on your OWN computer (not in GitHub).
#
# Downloads the history for the bus analysis, in three steps:
#   1. Entur's recorded bus times (Google BigQuery): every weekday bus on your
#      line that left your stop between 05:30 and 11:00, with its timetabled
#      and actual times at your stop and every stop after it.
#   2. Stop names (Entur's Journey Planner API), so the stretches between
#      stops can be named in the analysis.
#   3. Hourly weather for Oslo (Open-Meteo's archive, free, no key).
#
# Everything is saved in private/bus_history/, which Git ignores, so no data
# or stop IDs ever reach GitHub. Running it again only downloads the days you
# don't already have.
#
# Needs: private/bus_history_settings.json (see the setup steps) and the R
# packages bigrquery, httr2, jsonlite, dplyr and lubridate.
# Run it from the repository folder:  source("data-analysis/bus_history_fetch.R")
# =============================================================================

# Load the packages:
#   bigrquery - asks BigQuery questions (queries) and downloads the answers
#   httr2     - web requests to Entur and Open-Meteo
#   jsonlite  - reading the settings file and the web answers
#   dplyr     - working with the tables
#   lubridate - dates, times and the Oslo time zone
suppressPackageStartupMessages({
  library(bigrquery)
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
})

# "a %||% b" returns a, unless a is missing (NULL or empty), then it returns b.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Settings ---------------------------------------------------------------

# Your private settings (not in Git): your Google Cloud project ID, the line,
# and the stops to start and end at. Example:
#   {"gcp_project": "data-analysis-123456", "line": "RUT:Line:81",
#    "from": "NSR:StopPlace:6743", "to": "NSR:StopPlace:58404"}
SETTINGS_FILE <- "private/bus_history_settings.json"
if (!file.exists(SETTINGS_FILE)) stop("Can't find ", SETTINGS_FILE, ". See the setup steps.", call. = FALSE)
S <- fromJSON(SETTINGS_FILE)

# How far back to go, and the time window at your stop (Oslo time).
FIRST_DATE  <- as_date(S$first_date %||% (today() - years(2)))
LAST_DATE   <- today() - days(1)
TIME_FROM   <- S$time_from %||% "05:30:00"
TIME_TO     <- S$time_to   %||% "11:00:00"

# Safety limit: the script stops before running anything if the queries would
# read more than this many GB in total. BigQuery's free sandbox allows 1,000 GB
# (1 TB) of queries a month.
MAX_TOTAL_GB <- as.numeric(S$max_total_gb %||% 300)

# Where everything is saved (inside the ignored private folder).
OUT_DIR <- "private/bus_history"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
CALLS_FILE   <- file.path(OUT_DIR, "calls.rds")
QUAYS_FILE   <- file.path(OUT_DIR, "quays.rds")
WEATHER_FILE <- file.path(OUT_DIR, "weather.rds")

# Entur's public table of recorded real-time data in BigQuery.
TABLE <- "ent-data-sharing-ext-prd.realtime_siri_et.realtime_siri_et_last_recorded"

# The column names the script expects in that table. If Entur names a column
# differently, the script stops and lists the real names, and you can change
# the name on the right-hand side here.
COLS <- list(
  date        = "operatingDate",        # the service day
  line        = "lineRef",              # e.g. RUT:Line:81
  journey     = "serviceJourneyId",     # one bus trip
  seq         = "sequenceNr",           # the stop's number along the trip
  stop        = "stopPointRef",         # the platform (NSR:Quay:...)
  aimed_arr   = "aimedArrivalTime",     # timetabled arrival
  actual_arr  = "arrivalTime",          # recorded arrival
  aimed_dep   = "aimedDepartureTime",   # timetabled departure
  actual_dep  = "departureTime",        # recorded departure
  cancelled   = "journeyCancellation",  # the whole trip was cancelled (optional)
  stop_cancel = "stopCancellation"      # this stop was skipped (optional)
)
OPTIONAL_COLS <- c("cancelled", "stop_cancel")

# ---- Step 1a: check that BigQuery has the columns we need ---------------------

# Signs you in to Google the first time (a browser window opens), then
# remembers it. Reading the table's column list is free.
message("Checking Entur's BigQuery table...")
parts  <- strsplit(TABLE, ".", fixed = TRUE)[[1]]
fields <- bq_table_fields(bq_table(parts[1], parts[2], parts[3]))
have   <- vapply(fields, function(f) f$name, character(1))
missing <- setdiff(names(COLS), OPTIONAL_COLS)
missing <- missing[!unlist(COLS[missing]) %in% have]
if (length(missing)) {
  message("These columns weren't found: ", paste(unlist(COLS[missing]), collapse = ", "))
  message("The table has these columns:\n  ", paste(have, collapse = ", "))
  stop("Change the names in COLS at the top of the script (or send Claude the list above).", call. = FALSE)
}
for (k in OPTIONAL_COLS) if (!COLS[[k]] %in% have) COLS[[k]] <- NA

# The script handles the date column being a DATE, TIMESTAMP or text, and the
# time columns being TIMESTAMPs (Entur's usual types).
types <- setNames(vapply(fields, function(f) f$type, character(1)), have)
DATE_EXPR <- function(prefix = "") {
  col <- paste0(prefix, "`", COLS$date, "`")
  switch(types[[COLS$date]],
         DATE = col, TIMESTAMP = paste0("DATE(", col, ", 'Europe/Oslo')"),
         DATETIME = paste0("DATE(", col, ")"), STRING = paste0("SAFE.PARSE_DATE('%Y-%m-%d', ", col, ")"),
         stop("Unexpected type for ", COLS$date, ": ", types[[COLS$date]], call. = FALSE))
}
if (types[[COLS$aimed_dep]] != "TIMESTAMP") {
  stop("Expected ", COLS$aimed_dep, " to be a TIMESTAMP, but it's ", types[[COLS$aimed_dep]],
       ". Send Claude this message.", call. = FALSE)
}

# ---- Step 1b: find the platforms (quays) of your stop -----------------------

# BigQuery identifies stops by platform (NSR:Quay:...), so we ask Entur which
# platforms belong to your starting stop.
entur <- function(query, variables = list()) {
  request("https://api.entur.io/journey-planner/v3/graphql") |>
    req_headers("ET-Client-Name" = "private-bushistory") |>
    req_body_json(list(query = query, variables = variables), auto_unbox = TRUE) |>
    req_perform() |>
    resp_body_json()
}
j <- entur('query($id: String!) { stopPlace(id: $id) { quays { id } } }', list(id = S$from))
quays_from <- vapply(j$data$stopPlace$quays %||% list(), function(q) q$id, character(1))
if (!length(quays_from)) stop("Entur found no platforms for the 'from' stop in your settings.", call. = FALSE)

# Only allow IDs that look like Entur's, since they're put into the query text.
ok_id <- function(x) grepl("^[A-Z]{3}:[A-Za-z]+:[A-Za-z0-9_-]+$", x)
stopifnot(all(ok_id(quays_from)), ok_id(S$line))

# ---- Step 1c: build the query --------------------------------------------------

# For one period: first find the trips on your line that left one of your
# stop's platforms on a weekday between TIME_FROM and TIME_TO (Oslo time),
# then fetch those trips' times at your stop and every stop after it.
# Only the columns needed are read, which keeps the query small.
q <- function(x) paste0("`", x, "`")
col_or_null <- function(k) if (is.na(COLS[[k]])) paste0("NULL AS ", k) else paste0("t.", q(COLS[[k]]), " AS ", k)
build_query <- function(d1, d2) {
  sprintf("
WITH starts AS (
  SELECT %s AS journey, %s AS date, %s AS seq_from
  FROM %s
  WHERE %s BETWEEN DATE '%s' AND DATE '%s'
    AND %s = '%s'
    AND %s IN (%s)
    AND EXTRACT(DAYOFWEEK FROM %s) BETWEEN 2 AND 6
    AND TIME(DATETIME(%s, 'Europe/Oslo')) BETWEEN '%s' AND '%s'
)
SELECT %s AS date, t.%s AS journey, t.%s AS seq, t.%s AS stop,
       t.%s AS aimed_arr, t.%s AS actual_arr, t.%s AS aimed_dep, t.%s AS actual_dep,
       %s, %s, s.seq_from
FROM %s AS t
JOIN starts AS s ON t.%s = s.journey AND %s = s.date
WHERE %s BETWEEN DATE '%s' AND DATE '%s'
  AND t.%s = '%s'
  AND t.%s >= s.seq_from",
    q(COLS$journey), DATE_EXPR(), q(COLS$seq),
    q(TABLE),
    DATE_EXPR(), d1, d2,
    q(COLS$line), S$line,
    q(COLS$stop), paste0("'", quays_from, "'", collapse = ", "),
    DATE_EXPR(),
    q(COLS$aimed_dep), TIME_FROM, TIME_TO,
    DATE_EXPR("t."), q(COLS$journey), q(COLS$seq), q(COLS$stop),
    q(COLS$aimed_arr), q(COLS$actual_arr), q(COLS$aimed_dep), q(COLS$actual_dep),
    col_or_null("cancelled"), col_or_null("stop_cancel"),
    q(TABLE),
    q(COLS$journey), DATE_EXPR("t."),
    DATE_EXPR("t."), d1, d2,
    q(COLS$line), S$line,
    q(COLS$seq))
}

# ---- Step 1d: work out which months are still missing ----------------------

# Months already downloaded are skipped, except the last one, which is
# fetched again to fill in days that were missing last time.
old <- if (file.exists(CALLS_FILE)) readRDS(CALLS_FILE) else NULL
start_from <- if (!is.null(old) && nrow(old)) max(as_date(old$date)) - days(7) else FIRST_DATE
if (start_from > LAST_DATE) {
  message("Bus data is already up to date.")
  periods <- list()
} else {
  m_starts <- seq(floor_date(start_from, "month"), floor_date(LAST_DATE, "month"), by = "1 month")
  periods  <- lapply(m_starts, function(m) c(max(m, start_from), min(ceiling_date(m + days(1), "month") - days(1), LAST_DATE)))
}

# ---- Step 1e: estimate the size first (free), then ask before running -----------

# A "dry run" tells how much data a query would read, without running it or
# using any quota. The total is compared with MAX_TOTAL_GB.
if (length(periods)) {
  message("Estimating the size of ", length(periods), " monthly queries (free)...")
  gb <- vapply(periods, function(p) {
    bq_perform_query_dry_run(build_query(p[1], p[2]), billing = S$gcp_project) / 1e9
  }, numeric(1))
  message(sprintf("They would read about %.1f GB in total (the free sandbox allows 1,000 GB a month).", sum(gb)))
  if (sum(gb) > MAX_TOTAL_GB) {
    stop(sprintf("That's more than the safety limit of %d GB. Try a later first_date in the settings, or raise max_total_gb.",
                 as.integer(MAX_TOTAL_GB)), call. = FALSE)
  }
  answer <- if (interactive()) readline("Run the queries now? (y/n) ") else "y"
  if (!tolower(substr(answer, 1, 1)) %in% "y") stop("Stopped; nothing was run.", call. = FALSE)

  # ---- Step 1f: run the queries month by month and save after each --------------

  # Saving after every month means an interruption loses at most one month.
  for (i in seq_along(periods)) {
    p <- periods[[i]]
    message(sprintf("Month %d of %d: %s to %s ...", i, length(periods), p[1], p[2]))
    tb  <- bq_project_query(S$gcp_project, build_query(p[1], p[2]))
    new <- bq_table_download(tb, quiet = TRUE)
    new$date <- as_date(new$date)
    # Replace this month's days in the saved data (on the first run there is none yet).
    if (!is.null(old)) old <- old |> filter(!(date >= p[1] & date <= p[2]))
    old <- bind_rows(old, new)
    saveRDS(old, CALLS_FILE)
    message(sprintf("  %d rows, %d bus trips.", nrow(new), n_distinct(paste(new$date, new$journey))))
  }
}

# ---- Step 2: stop names and which stop each platform belongs to ---------------

# Look up every platform that appears in the data (in batches of 100), so the
# analysis can find your end stop and name the stretches between stops.
calls <- readRDS(CALLS_FILE)
need  <- unique(calls$stop)
known <- if (file.exists(QUAYS_FILE)) readRDS(QUAYS_FILE) else tibble(stop = character())
need  <- setdiff(need[ok_id(need)], known$stop)
if (length(need)) {
  message("Looking up ", length(need), " platform names at Entur...")
  batches <- split(need, ceiling(seq_along(need) / 100))
  found <- bind_rows(lapply(batches, function(ids) {
    j <- entur('query($ids: [String]) { quays(ids: $ids) { id name stopPlace { id name parent { id } } } }',
               list(ids = I(ids)))
    bind_rows(lapply(j$data$quays %||% list(), function(x) {
      if (is.null(x)) return(NULL)
      tibble(stop = x$id, stop_name = x$stopPlace$name %||% x$name %||% x$id,
             stop_place = x$stopPlace$id %||% NA_character_,
             parent = x$stopPlace$parent$id %||% NA_character_)
    }))
  }))
  known <- bind_rows(known, found)
  saveRDS(known, QUAYS_FILE)
}

# ---- Step 3: hourly weather for Oslo ------------------------------------------

# Open-Meteo's historical weather (free, no key) for central Oslo: temperature,
# rain, snowfall and snow depth, hour by hour, in Oslo time. The archive lags
# a few days behind, so the most recent days may be missing until next time.
w_old   <- if (file.exists(WEATHER_FILE)) readRDS(WEATHER_FILE) else NULL
w_start <- if (!is.null(w_old) && nrow(w_old)) max(as_date(w_old$time)) - days(7) else FIRST_DATE - days(1)
message("Fetching weather from ", w_start, " to ", LAST_DATE, "...")
resp <- request("https://archive-api.open-meteo.com/v1/archive") |>
  req_url_query(latitude = 59.91, longitude = 10.75,
                start_date = format(w_start), end_date = format(LAST_DATE),
                hourly = "temperature_2m,precipitation,rain,snowfall,snow_depth",
                timezone = "Europe/Oslo") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)
h <- resp$hourly
weather <- tibble(
  time        = ymd_hm(h$time, tz = "Europe/Oslo"),
  temp        = as.numeric(h$temperature_2m),
  precip      = as.numeric(h$precipitation),
  rain        = as.numeric(h$rain),
  snowfall    = as.numeric(h$snowfall),
  snow_depth  = as.numeric(h$snow_depth)
) |> filter(!is.na(temp), !is.na(time))   # drops the hour skipped when clocks go forward
weather <- bind_rows(if (!is.null(w_old)) filter(w_old, time < min(weather$time)), weather)
saveRDS(weather, WEATHER_FILE)

# ---- Done ---------------------------------------------------------------------

message(sprintf("Done. %d bus trips from %s to %s, and %d hours of weather, saved in %s.",
                n_distinct(paste(calls$date, calls$journey)), min(calls$date), max(calls$date),
                nrow(weather), OUT_DIR))
message("Next: source(\"data-analysis/bus_history_analysis.R\")")
