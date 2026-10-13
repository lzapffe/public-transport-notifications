# =============================================================================
# bus_history_analysis.R - run by hand on your OWN computer, after
# bus_history_fetch.R. Works only on the files that script saved, so you can
# rerun it as often as you like without using any BigQuery quota.
#
# Answers nine questions about the bus from your stop to your end stop on
# weekday mornings (05:30-11:00):
#   1. How long does the ride take for each 5-minute departure slot, normally
#      and on a bad day (1 in 10)?
#   2. What's the latest bus that gets me there by a given time on 9 of 10 days?
#   3. When does the rush start, peak and ease off?
#   4. Is the lost time spent waiting at my stop, or on the ride itself?
#   5. Do weekdays, months or school holidays differ?
#   6. How much do rain, snowfall, snow on the ground and frost add?
#   7. How often are buses cancelled or missing recorded times, and when?
#   8. How do delays change with temperature (5 °C bins), around 0 °C, and
#      after a drop through zero overnight?
#   9. Where along the route is the time lost (each stretch between stops), and
#      at what time of the morning?
#
# Results (CSV files you can open in Excel, and PNG plots) are saved in
# private/bus_history/output/, which Git ignores. A short summary is printed.
# Run it from the repository folder:  source("data-analysis/bus_history_analysis.R")
# =============================================================================

# Load the packages:
#   dplyr, tidyr - working with the tables
#   lubridate    - dates, times and the Oslo time zone
#   ggplot2      - the plots
#   readr        - writing the CSV files
#   jsonlite     - reading the settings file
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(jsonlite)
})

# "a %||% b" returns a, unless a is missing (NULL or empty), then it returns b.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Settings ---------------------------------------------------------------

# The same private settings file as the fetch script (for the stop IDs).
S  <- fromJSON("private/bus_history_settings.json")
TZ <- "Europe/Oslo"

# "Recent" results use only the last RECENT_DAYS days, since timetables and
# roadworks change; everything is also shown for the whole period.
RECENT_DAYS <- as.numeric(S$recent_days %||% 90)

# A ride counts as "slow" when it's at least this many minutes over the timetable.
SLOW_MIN <- 5

# Arrival times to check in question 2 (every 15 minutes).
TARGETS <- format(seq(ymd_hm("2000-01-01 06:30"), ymd_hm("2000-01-01 11:30"), by = "15 min"), "%H:%M")

# Where the data is, and where the results go.
IN_DIR  <- "private/bus_history"
OUT_DIR <- file.path(IN_DIR, "output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# save_csv() / save_png(): write a result table or plot to the output folder.
save_csv <- function(x, name) write_excel_csv(x, file.path(OUT_DIR, paste0(name, ".csv")))
save_png <- function(p, name, w = 9, h = 5) ggsave(file.path(OUT_DIR, paste0(name, ".png")), p, width = w, height = h, dpi = 120)

# ---- Load the data ------------------------------------------------------------

# The bus calls (one row per bus per stop), the platform lookup and the weather.
calls   <- readRDS(file.path(IN_DIR, "calls.rds"))
quays   <- readRDS(file.path(IN_DIR, "quays.rds"))
weather <- readRDS(file.path(IN_DIR, "weather.rds"))

# Times in Oslo time, and each platform matched to its stop (and the stop's
# parent, e.g. a hub that groups the bus and metro stops).
calls <- calls |>
  mutate(across(c(aimed_arr, actual_arr, aimed_dep, actual_dep), ~ with_tz(as_datetime(.x), TZ)),
         date = as_date(date)) |>
  left_join(quays, by = "stop") |>
  mutate(stop_name = coalesce(stop_name, stop),
         is_to = stop == S$to | stop_place %in% S$to | parent %in% S$to,
         # One time per stop: departure if there is one, else arrival.
         aimed  = coalesce(aimed_dep, aimed_arr),
         actual = coalesce(actual_dep, actual_arr))

# ---- One row per bus trip ----------------------------------------------------------

# For each trip: the row at your stop (where the trip was found) and the first
# later row at your end stop. Trips that never reach the end stop (the other
# direction, or a shortened trip) are left out.
from_rows <- calls |> filter(seq == seq_from) |>
  select(date, journey, dep_aimed = aimed_dep, dep_actual = actual_dep,
         cancelled, stop_cancel)
to_rows <- calls |> filter(is_to, seq > seq_from) |>
  group_by(date, journey) |> slice_min(seq, n = 1, with_ties = FALSE) |> ungroup() |>
  select(date, journey, arr_aimed = aimed_arr, arr_actual = actual_arr, to_cancel = stop_cancel)

trips <- inner_join(from_rows, to_rows, by = c("date", "journey")) |>
  mutate(
    # Was the trip cancelled, or does it lack recorded times?
    is_cancelled = coalesce(as.logical(cancelled), FALSE) | coalesce(as.logical(stop_cancel), FALSE),
    has_times    = !is.na(dep_actual) & !is.na(arr_actual),
    # The departure slot (5 minutes) and half hour, from the timetable.
    slot   = floor_date(dep_aimed, "5 min"),
    slot_t = format(slot, "%H:%M"),
    half   = format(floor_date(dep_aimed, "30 min"), "%H:%M"),
    # Minutes: late at your stop, ride time, timetabled ride time, and total
    # from the timetabled departure to the actual arrival.
    late_at_stop = as.numeric(difftime(dep_actual, dep_aimed, units = "mins")),
    ride         = as.numeric(difftime(arr_actual, dep_actual, units = "mins")),
    ride_planned = as.numeric(difftime(arr_aimed, dep_aimed, units = "mins")),
    ride_extra   = ride - ride_planned,
    total        = as.numeric(difftime(arr_actual, dep_aimed, units = "mins")),
    total_extra  = total - ride_planned,
    weekday = wday(date, label = TRUE, week_start = 1, abbr = FALSE, locale = "C"),
    month   = month(date, label = TRUE, abbr = TRUE, locale = "C"),
    recent  = date > max(date) - days(RECENT_DAYS)
  )

# Trips with usable times: recorded, not cancelled, and not obviously wrong
# (e.g. a negative ride, or one more than an hour over the timetable).
ok <- trips |>
  filter(has_times, !is_cancelled, ride > 0.5 * ride_planned, ride_extra < 60, abs(late_at_stop) < 60)

# ---- School holidays and public holidays -----------------------------------------

# easter(): Easter Sunday for a year (the standard "anonymous Gregorian" method).
easter <- function(y) {
  a <- y %% 19; b <- y %/% 100; c <- y %% 100; d <- b %/% 4; e <- b %% 4
  f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3; h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4; l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  make_date(y, (h + l - 7 * m + 114) %/% 31, ((h + l - 7 * m + 114) %% 31) + 1)
}

# holiday_type(): "Public holiday", "School holiday" or "Normal" for each date.
# School holidays are APPROXIMATE for Oslo (winter break week 8, autumn break
# week 40, Easter week, Christmas, summer 21 June-17 August). Adjust here if needed.
holiday_type <- function(d) {
  y  <- year(d); e <- easter(y)
  public <- format(d, "%m-%d") %in% c("01-01", "05-01", "05-17", "12-25", "12-26") |
    d %in% c(e - 3, e - 2, e + 1, e + 39, e + 50)
  school <- isoweek(d) %in% c(8, 40) | (d >= e - 6 & d <= e + 1) |
    format(d, "%m-%d") >= "12-21" | format(d, "%m-%d") <= "01-02" |
    (format(d, "%m-%d") >= "06-21" & format(d, "%m-%d") <= "08-17")
  case_when(public ~ "Public holiday", school ~ "School holiday", TRUE ~ "Normal")
}
ok <- ok |> mutate(holiday = holiday_type(date))

# ---- Weather for each trip ----------------------------------------------------------

# Hourly weather in Oslo time, and per-morning summaries:
#   temp       - temperature in the hour the bus left
#   precip_am  - rain/sleet/snow (mm) from 05:00 to 11:00
#   snow_am    - snowfall (cm) from 05:00 to 11:00
#   snow_depth - snow on the ground (cm) at 06:00
#   evening_temp / night_min - 18:00 the day before, and the lowest from 00 to 08
#   night_precip - precipitation from 18:00 the day before to 06:00
weather <- weather |> mutate(time = with_tz(time, TZ), date = as_date(time), hr = hour(time))
morning <- weather |>
  group_by(date) |>
  summarise(precip_am  = sum(precip[hr >= 5 & hr < 11], na.rm = TRUE),
            snow_am    = sum(snowfall[hr >= 5 & hr < 11], na.rm = TRUE),
            snow_depth = 100 * first(snow_depth[hr == 6]),   # metres -> cm (NA if missing)
            night_min  = min(temp[hr <= 8], na.rm = TRUE),
            .groups = "drop")
evening_before <- weather |> filter(hr == 18) |>
  transmute(date = date + days(1), evening_temp = temp)
night_precip <- weather |>
  mutate(night = if_else(hr >= 18, date + days(1), date)) |>
  filter(hr >= 18 | hr < 6) |>
  group_by(date = night) |> summarise(night_precip = sum(precip, na.rm = TRUE), .groups = "drop")
hourly <- weather |> transmute(hour_time = floor_date(time, "hour"), temp)

ok <- ok |>
  mutate(hour_time = floor_date(dep_aimed, "hour")) |>
  left_join(hourly, by = "hour_time") |>
  left_join(morning, by = "date") |>
  left_join(evening_before, by = "date") |>
  left_join(night_precip, by = "date") |>
  mutate(
    rainy        = precip_am >= 1,
    snowing      = snow_am >= 0.5,
    snow_ground  = coalesce(snow_depth, 0) >= 5,
    frost        = temp < 0,
    temp_bin     = cut(temp, breaks = seq(-30, 35, 5), right = FALSE,
                       labels = paste(seq(-30, 30, 5), "to", seq(-25, 35, 5))),
    # "Above +3 °C" is listed first so the model below compares against it.
    near_zero    = factor(case_when(temp < -3 ~ "Below -3 °C", temp <= 3 ~ "-3 to +3 °C", temp > 3 ~ "Above +3 °C"),
                          levels = c("Above +3 °C", "-3 to +3 °C", "Below -3 °C")),
    drop_through_zero = evening_temp > 0 & night_min <= 0,
    rain_then_frost   = night_precip >= 1 & temp <= 0
  )

# summarise_extra(): the standard summary used in several tables: number of
# trips and mornings, average and median extra minutes, and the share of slow trips.
summarise_extra <- function(d) {
  d |> summarise(trips = n(), mornings = n_distinct(date),
                 avg_extra_min = round(mean(total_extra), 1),
                 median_extra_min = round(median(total_extra), 1),
                 share_slow = round(mean(total_extra >= SLOW_MIN), 2),
                 .groups = "drop")
}

# ---- 1 and 3. Ride time per 5-minute slot ---------------------------------------------

# For each slot: the median and bad-day (90th percentile) time from the
# timetabled departure to arrival, the share of slow trips, and the typical
# and bad-day arrival times. Done for the whole period and the recent days.
per_slot <- function(d, label) {
  d |> group_by(slot_t) |>
    summarise(period = label, trips = n(),
              planned_min = round(median(ride_planned), 1),
              median_min  = round(median(total), 1),
              bad_day_min = round(quantile(total, 0.9), 1),
              share_slow  = round(mean(total_extra >= SLOW_MIN), 2),
              .groups = "drop") |>
    mutate(slot_start   = ymd_hm(paste("2000-01-01", slot_t)),
           arrive_typical = format(slot_start + dminutes(median_min), "%H:%M"),
           arrive_bad_day = format(slot_start + dminutes(bad_day_min), "%H:%M")) |>
    select(-slot_start)
}
q1 <- bind_rows(per_slot(ok, "All"), per_slot(filter(ok, recent), paste("Last", RECENT_DAYS, "days")))
save_csv(q1, "1_ride_time_per_slot")

p1 <- q1 |> filter(trips >= 5) |>
  pivot_longer(c(planned_min, median_min, bad_day_min), names_to = "measure", values_to = "minutes") |>
  mutate(measure = recode(measure, planned_min = "Timetable", median_min = "Typical", bad_day_min = "Bad day (1 in 10)"),
         time = ymd_hm(paste("2000-01-01", slot_t))) |>
  ggplot(aes(time, minutes, colour = measure)) + geom_line(linewidth = 0.8) +
  facet_wrap(~period, ncol = 1) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "30 min") +
  labs(title = "Minutes from the timetabled departure at your stop to arrival",
       x = "Timetabled departure from your stop", y = "Minutes", colour = NULL) +
  theme_minimal() + theme(legend.position = "top")
save_png(p1, "1_ride_time_per_slot", h = 7)

# ---- 2. Latest bus to arrive by a given time on 9 of 10 days ---------------------------

# For each target arrival time: the latest slot whose bad-day arrival is still
# on time (recent period if it has enough data, otherwise the whole period).
base_q2 <- q1 |> filter(period != "All", trips >= 5)
if (nrow(base_q2) < 10) base_q2 <- q1 |> filter(period == "All", trips >= 5)
q2 <- bind_rows(lapply(TARGETS, function(t) {
  fits <- base_q2 |> filter(arrive_bad_day <= t)
  tibble(arrive_by = t, latest_bus = if (nrow(fits)) max(fits$slot_t) else NA_character_,
         based_on = first(base_q2$period))
}))
save_csv(q2, "2_latest_bus_for_arrival")

# ---- 4. Waiting at the stop vs the ride itself -------------------------------------------

# Average minutes late at your stop, and average extra ride time, per half hour.
q4 <- ok |> group_by(half) |>
  summarise(trips = n(), late_at_stop = round(mean(late_at_stop), 1),
            ride_extra = round(mean(ride_extra), 1), .groups = "drop")
save_csv(q4, "4_wait_vs_ride")
p4 <- q4 |> pivot_longer(c(late_at_stop, ride_extra), names_to = "part", values_to = "minutes") |>
  mutate(part = recode(part, late_at_stop = "Late at your stop", ride_extra = "Slower ride")) |>
  ggplot(aes(half, minutes, fill = part)) + geom_col() +
  labs(title = "Where the extra minutes come from", x = "Departure (half hour)", y = "Average extra minutes", fill = NULL) +
  theme_minimal() + theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))
save_png(p4, "4_wait_vs_ride")

# ---- 5. Weekdays, months and holidays ------------------------------------------------------

# Only rush-hour trips (07:00-09:00) here, so a quiet early bus doesn't skew a day.
rush <- ok |> filter(format(dep_aimed, "%H:%M") >= "07:00", format(dep_aimed, "%H:%M") < "09:00")
q5 <- bind_rows(
  rush |> group_by(group = as.character(weekday)) |> summarise_extra() |> mutate(by = "Weekday"),
  rush |> group_by(group = as.character(month)) |> summarise_extra() |> mutate(by = "Month"),
  rush |> group_by(group = holiday) |> summarise_extra() |> mutate(by = "Holiday")
) |> relocate(by)
save_csv(q5, "5_weekday_month_holiday")

# ---- 6. Weather ------------------------------------------------------------------------------

# Simple comparisons: average extra minutes with and without each condition.
q6 <- bind_rows(
  ok |> group_by(group = if_else(rainy, "Rain/sleet (1+ mm, 05-11)", "Dry")) |> summarise_extra() |> mutate(by = "Rain"),
  ok |> group_by(group = if_else(snowing, "Snowing (0.5+ cm, 05-11)", "Not snowing")) |> summarise_extra() |> mutate(by = "Snowfall"),
  ok |> group_by(group = if_else(snow_ground, "Snow on the ground (5+ cm)", "Bare ground")) |> summarise_extra() |> mutate(by = "Snow on ground"),
  ok |> group_by(group = if_else(frost, "Below 0 °C", "0 °C or warmer")) |> summarise_extra() |> mutate(by = "Frost")
) |> relocate(by) |> filter(!is.na(group))
save_csv(q6, "6a_weather_simple")

# A model that looks at all of them together, while taking the time of
# morning, weekday and month into account (they overlap: snow comes in
# winter, when other things differ too). Each estimate is the extra minutes
# that condition adds on its own.
model_data <- ok |> filter(!is.na(temp_bin), !is.na(rainy), !is.na(snowing))
q6m <- tryCatch({
  fit <- lm(total_extra ~ half + weekday + month + rainy + snowing + snow_ground + near_zero, data = model_data)
  ci  <- suppressMessages(confint(fit))
  tibble(term = names(coef(fit)), extra_min = round(coef(fit), 1),
         low = round(ci[, 1], 1), high = round(ci[, 2], 1)) |>
    filter(grepl("rainy|snowing|snow_ground|near_zero", term)) |>
    mutate(term = recode(term, rainyTRUE = "Rain/sleet", snowingTRUE = "Snowing", snow_groundTRUE = "Snow on the ground",
                         `near_zero-3 to +3 °C` = "-3 to +3 °C (vs above +3)", `near_zeroBelow -3 °C` = "Below -3 °C (vs above +3)"))
}, error = function(e) tibble(term = "Model could not be fitted", extra_min = NA, low = NA, high = NA))
save_csv(q6m, "6b_weather_model")

# ---- 7. Cancellations and missing times ----------------------------------------------------

# Per half hour: the share of trips cancelled, and the share without recorded
# times (which can't be used in the other questions).
q7 <- trips |> group_by(half) |>
  summarise(trips = n(), cancelled = sum(is_cancelled), share_cancelled = round(mean(is_cancelled), 3),
            share_no_times = round(mean(!has_times & !is_cancelled), 3), .groups = "drop")
save_csv(q7, "7_cancellations")

# ---- 8. Temperature ------------------------------------------------------------------------

# 8a: 5 °C bins, with how many mornings each average rests on.
q8a <- ok |> filter(!is.na(temp_bin)) |> group_by(temp_bin) |> summarise_extra()
save_csv(q8a, "8a_temperature_bins")
p8 <- ggplot(q8a, aes(temp_bin, avg_extra_min)) + geom_col(fill = "#4C78A8") +
  geom_text(aes(label = paste0(mornings, " d")), vjust = -0.4, size = 3) +
  labs(title = "Average extra minutes by temperature (number of mornings above each bar)",
       x = "Temperature when the bus left (°C)", y = "Average extra minutes") + theme_minimal()
save_png(p8, "8a_temperature_bins")

# 8b: around zero, a drop through zero overnight, and rain followed by frost.
q8b <- bind_rows(
  ok |> group_by(group = near_zero) |> summarise_extra() |> mutate(by = "Around zero"),
  ok |> filter(!is.na(drop_through_zero)) |>
    group_by(group = if_else(drop_through_zero, "Dropped through 0 °C overnight", "No drop through zero")) |>
    summarise_extra() |> mutate(by = "Overnight drop"),
  ok |> filter(!is.na(rain_then_frost)) |>
    group_by(group = if_else(rain_then_frost, "Rain overnight, then frost", "Not")) |>
    summarise_extra() |> mutate(by = "Rain then frost")
) |> relocate(by)
save_csv(q8b, "8b_around_zero")

# ---- 9. Where the time is lost along the route ----------------------------------------------

# For each trip, every stretch between two stops from your stop to the end
# stop: actual minutes minus timetabled minutes. Averaged per stretch and half
# hour, keeping stretches seen at least 30 times.
end_seq <- calls |> filter(is_to, seq > seq_from) |> group_by(date, journey) |>
  summarise(end_seq = min(seq), .groups = "drop")
legs <- calls |>
  inner_join(end_seq, by = c("date", "journey")) |>
  semi_join(ok, by = c("date", "journey")) |>
  filter(seq <= end_seq) |>
  arrange(date, journey, seq) |>
  group_by(date, journey) |>
  mutate(next_name = lead(stop_name), next_aimed = lead(coalesce(aimed_arr, aimed_dep)),
         next_actual = lead(coalesce(actual_arr, actual_dep)), pos = row_number(),
         half = format(floor_date(first(aimed_dep), "30 min"), "%H:%M")) |>
  ungroup() |>
  filter(!is.na(next_name)) |>
  mutate(stretch = paste(stop_name, "→", next_name),
         extra = as.numeric(difftime(next_actual, actual, units = "mins")) -
                 as.numeric(difftime(next_aimed, aimed, units = "mins"))) |>
  filter(!is.na(extra), abs(extra) < 30)
q9 <- legs |> group_by(stretch) |> mutate(order = median(pos), n_all = n()) |> ungroup() |>
  filter(n_all >= 30) |>
  group_by(order, stretch, half) |>
  summarise(trips = n(), avg_extra_min = round(mean(extra), 2), .groups = "drop") |>
  arrange(order, half)
save_csv(select(q9, -order), "9_stretches_by_half_hour")
p9 <- q9 |> filter(trips >= 10) |>
  ggplot(aes(half, reorder(stretch, -order), fill = avg_extra_min)) + geom_tile() +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
  labs(title = "Average extra minutes per stretch (red = slower than the timetable)",
       x = "Departure from your stop (half hour)", y = NULL, fill = "Extra min") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_png(p9, "9_stretches_heatmap", w = 10, h = 7)

# ---- Summary ----------------------------------------------------------------------------------

# A few headline numbers printed in the console, and saved as summary.txt.
peak <- q1 |> filter(period == "All", trips >= 5) |> slice_max(median_min - planned_min, n = 1, with_ties = FALSE)
worst_stretch <- legs |> filter(half >= "07:00", half < "09:00") |> group_by(stretch) |>
  summarise(n = n(), avg = mean(extra), .groups = "drop") |> filter(n >= 30) |> slice_max(avg, n = 1)
lines <- c(
  sprintf("Data: %d usable trips on %d mornings, %s to %s.", nrow(ok), n_distinct(ok$date), min(ok$date), max(ok$date)),
  sprintf("Busiest slot: %s, typically %.0f min (timetable %.0f), bad day %.0f min.",
          peak$slot_t, peak$median_min, peak$planned_min, peak$bad_day_min),
  if (nrow(worst_stretch)) sprintf("Slowest stretch at 07-09: %s, +%.1f min on average.", worst_stretch$stretch, worst_stretch$avg),
  "Latest bus to arrive in time on 9 of 10 days: see 2_latest_bus_for_arrival.csv.",
  paste("All results are in", OUT_DIR)
)
writeLines(lines, file.path(OUT_DIR, "summary.txt"))
cat(paste(lines, collapse = "\n"), "\n")
