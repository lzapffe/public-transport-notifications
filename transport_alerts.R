# =============================================================================
# transport_alerts.R
#
# Checks your commute with Entur's Journey Planner API (all public transport in
# Norway, including Ruter) and sends a Slack message ONLY when something looks
# wrong:
#
#   MODE=outlook  (Fridays): for each weekday next week, compares the planned
#                 departures in each trip's outlook window (e.g. 08:00-10:00)
#                 with the most recent normal week, and reports fewer
#                 departures, planned cancellations and disruption messages.
#   MODE=live     (weekday mornings/evenings): checks the departures in the
#                 next 30 minutes for cancellations, delays and active
#                 disruption messages.
#
# A trip can have several legs (e.g. a bus to a hub, then a metro from the
# hub). Each leg has its own boarding stop, lines (by mode and number, e.g.
# "bus 31", "metro 2") and direction, plus an offset in minutes for when you
# usually reach that stop, so transfers are checked at the right time.
#
# Privacy (the repository is public, and so are its Actions logs):
#   - Stops, lines and directions only come from the GitHub secret
#     TRANSPORT_CONFIG, and the script never prints them, or anything derived
#     from them (stop names, line numbers, destinations), to the log.
#   - The log only shows counts, e.g. "Live check: 2 leg(s), 0 issue(s)".
#   - Details only go into the Slack message, which only you receive.
#
# Known warnings: the Friday outlook saves the disruption messages it reported
# for next week in state/known_alerts.json, and the live checks don't repeat
# those. A new message found by a live check is sent once and then added to the
# same list. Delays and cancellations are always reported. The file only holds
# keyed hashes (HMAC-SHA256, with your TRANSPORT_CONFIG as the key), so it
# can't be linked to Entur's messages, stops or lines, even in a public repo.
#
# Needs: TRANSPORT_CONFIG and SLACK_WEBHOOK_URL (GitHub secrets), MODE.
# =============================================================================

# Load the packages:
#   httr2     - web requests to Entur and Slack
#   jsonlite  - reading the JSON settings
#   dplyr     - working with the departure tables
#   lubridate - dates, times and the Oslo time zone
#   purrr     - map()/keep() for lists returned by the API
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(openssl)
})

# "a %||% b" returns a, unless a is missing (NULL or empty), then it returns b.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Settings (from the TRANSPORT_CONFIG secret) ----------------------------

# Which check to run: "outlook" or "live" (set by the workflow).
MODE <- tolower(Sys.getenv("MODE", "live"))
TZ   <- "Europe/Oslo"

# Read the settings. Error messages never include the settings themselves.
cfg_text <- Sys.getenv("TRANSPORT_CONFIG")
if (cfg_text == "") stop("The TRANSPORT_CONFIG secret is empty or missing.", call. = FALSE)
cfg <- tryCatch(fromJSON(cfg_text, simplifyVector = FALSE),
                error = function(e) stop("TRANSPORT_CONFIG is not valid JSON.", call. = FALSE))

# General settings, with defaults for anything left out:
#   client_name          - identifies the app to Entur (Entur requires a name)
#   delay_minutes        - live check: delays of at least this many minutes are reported
#   live_window_minutes  - live check: how far ahead to look (default 30)
#   notify_when_ok       - live check: also send a message when everything is fine
#   min_share_of_normal  - outlook: flag a day with fewer departures than this share of normal
#   noon                 - live check: runs before this time check "morning" trips, after it "evening" trips
#   repeat_new_warnings  - live check: repeat a new warning in every check (default false: send it once)
CLIENT      <- cfg$client_name %||% "private-commutecheck"
DELAY_MIN   <- as.numeric(cfg$delay_minutes %||% 5)
LIVE_WINDOW <- as.numeric(cfg$live_window_minutes %||% 30)
NOTIFY_OK   <- isTRUE(cfg$notify_when_ok)
MIN_SHARE   <- as.numeric(cfg$min_share_of_normal %||% 0.8)
NOON        <- cfg$noon %||% "12:00"
TRIPS       <- cfg$trips %||% list()
REPEAT_NEW  <- isTRUE(cfg$repeat_new_warnings)
WEBHOOK     <- Sys.getenv("SLACK_WEBHOOK_URL")
if (!length(TRIPS)) stop("TRANSPORT_CONFIG has no trips.", call. = FALSE)

# The current time in Oslo, today's date and weekday (1 = Monday ... 7 = Sunday).
NOW   <- with_tz(Sys.time(), TZ)
TODAY <- as_date(NOW)
WDAY  <- wday(TODAY, week_start = 1)

# Transport modes as Entur names them, used in line settings like "bus 31".
MODES <- c("bus", "tram", "metro", "rail", "water", "coach", "air")

# ---- Small helpers ----------------------------------------------------------

# iso_time(): a time in the format Entur expects, e.g. "2026-10-12T07:00:00+02:00".
iso_time <- function(t) {
  s <- format(with_tz(t, TZ), "%Y-%m-%dT%H:%M:%S%z")
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", s)
}

# at_time(): a date plus a clock time like "07:00", as an Oslo time.
at_time <- function(date, hhmm) ymd_hm(paste(date, hhmm), tz = TZ)

# hm_text(): show a time as "07:15".
hm_text <- function(t) format(with_tz(t, TZ), "%H:%M")

# trip_days(): the weekdays a trip applies to (default Monday-Friday).
trip_days <- function(trip) as.integer(unlist(trip$days %||% list(1, 2, 3, 4, 5)))

# legs_of(): a trip's legs. A trip without "legs" is treated as one leg, using
# the trip's own "from", "lines", "modes" and "destination_contains".
legs_of <- function(trip) trip$legs %||% list(trip)

# leg_offset(): minutes from the start of the trip until you usually board
# this leg (0 for the first leg), so transfers are checked at the right time.
leg_offset <- function(leg) as.numeric(leg$offset_minutes %||% 0)

# leg_label(): how a leg is named in Slack messages.
leg_label <- function(trip, leg, i, n_legs) {
  base <- trip$name %||% "Trip"
  if (n_legs > 1) paste0(base, ", ", leg$name %||% paste("leg", i)) else base
}

# pick_text(): Entur gives texts in several languages; prefer Norwegian, else the first.
pick_text <- function(ml) {
  if (!length(ml)) return("")
  langs <- map_chr(ml, function(x) x$language %||% "")
  i <- which(langs %in% c("no", "nob", "nb"))[1]
  if (is.na(i)) i <- 1
  ml[[i]]$value %||% ""
}

# ---- Choosing the right departures ------------------------------------------

# parse_line(): understand one entry in a leg's "lines" list. Accepted forms:
#   "bus 31"       - mode and line number (most precise at a hub)
#   "31"           - line number, any mode
#   "RUT:Line:31"  - Entur's line ID (from find_ids.R)
parse_line <- function(s) {
  s <- trimws(as.character(s))
  if (grepl(":Line:", s, fixed = TRUE)) return(list(id = s))
  parts <- strsplit(s, "\\s+")[[1]]
  if (length(parts) >= 2 && tolower(parts[1]) %in% MODES) {
    return(list(mode = tolower(parts[1]), code = paste(parts[-1], collapse = " ")))
  }
  list(code = s)
}

# keeps_call(): TRUE if a departure matches a leg's settings:
#   - "modes": only these transport modes (e.g. ["metro"]),
#   - "lines": one of these lines (see parse_line),
#   - "destination_contains": the destination sign contains one of these words
#     (this picks the direction).
# Settings that are left out don't filter anything.
keeps_call <- function(cl, leg) {
  ln   <- cl$serviceJourney$line
  mode <- tolower(ln$transportMode %||% "")
  code <- toupper(ln$publicCode %||% "")
  id   <- ln$id %||% ""

  modes <- tolower(unlist(leg$modes))
  if (length(modes) && !(mode %in% modes)) return(FALSE)

  specs <- map(unlist(leg$lines), parse_line)
  if (length(specs)) {
    ok <- any(map_lgl(specs, function(sp) {
      (is.null(sp$id) || sp$id == id) &&
        (is.null(sp$mode) || sp$mode == mode) &&
        (is.null(sp$code) || toupper(sp$code) == code)
    }))
    if (!ok) return(FALSE)
  }

  dest <- tolower(unlist(leg$destination_contains))
  # Ignore empty entries, so "destination_contains": [""] or [" "] means
  # "no destination filter", the same as leaving the setting out.
  dest <- trimws(dest)
  dest <- dest[!is.na(dest) & nzchar(dest)]
  if (length(dest)) {
    d <- tolower(cl$destinationDisplay$frontText %||% "")
    if (!any(map_lgl(dest, function(w) grepl(w, d, fixed = TRUE)))) return(FALSE)
  }
  TRUE
}

# ---- Entur ------------------------------------------------------------------

# The fields fetched for each disruption message ("situation").
SIT_FIELDS <- "situations { situationNumber summary { value language } description { value language }
               validityPeriod { startTime endTime } }"

# entur_calls(): fetch all departures from a leg's stop between `start` and
# `start + range_sec`, including cancelled ones (up to 1,000, enough for a big
# hub), with each departure's line, mode and destination, and the disruption
# messages for the stop and each departure. The stop can be a whole stop
# ("NSR:StopPlace:...") or one platform ("NSR:Quay:..."). The ID is sent in the
# request body to Entur and never printed.
entur_calls <- function(leg, start, range_sec) {
  root  <- if (startsWith(leg$from, "NSR:Quay:")) "quay" else "stopPlace"
  query <- sprintf(
    'query($id: String!, $start: DateTime!, $range: Int!) {
       place: %s(id: $id) {
         %s
         estimatedCalls(startTime: $start, timeRange: $range, numberOfDepartures: 1000,
                        includeCancelledTrips: true) {
           aimedDepartureTime expectedDepartureTime realtime cancellation
           destinationDisplay { frontText }
           serviceJourney { line { id publicCode transportMode } }
           %s
         }
       }
     }',
    root, SIT_FIELDS, SIT_FIELDS
  )
  vars <- list(id = leg$from, start = iso_time(start), range = as.integer(range_sec))

  resp <- request("https://api.entur.io/journey-planner/v3/graphql") |>
    req_headers("ET-Client-Name" = CLIENT) |>
    req_body_json(list(query = query, variables = vars), auto_unbox = TRUE) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  if (resp_status(resp) >= 400) stop("Entur returned HTTP ", resp_status(resp), ".", call. = FALSE)
  j <- resp_body_json(resp)
  if (!is.null(j$errors)) stop("Entur returned an error for one of the legs.", call. = FALSE)
  if (is.null(j$data$place)) stop("A stop ID in TRANSPORT_CONFIG was not found by Entur.", call. = FALSE)
  j$data$place
}

# situation_texts(): turn disruption messages into short, unique texts, keeping
# only those valid at some point between `from` and `to`. Each text is named by
# a key identifying the message: Entur's situation number when there is one
# (stays the same if Entur edits the wording), otherwise the text itself.
situation_texts <- function(sits, from, to) {
  if (!length(sits)) return(character())
  valid <- keep(sits, function(s) {
    st <- if (!is.null(s$validityPeriod$startTime)) ymd_hms(s$validityPeriod$startTime, tz = TZ, quiet = TRUE) else NA
    en <- if (!is.null(s$validityPeriod$endTime)) ymd_hms(s$validityPeriod$endTime, tz = TZ, quiet = TRUE) else NA
    (is.na(st) || st <= to) && (is.na(en) || en >= from)
  })
  if (!length(valid)) return(character())
  txt <- map_chr(valid, function(s) {
    sm <- pick_text(s$summary)
    ds <- pick_text(s$description)
    out <- if (nzchar(ds) && ds != sm) paste0(sm, ": ", ds) else sm
    if (nchar(out) > 300) paste0(substr(out, 1, 297), "...") else out
  })
  keys <- map_chr(valid, function(s) s$situationNumber %||% "")
  names(txt) <- ifelse(nzchar(keys), keys, txt)
  txt <- txt[nzchar(txt)]
  txt[!duplicated(names(txt))]
}

# leg_data(): departures and messages for one leg between two times.
#   1. Fetch all departures from the leg's stop in the period.
#   2. Keep only those matching the leg's modes, lines and direction.
#   3. Return a table (aimed and expected times, cancelled, mode and line),
#      the disruption messages for the stop and the kept departures, and counts.
# Messages about the stop as a whole are only kept for single-stop legs given as
# a platform, or when no lines/modes are set; at a hub they'd mostly concern
# other lines, so there only messages attached to your departures are used.
leg_data <- function(leg, start, end) {
  place <- entur_calls(leg, start, as.numeric(difftime(end, start, units = "secs")))
  calls <- keep(place$estimatedCalls %||% list(), function(cl) keeps_call(cl, leg))

  df <- if (length(calls)) {
    tibble(
      aimed     = ymd_hms(map_chr(calls, "aimedDepartureTime"), tz = TZ),
      expected  = ymd_hms(map_chr(calls, function(cl) cl$expectedDepartureTime %||% cl$aimedDepartureTime), tz = TZ),
      cancelled = map_lgl(calls, function(cl) isTRUE(cl$cancellation)),
      line      = map_chr(calls, function(cl) {
        ln <- cl$serviceJourney$line
        trimws(paste(ln$transportMode %||% "", ln$publicCode %||% ""))
      })
    )
  } else {
    tibble(aimed = as.POSIXct(character(), tz = TZ), expected = as.POSIXct(character(), tz = TZ),
           cancelled = logical(), line = character())
  }

  filtered  <- length(unlist(leg$lines)) > 0 || length(unlist(leg$modes)) > 0
  stop_sits <- if (!filtered || startsWith(leg$from, "NSR:Quay:")) place$situations %||% list() else list()
  sits <- c(stop_sits, do.call(c, lapply(calls, function(cl) cl$situations %||% list())))

  list(df = df,
       situations = situation_texts(sits, start, end),
       n = sum(!df$cancelled),
       n_cancelled = sum(df$cancelled))
}

# ---- Known warnings (saved between runs) --------------------------------------

# Where the known warnings are saved. The workflow commits this file back to
# the repository after each run.
KNOWN_FILE <- file.path("state", "known_alerts.json")

# week_key(): the ISO week a date belongs to, e.g. "2026-W42".
week_key <- function(date) format(as_date(date), "%G-W%V")

# alert_hash(): a keyed hash of a warning's key. Without your TRANSPORT_CONFIG
# (a secret), nobody can tell which Entur message a hash belongs to.
alert_hash <- function(keys) {
  if (!length(keys)) return(character())
  as.character(sha256(enc2utf8(keys), key = cfg_text))
}

# load_known() / save_known(): read and write the file as a list of weeks,
# each holding the hashes of the warnings already reported for that week.
# Only the current and coming weeks are kept, so the file stays small.
load_known <- function() {
  if (!file.exists(KNOWN_FILE)) return(list())
  tryCatch(fromJSON(KNOWN_FILE, simplifyVector = FALSE), error = function(e) list())
}
save_known <- function(known) {
  keep_weeks <- week_key(TODAY + days(c(-7, 0, 7)))
  known <- known[names(known) %in% keep_weeks]
  dir.create(dirname(KNOWN_FILE), showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(lapply(known, function(x) I(unique(unlist(x)))), auto_unbox = TRUE), KNOWN_FILE)
}

# add_known(): add hashes to a week in the known list.
add_known <- function(known, week, hashes) {
  known[[week]] <- unique(c(unlist(known[[week]]), hashes))
  known
}

# ---- Slack ------------------------------------------------------------------

# send_slack(): post a message to your Slack channel through the webhook.
send_slack <- function(text) {
  if (WEBHOOK == "") {
    message("SLACK_WEBHOOK_URL is not set; no message sent.")
    return(invisible())
  }
  request(WEBHOOK) |>
    req_body_json(list(text = text), auto_unbox = TRUE) |>
    req_perform()
  message("Slack message sent.")
}

# ---- Live check ---------------------------------------------------------------

# run_live(): check each leg of today's trips for the coming LIVE_WINDOW minutes.
#   1. Runs before NOON check the trips marked "part": "morning", later runs
#      the "evening" trips; only trips for today's weekday are checked.
#   2. Each leg is checked from now + its offset (e.g. +15 minutes for the
#      transfer at the hub) and LIVE_WINDOW minutes ahead, with real-time data.
#   3. Report cancellations, delays of at least DELAY_MIN minutes, active
#      disruption messages, and a warning if no departures were found.
#   4. Send a Slack message only if something was found (or always, if
#      notify_when_ok is true). The log only shows counts.
run_live <- function() {
  part <- if (NOW < at_time(TODAY, NOON)) "morning" else "evening"
  due  <- keep(TRIPS, function(tr) {
    identical(tolower(tr$part %||% ""), part) && WDAY %in% trip_days(tr)
  })
  if (!length(due)) {
    message("Live check: no trips to check right now.")
    return(invisible())
  }

  known      <- load_known()
  this_week  <- week_key(TODAY)
  known_week <- unlist(known[[this_week]])
  n_known    <- 0
  new_hashes <- character()

  parts  <- character()
  issues <- 0
  n_legs_checked <- 0
  for (tr in due) {
    legs <- legs_of(tr)
    for (i in seq_along(legs)) {
      leg   <- legs[[i]]
      start <- NOW + minutes(leg_offset(leg))
      end   <- start + minutes(LIVE_WINDOW)
      td    <- leg_data(leg, start, end)
      df    <- td$df
      n_legs_checked <- n_legs_checked + 1

      cancelled <- df |> filter(cancelled)
      delayed   <- df |>
        filter(!cancelled, as.numeric(difftime(expected, aimed, units = "mins")) >= DELAY_MIN)

      lines_out <- character()
      if (nrow(df) == 0) {
        lines_out <- c(lines_out, ":grey_question: No matching departures found.")
      }
      if (nrow(cancelled) > 0) {
        lines_out <- c(lines_out, paste0(":x: Cancelled: ",
          paste0(hm_text(cancelled$aimed), " (", cancelled$line, ")", collapse = ", ")))
      }
      if (nrow(delayed) > 0) {
        mins <- round(as.numeric(difftime(delayed$expected, delayed$aimed, units = "mins")))
        lines_out <- c(lines_out, paste0(":hourglass: Delayed: ",
          paste0(hm_text(delayed$aimed), " (", delayed$line, ", +", mins, " min)", collapse = ", ")))
      }
      # Disruption messages: leave out those already reported (in Friday's
      # outlook, or earlier this week), and remember the new ones.
      if (length(td$situations) > 0) {
        h       <- alert_hash(names(td$situations))
        is_new  <- !(h %in% known_week) & !(h %in% new_hashes)
        n_known <- n_known + sum(!is_new)
        if (any(is_new)) {
          lines_out  <- c(lines_out, paste0(":warning: *New:* ", td$situations[is_new]))
          new_hashes <- c(new_hashes, h[is_new])
        }
      }

      issues <- issues + length(lines_out)
      if (length(lines_out) > 0 || NOTIFY_OK) {
        header <- sprintf("*%s* (%s-%s)", leg_label(tr, leg, i, length(legs)), hm_text(start), hm_text(end))
        body   <- if (length(lines_out)) paste(lines_out, collapse = "\n") else ":white_check_mark: No disruptions found."
        parts  <- c(parts, paste(header, body, sep = "\n"))
      }
    }
  }

  message(sprintf("Live check: %d leg(s) checked, %d issue(s) found, %d known warning(s) left out.",
                  n_legs_checked, issues, n_known))
  if (length(parts)) {
    send_slack(paste0(":bus: *Commute check ", hm_text(NOW), "*\n\n", paste(parts, collapse = "\n\n")))
  }

  # Save new warnings as known for the rest of the week (unless they should
  # be repeated), only after the Slack message was sent.
  if (length(new_hashes) && !REPEAT_NEW) {
    save_known(add_known(known, this_week, new_hashes))
  }
}

# ---- Friday outlook -------------------------------------------------------------

# leg_window(): the outlook window on a given date for a leg: the trip's
# outlook times, shifted by the leg's offset.
leg_window <- function(date, win, leg) {
  c(at_time(date, win[[1]]), at_time(date, win[[2]])) + minutes(leg_offset(leg))
}

# find_normal(): the most recent "normal" day to compare with, for one leg,
# weekday and window. A normal day has departures, no cancellations and no
# disruption messages. It tries, in order:
#   1. the same weekday this week, the week before, and the week before that
#      (the most recent normal week), and
#   2. if Entur has no data for those past days, the same weekday 2, 3 and 4
#      weeks ahead.
# Returns the count and the date used, or NULL if no normal day was found.
find_normal <- function(leg, date_next, win) {
  for (o in c(-7, -14, -21, 7, 14, 21)) {
    d  <- date_next + days(o)
    w  <- leg_window(d, win, leg)
    td <- leg_data(leg, w[1], w[2])
    if (td$n > 0 && td$n_cancelled == 0 && length(td$situations) == 0) {
      return(list(n = td$n, date = d))
    }
  }
  NULL
}

# run_outlook(): check next week (Monday-Friday) for every leg of each trip
# with an "outlook" window, e.g. ["08:00", "10:00"].
#   1. For each day, fetch next week's planned departures in the window
#      (shifted by the leg's offset), and find the most recent normal day.
#   2. Flag the day if it has fewer than MIN_SHARE of the normal number of
#      departures, no departures, or planned cancellations.
#   3. Collect disruption messages valid during the window next week, listing
#      each message once with the days it applies to.
#   4. Send a Slack message only if something was found.
run_outlook <- function() {
  next_monday <- floor_date(TODAY, "week", week_start = 1) + days(7)
  week_no     <- isoweek(next_monday)
  checked <- keep(TRIPS, function(tr) length(tr$outlook) == 2)
  parts   <- character()
  issues  <- 0
  n_legs_checked <- 0
  sit_keys <- character()   # keys of all warnings reported for next week

  for (tr in checked) {
    win  <- tr$outlook
    legs <- legs_of(tr)
    for (i in seq_along(legs)) {
      leg       <- legs[[i]]
      day_lines <- character()
      sit_days  <- list()
      n_legs_checked <- n_legs_checked + 1

      for (d in 0:4) {
        date0 <- next_monday + days(d)
        if (!((d + 1) %in% trip_days(tr))) next
        w         <- leg_window(date0, win, leg)
        td        <- leg_data(leg, w[1], w[2])
        day_label <- format(date0, "%A %d.%m")

        if (td$n == 0) {
          day_lines <- c(day_lines, sprintf(":grey_question: %s: no matching departures found.", day_label))
        } else {
          normal <- find_normal(leg, date0, win)
          if (!is.null(normal) && td$n < MIN_SHARE * normal$n) {
            day_lines <- c(day_lines, sprintf(
              ":small_red_triangle_down: %s: %d departures (%d on %s, the last normal day).",
              day_label, td$n, normal$n, format(normal$date, "%d.%m")))
          }
        }
        if (td$n_cancelled > 0) {
          day_lines <- c(day_lines, sprintf(":x: %s: %d planned cancellation(s).", day_label, td$n_cancelled))
        }
        for (s in td$situations) {
          sit_days[[s]] <- c(sit_days[[s]], format(date0, "%a"))
        }
        sit_keys <- c(sit_keys, names(td$situations))
      }

      sit_lines <- if (length(sit_days)) {
        paste0(":warning: ", names(sit_days), " _(", map_chr(sit_days, function(x) paste(unique(x), collapse = ", ")), ")_")
      } else character()

      all_lines <- c(day_lines, sit_lines)
      issues <- issues + length(all_lines)
      if (length(all_lines)) {
        w0 <- leg_window(next_monday, win, leg)
        parts <- c(parts, paste(sprintf("*%s* (%s-%s)", leg_label(tr, leg, i, length(legs)), hm_text(w0[1]), hm_text(w0[2])),
                                paste(all_lines, collapse = "\n"), sep = "\n"))
      }
    }
  }

  message(sprintf("Outlook: %d leg(s) checked for week %d, %d issue(s) found.", n_legs_checked, week_no, issues))
  if (length(parts)) {
    send_slack(paste0(":calendar: *Commute outlook for week ", week_no, "*\n\n", paste(parts, collapse = "\n\n")))
  }

  # Save the reported warnings as known for next week, so the live checks
  # next week don't repeat them. Only after the Slack message was sent.
  if (length(sit_keys)) {
    known <- load_known()
    save_known(add_known(known, week_key(next_monday), alert_hash(unique(sit_keys))))
  }
}

# ---- Run --------------------------------------------------------------------

# Run the chosen check. If anything fails, a short, non-revealing message goes
# to the (public) log, and a warning to Slack, so a broken check doesn't go
# unnoticed. Messages written by this script never contain stops or lines.
tryCatch({
  if (MODE == "outlook") run_outlook() else run_live()
}, error = function(e) {
  message("The check failed: ", conditionMessage(e))
  try(send_slack(paste0(":warning: The commute ", MODE, " check failed. See the GitHub Actions log.")), silent = TRUE)
  quit(save = "no", status = 1)
})
