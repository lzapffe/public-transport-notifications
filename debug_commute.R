# =============================================================================
# debug_commute.R - run this on your OWN computer (not in GitHub).
#
# For each leg in your settings, shows what Entur returns from the leg's stop in
# the next 30 minutes, and how many departures are left after each filter
# (mode/line, then destination). This shows which setting doesn't match.
# Everything is printed in your R console only.
# =============================================================================

# Load the packages.
library(httr2)
library(jsonlite)
library(purrr)

# "a %||% b" returns a unless it's missing.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Your private settings file (the same JSON as the TRANSPORT_CONFIG secret).
# Change the folder here if the file isn't in the "private" folder.
CONFIG_FILE <- "private/JSON string for secret in Github.txt"

# Read your settings from that file.
cfg <- fromJSON(paste(readLines(CONFIG_FILE, warn = FALSE), collapse = ""),
                simplifyVector = FALSE)

# Same line-setting rules as the main script ("bus 31", "31" or "RUT:Line:31").
MODES <- c("bus", "tram", "metro", "rail", "water", "coach", "air")
parse_line <- function(s) {
  s <- trimws(as.character(s))
  if (grepl(":Line:", s, fixed = TRUE)) return(list(id = s))
  parts <- strsplit(s, "\\s+")[[1]]
  if (length(parts) >= 2 && tolower(parts[1]) %in% MODES) {
    return(list(mode = tolower(parts[1]), code = paste(parts[-1], collapse = " ")))
  }
  list(code = s)
}

# Fetch the next 30 minutes of departures from a stop (or platform).
fetch <- function(from) {
  root  <- if (startsWith(from, "NSR:Quay:")) "quay" else "stopPlace"
  query <- sprintf('query($id: String!) {
    place: %s(id: $id) {
      name
      estimatedCalls(timeRange: 1800, numberOfDepartures: 1000, includeCancelledTrips: true) {
        aimedDepartureTime
        destinationDisplay { frontText }
        serviceJourney { line { id publicCode transportMode } }
      }
    }
  }', root)
  j <- request("https://api.entur.io/journey-planner/v3/graphql") |>
    req_headers("ET-Client-Name" = cfg$client_name %||% "private-commutecheck") |>
    req_body_json(list(query = query, variables = list(id = from)), auto_unbox = TRUE) |>
    req_perform() |>
    resp_body_json()
  if (!is.null(j$errors)) cat("  Entur error:", j$errors[[1]]$message, "\n")
  j$data$place
}

# Go through every leg: show the stop name, all lines/destinations found, and
# the number of departures left after each filter.
for (tr in cfg$trips) {
  legs <- tr$legs %||% list(tr)
  for (leg in legs) {
    cat("\n====", tr$name %||% "Trip", "/", leg$name %||% "leg", "====\n")
    place <- fetch(leg$from)
    if (is.null(place)) { cat("  Stop ID not found by Entur:", leg$from, "\n"); next }
    calls <- place$estimatedCalls %||% list()
    cat("  Stop:", place$name, "-", length(calls), "departures in the next 30 minutes\n")

    # What's actually there: mode, line number and destination of each departure.
    seen <- unique(map_chr(calls, function(cl) sprintf("%s %s -> %s",
      cl$serviceJourney$line$transportMode, cl$serviceJourney$line$publicCode,
      cl$destinationDisplay$frontText)))
    cat("  Found:", paste(head(seen, 25), collapse = "\n         "), "\n")

    # Filter 1: modes and lines.
    specs <- map(unlist(leg$lines), parse_line)
    modes <- tolower(unlist(leg$modes))
    step1 <- keep(calls, function(cl) {
      ln <- cl$serviceJourney$line
      m  <- tolower(ln$transportMode %||% ""); code <- toupper(ln$publicCode %||% "")
      (!length(modes) || m %in% modes) &&
        (!length(specs) || any(map_lgl(specs, function(sp)
          (is.null(sp$id) || sp$id == ln$id) && (is.null(sp$mode) || sp$mode == m) &&
            (is.null(sp$code) || toupper(sp$code) == code))))
    })
    cat("  After the mode/line filter (", paste(unlist(leg$lines), collapse = ", "), "):", length(step1), "\n")

    # Filter 2: destination words.
    dest  <- tolower(unlist(leg$destination_contains))
    step2 <- keep(step1, function(cl) {
      d <- tolower(cl$destinationDisplay$frontText %||% "")
      !length(dest) || any(map_lgl(dest, function(w) grepl(w, d, fixed = TRUE)))
    })
    cat("  After the destination filter (", paste(dest, collapse = ", "), "):", length(step2), "\n")
  }
}

# =============================================================================
# Stretch times ("sections"): for each leg with sections, show the last buses
# that left the leg's stop in the past 90 minutes, and for each section how
# long they took compared with the timetable. "actual" means Entur has a
# recorded time; "expected" means only a prediction was available.
# =============================================================================

# Fetch the last 90 minutes of buses from a stop, with every stop of each
# bus's journey today (timetable, expected and actual times).
fetch_past <- function(from) {
  root  <- if (startsWith(from, "NSR:Quay:")) "quay" else "stopPlace"
  query <- sprintf('query($id: String!, $start: DateTime!, $date: Date!) {
    place: %s(id: $id) {
      name
      estimatedCalls(startTime: $start, timeRange: 5400, numberOfDepartures: 200, includeCancelledTrips: true) {
        cancellation
        destinationDisplay { frontText }
        serviceJourney {
          line { id publicCode transportMode }
          estimatedCalls(date: $date) {
            quay { id name stopPlace { id name parent { id name } } }
            aimedArrivalTime aimedDepartureTime expectedArrivalTime expectedDepartureTime
            actualArrivalTime actualDepartureTime cancellation
          }
        }
      }
    }
  }', root)
  start <- format(Sys.time() - 90 * 60, "%Y-%m-%dT%H:%M:%S%z")
  start <- sub("(\\d{2})(\\d{2})$", "\\1:\\2", start)
  j <- request("https://api.entur.io/journey-planner/v3/graphql") |>
    req_headers("ET-Client-Name" = cfg$client_name %||% "private-commutecheck") |>
    req_body_json(list(query = query, variables = list(id = from, start = start, date = format(Sys.Date()))),
                  auto_unbox = TRUE) |>
    req_perform() |>
    resp_body_json()
  if (!is.null(j$errors)) cat("  Entur error:", j$errors[[1]]$message, "\n")
  j$data$place
}

# Is a journey's stop the given stop (whole stop, platform, or a hub's parent ID)?
is_stop <- function(cl, id) identical(cl$quay$id, id) || identical(cl$quay$stopPlace$id, id) ||
  identical(cl$quay$stopPlace$parent$id, id)

# Turn Entur's time text into a time (or NA).
tm <- function(x) if (is.null(x)) NA else as.POSIXct(sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x), format = "%Y-%m-%dT%H:%M:%S%z")

for (tr in cfg$trips) {
  for (leg in tr$legs %||% list(tr)) {
    if (!length(leg$sections)) next
    cat("\n==== Stretch times:", tr$name %||% "Trip", "/", leg$name %||% "leg", "====\n")
    place <- fetch_past(leg$from)
    if (is.null(place)) { cat("  Stop ID not found by Entur:", leg$from, "\n"); next }
    calls <- place$estimatedCalls %||% list()
    cat("  Stop:", place$name, "-", length(calls), "departures in the last 90 minutes (all lines)\n")

    # Same line filter as above (mode/line only; the destination filter is
    # skipped here to keep it simple).
    specs <- map(unlist(leg$lines), parse_line)
    calls <- keep(calls, function(cl) {
      ln <- cl$serviceJourney$line; m <- tolower(ln$transportMode %||% ""); code <- toupper(ln$publicCode %||% "")
      !length(specs) || any(map_lgl(specs, function(sp)
        (is.null(sp$id) || sp$id == ln$id) && (is.null(sp$mode) || sp$mode == m) &&
          (is.null(sp$code) || toupper(sp$code) == code)))
    })
    cat("  On your line(s):", length(calls), "\n")

    # For each section, show each of the last 6 buses: when it left, how long it
    # took, the timetable time, and whether the times were recorded or predicted.
    # Show the route of the most recent bus: every stop with its IDs, so you
    # can compare them with the IDs in your settings.
    if (length(calls)) {
      st <- calls[[length(calls)]]$serviceJourney$estimatedCalls %||% list()
      cat("  Route of the most recent bus (", length(st), "stops): stop ID | parent ID | name\n")
      for (x in st) cat(sprintf("     %s | %s | %s\n", x$quay$stopPlace$id %||% "-",
                                x$quay$stopPlace$parent$id %||% "-", x$quay$stopPlace$name %||% x$quay$name %||% "?"))
    }

    for (s in leg$sections) {
      from <- s$from %||% leg$from
      cat("  -- Section:", s$name %||% "section", "\n")
      for (cl in tail(calls, 6)) {
        st <- cl$serviceJourney$estimatedCalls %||% list()
        i <- which(map_lgl(st, is_stop, id = from))[1]
        k <- which(map_lgl(st, is_stop, id = s$to)); k <- k[!is.na(i) & k > i][1]
        # A bus where the end comes before the start is going the other way
        # (this debug script doesn't use the destination filter); the main
        # script simply ignores those.
        if (!is.na(i) && is.na(k) && any(map_lgl(st, is_stop, id = s$to))) {
          cat("     bus to", cl$destinationDisplay$frontText, ": other direction, ignored\n")
          next
        }
        if (is.na(i) || is.na(k)) {
          cat("     bus to", cl$destinationDisplay$frontText, ": stops not found on its route (",
              length(st), "stops in the journey; start", if (is.na(i)) "NOT found" else "found",
              "; end", if (any(map_lgl(st, is_stop, id = s$to))) "found, but not after the start" else "NOT found", ")\n")
          next
        }
        dep <- tm(st[[i]]$actualDepartureTime %||% st[[i]]$expectedDepartureTime)
        arr <- tm(st[[k]]$actualArrivalTime %||% st[[k]]$expectedArrivalTime)
        plan <- as.numeric(difftime(tm(st[[k]]$aimedArrivalTime), tm(st[[i]]$aimedDepartureTime), units = "mins"))
        took <- as.numeric(difftime(arr, dep, units = "mins"))
        cat(sprintf("     left %s, took %.0f min (timetable %.0f, %+.0f), %s at the start, %s at the end%s\n",
                    format(dep, "%H:%M"), took, plan, took - plan,
                    if (is.null(st[[i]]$actualDepartureTime)) "expected" else "actual",
                    if (is.null(st[[k]]$actualArrivalTime)) "expected" else "actual",
                    if (isTRUE(arr > Sys.time())) " (not there yet)" else ""))
      }
    }
  }
}
