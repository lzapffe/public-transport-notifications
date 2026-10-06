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

# Read your settings from the private file (change the path if needed).
cfg <- fromJSON(paste(readLines("private/commute_config.txt", warn = FALSE), collapse = ""),
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
