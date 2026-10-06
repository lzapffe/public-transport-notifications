# =============================================================================
# find_ids.R - run this on your OWN computer (not in GitHub) to find the IDs
# you need for TRANSPORT_CONFIG. Nothing is saved or sent anywhere except to
# Entur's public API.
#
# 1. find_stop("Jernbanetorget")       -> stop names and their IDs (NSR:StopPlace:...)
# 2. show_departures("NSR:StopPlace:X") -> the next departures from that stop,
#    with line IDs (RUT:Line:...), destinations and platform IDs (NSR:Quay:...)
# =============================================================================

# Load the packages for web requests and lists.
library(httr2)
library(purrr)

# "a %||% b" returns a unless it's missing (built into R 4.4+, defined here for older R).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Entur asks every app to identify itself; any name in this form works.
CLIENT <- "private-commutecheck"

# find_stop(): search Entur's stop register for a name, and print matching
# stops with their IDs and area.
find_stop <- function(text) {
  j <- request("https://api.entur.io/geocoder/v1/autocomplete") |>
    req_headers("ET-Client-Name" = CLIENT) |>
    req_url_query(text = text, layers = "venue", size = 10) |>
    req_perform() |>
    resp_body_json()
  for (f in j$features) {
    p <- f$properties
    cat(sprintf("%-28s %-40s %s\n", p$id, p$name, p$locality %||% ""))
  }
}

# show_departures(): print the next departures from a stop, so you can see
# each departure's mode and line number (e.g. "bus 31"), destination sign and
# platform (quay). Use n = 100 or more at a big hub to see all lines.
show_departures <- function(stop_id, n = 30) {
  query <- 'query($id: String!, $n: Int!) {
    stopPlace(id: $id) {
      name
      estimatedCalls(numberOfDepartures: $n) {
        aimedDepartureTime
        destinationDisplay { frontText }
        quay { id publicCode }
        serviceJourney { line { id publicCode transportMode } }
      }
    }
  }'
  j <- request("https://api.entur.io/journey-planner/v3/graphql") |>
    req_headers("ET-Client-Name" = CLIENT) |>
    req_body_json(list(query = query, variables = list(id = stop_id, n = n)), auto_unbox = TRUE) |>
    req_perform() |>
    resp_body_json()
  cat("Departures from", j$data$stopPlace$name, "\n")
  for (cl in j$data$stopPlace$estimatedCalls) {
    cat(sprintf("%s  %-6s %-5s %-16s to %-30s platform %-4s %s\n",
                substr(cl$aimedDepartureTime, 12, 16),
                cl$serviceJourney$line$transportMode %||% "",
                cl$serviceJourney$line$publicCode, cl$serviceJourney$line$id,
                cl$destinationDisplay$frontText,
                cl$quay$publicCode %||% "", cl$quay$id))
  }
}

# Example use (replace with your own stops):
# find_stop("Jernbanetorget")
# show_departures("NSR:StopPlace:4000")
