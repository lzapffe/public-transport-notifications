# Data analysis (run by hand)

These scripts are **not part of the commute alerts**. GitHub Actions never runs them, and the Central scheduler doesn't start them. You run them yourself in RStudio when you want fresh numbers.

## Bus delay history

Bus 81 from your stop to your end stop on weekday mornings (05:30–11:00), for the last two years, compared with the weather.

| Script | What it does | Uses BigQuery? |
|---|---|---|
| `bus_history_fetch.R` | Downloads Entur's recorded bus times (BigQuery), the stop names (Entur) and hourly Oslo weather (Open-Meteo). If you run it again, it only downloads the days you don't already have. | Yes. It checks the size first and asks before running. |
| `bus_history_analysis.R` | Answers the nine questions (ride time per 5-minute slot, the latest bus that gets you there in time, waiting vs. riding, weekdays, weather, cancellations, temperature, and which stretch of the route is slow). | No. It only reads the saved files. |

**Workflow:** settings file → fetch → analysis → open the CSV and PNG files.

- Settings: `private/bus_history_settings.json`.
- Downloaded data: `private/bus_history/`.
- Results: `private/bus_history/output/`.

The `private/` folder is ignored by Git, so data, stop IDs and results stay on your computer. Only these scripts go to GitHub.

Settings file example:

```json
{"gcp_project": "your-project-id", "line": "RUT:Line:81",
 "from": "NSR:StopPlace:6743", "to": "NSR:StopPlace:58404"}
```

Optional keys:
- `first_date` (default: two years ago)
- `time_from` / `time_to` (default: 05:30:00 / 11:00:00)
- `max_total_gb` (default: 300; the free sandbox allows 1,000 GB a month)
- `recent_days` (default: 90; used by the analysis for the "recent" results)

Run from the repository folder:

```r
source("data-analysis/bus_history_fetch.R")
source("data-analysis/bus_history_analysis.R")
```
