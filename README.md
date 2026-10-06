# public-transport-notifications
Checks the planned and current public transport routes and times and sends a notification when something is disrupted, delayed, or canceled.

Runs on Entur's API and in Github actions.

Runs once on Fridays to check that there are no planned maintenance or disruptions of the relevant lines for the upcoming week.
In addition, runs four times in the morning and evening to look at live updates of the relevant stops.
Sends a Slack notification if anything seems unusual for the Friday check and for delays more than 5 minutes for the live updates.

To get the code to run, first you have to run the find_ids code to get the number ID for your stop.

Then, you enter that and the other necessary information into the following JSON string:
"{"client_name": "private-commutecheck", "delay_minutes": 5, "live_window_minutes": 30, "notify_when_ok": false, "min_share_of_normal": 0.8, "trips": [{"name": "Morning to work", "part": "morning", "outlook": ["08:00", "10:00"], "legs": [{"name": "from home", "from": "NSR:StopPlace:XXXX", "lines": ["bus 31"], "destination_contains": ["word on the sign"], "offset_minutes": 0}, {"name": "transfer at hub", "from": "NSR:StopPlace:HHHH", "lines": ["metro 2"], "destination_contains": ["word on the sign"], "offset_minutes": 15}]}, {"name": "Evening home", "part": "evening", "legs": [{"name": "from work", "from": "NSR:StopPlace:YYYY", "lines": ["metro 2"], "destination_contains": ["word on the sign"], "offset_minutes": 0}, {"name": "transfer at hub", "from": "NSR:StopPlace:HHHH", "lines": ["bus 31"], "destination_contains": ["word on the sign"], "offset_minutes": 20}]}]}"

Add this as a secret to Github actions under TRANSPORT_CONFIG

Also add your Webhook Slack URL under SLACK_WEBHOOK_URL.


The code in the repository is mainly made by Claude Opus 5.5 on medium effort.