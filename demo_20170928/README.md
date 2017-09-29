# Gemeen Amsterdam Dashboard

Code to create the dashboard for Gemeente Amsterdam. Demo given on Monday, July 10, 2017.

The dashboard can be viewed [here](https://luijkr.shinyapps.io/gemeenteamsterdam_dashboard_shinyapps/).

To deploy this to shinyapps.io, use:

```
rsconnect::setAccountInfo(name = '<NAME>', token = '<TOKEN>', secret = '<SECRET>')
rsconnect::deployApp('/path/to/dir')
```