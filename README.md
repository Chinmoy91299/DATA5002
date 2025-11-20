📊 Sydney Transport Trends (2016–2025)

DATA5002 – Data Visualisation Project

A Shiny dashboard exploring how COVID-19 disrupted and reshaped public transport usage in New South Wales.
The app visualises long-term trends across buses, trains, light rail, metro, and ferries, showing how travel behaviour shifted before, during, and after the pandemic.

📁 Project Structure
DATA5002/
│
├── app.R                      # Full Shiny app (UI + server + all plots)
│
├── data/                      # Cleaned datasets used by the app
│   ├── all_modes.csv
│   ├── bus_clean.csv
│   ├── train.csv
│   ├── lightrail.csv
│   └── other mode files…
│
├── www/                       # Images/icons (e.g., navbar logo)
│
├── README.md                  # Documentation (this file)
│
├── DATA5002 PROJECT.Rmd
│
└── DATA5002-PROJECT.nb.html

🚀 How to Run the App
1. Install required packages
   install.packages(c(
  "shiny", "tidyverse", "lubridate", "ggplot2", "plotly",
  "scales", "stringr", "forcats", "ggiraph", "readr"
))
2. Open the project folder in RStudio
3. Run the app
   shiny::runApp("app.R")
The dashboard will open in your browser automatically.

📦 Data Sources

All datasets come from the Transport for NSW Open Data Portal, including:

1. Monthly Opal patronage (tap on/off)
2. Bus contract region data
3. Train line-level monthly trips
4. Light rail usage
5. Metro and ferry patronage where available
6. Data covers the period 2016–2025.

🔧 Preprocessing Summary

The raw datasets were cleaned using the following steps:

Standardised Year_Month → converted to proper dates
Grouped lines/regions (e.g., Sydney Metro vs Outer Sydney)
Removed duplicates and incomplete rows
Converted all counts to a consistent monthly format
Merged multiple input sources (e.g., train files)

All plots, aggregation logic, and visual storytelling are generated inside app.R.
No manual graphic editing (Photoshop, etc.) was used.

🖥️ Dashboard Features
Overview Page
Total public transport patronage from 2016–2025

Modes & COVID Impact
Compare buses, trains, metro, ferries, and light rail
Mode share breakdown
Interactive filters
Decline/recovery rates during COVID waves

Light Rail Analysis
Growth patterns
Why it recovered faster than other modes

Train Analysis
Line-level patterns
Recovery speeds
Structural insights

Bus Analysis
Sydney Metro vs Outer Sydney
Slower post-COVID recovery

Conclusion
Summary of long-term behavioural shifts in Sydney’s mobility
