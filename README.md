📊 Sydney Transport Trends (2016–2025)
DATA5002 – Data Visualisation Project

A Shiny dashboard exploring how COVID-19 disrupted and reshaped public transport usage in New South Wales. The app visualises long-term trends across buses, trains, light rail, metro, and ferries, showing how travel behaviour shifted before, during, and after the pandemic.
app.R

📁 Project Structure
DATA5002/
│
├── app.R                 # Full Shiny app (UI + server + all plots)
│
├── data/                 # Cleaned datasets used by the app
│   ├── all_modes.csv
│   ├── bus_clean.csv
│   ├── train.csv
│   ├── lightrail.csv
│   └── other mode files…
│
├── www/               # Images/icons (e.g., logo used in navbar)
│
└── README.md             # Documentation (this file)
│
│
└── DATA5002 PROJECT.Rmd
│
│
└── DATA5002-PROJECT.nb.html

🚀 How to Run the App

1. Install required packages
   install.packages(c(
  "shiny", "tidyverse", "lubridate", "ggplot2", "plotly",
  "scales", "stringr", "forcats", "ggiraph", "readr"))
2. Open the folder in RStudio
3. Run the app
   shiny::runApp("app.R")

The dashboard will open in your browser automatically.

📦 Data Sources

All datasets come from the Transport for NSW Open Data Portal, including:
Monthly Opal patronage (tap on/off)
Bus contract region data
Train line-level monthly trips
Light rail line usage
Metro & ferry patronage where available
Data covers the period 2016–2025.

🔧 Preprocessing Summary

Before being loaded into the Shiny app, the raw datasets underwent:
Standardising Year_Month and converting to proper dates
Grouping lines/regions (e.g., Sydney Metro vs Outer Sydney)
Removing duplicates or incomplete rows
Converting all counts into consistent monthly format
Merging multiple source files (e.g., train datasets)
Everything else (plots, aggregation, storytelling) is generated inside app.R.
No Photoshop or manual graphic editing was used.

🖥️ What the Dashboard Provides

Overview Page
Total patronage trends from 2016–2025
Modes & COVID Impact
Compare buses, trains, metro, ferries, and light rail
Interactive filters + mode share + decline/recovery rates
Light Rail Analysis
Why this mode grew the fastest, even during COVID
Train Analysis
Line-level behaviour, recovery patterns, and network structure
Bus Analysis
Metro vs outer-Sydney differences + slow post-COVID recovery
Conclusion
Summary of major behavioural shifts in Sydney mobility
