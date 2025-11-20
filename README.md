DATA5002 – Sydney Transport Analysis (2016–2025)

This repository contains the code, data, and final Shiny application for analysing how COVID-19 affected public transport patterns in Sydney. The project visualises long-term trends across all major transport modes and highlights how travel behaviour has changed from the pre-pandemic era into the “new normal”.

📁 Project Contents
app.R

The main Shiny application.
Includes:

Data loading and cleaning

UI layout and navigation

All interactive plots (line charts, bar charts, pie charts, bump charts)

Storytelling text and explanations for each mode

/data/

Contains all cleaned datasets used by the Shiny app, including:

Bus trips

Train trips

Light rail trips

Ferry trips

Metro trips

Combined monthly totals

Preprocessed COVID comparison data

README.md

Documentation explaining:

Project structure

How to run the Shiny application

Data sources

Manual preprocessing steps

/assets/ (optional)

Images, icons, and additional visual elements used inside the Shiny app.

/src/ (optional)

Scripts used in early data cleaning before the final datasets were produced.

▶️ How to Run the Shiny App

1. Install required packages:

install.packages(c(
  "shiny", "tidyverse", "lubridate", "ggplot2", "plotly",
  "scales", "crosstalk", "ggiraph"
))


2. Open the project folder in RStudio.

3. Run the app:

shiny::runApp("app.R")


Or simply:

runApp()


The dashboard will launch automatically in your browser.

🗂 Data Source

All data was obtained from the Transport for NSW Open Data Portal.
Datasets include:

Opal tap-on/off patronage data

Bus contract region data

Train line usage

Light rail line-level monthly trips

Ferry and metro usage where available

🔧 Manual Data Processing

Before using the datasets in Shiny, the following preprocessing steps were performed manually:

Converting Year_Month into proper date formats

Fixing inconsistent line or region names

Removing duplicate rows

Grouping excessive line categories (e.g., train lines) to make graphs readable

Ensuring consistent monthly coverage between 2016–2025

Combining multiple files into unified monthly datasets

No manual visual editing (e.g., Photoshop) was used. All charts are generated programmatically.

🎯 Final Output

The Shiny dashboard includes:

Overview page with total trips + multi-mode trends

Mode comparison section

Deep dives on Bus, Train, and Light Rail

COVID impact visualisations

Interactive charts with hover-effects, filters, and sliders

Storytelling sections explaining trends and insights
