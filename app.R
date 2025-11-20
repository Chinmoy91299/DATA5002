# app.R

library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(plotly)
library(scales)
library(stringr)
library(forcats)
library(ggiraph)


# DATA LOADING & PREP(GLOBAL DATA)------------------------------------------------------------------------------------------------

# ---------- All modes (overall & by mode) -------------------------------------------------------------------------
df_all <- read_csv("data/all_modes.csv")

df_all$DATE <- as.Date(paste0("01-", df_all$Year_Month), format = "%d-%b-%y")

df_monthly <- df_all %>%
  group_by(DATE, Travel_Mode) %>%
  summarise(
    Trips = sum(Trip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DATE) %>%
  mutate(
    Travel_mode = tolower(Travel_Mode),
    Travel_Mode = case_when(
      Travel_Mode %in% c("bus") ~ "Bus",
      Travel_Mode %in% c("train") ~ "Train",
      Travel_Mode %in% c("metro") ~ "Metro",
      Travel_Mode %in% c("ferry") ~ "Ferry",
      Travel_Mode %in% c("light rail", "lightrail") ~ "Light Rail",
      TRUE ~ Travel_Mode
    )
  ) %>%
  filter(
    !Travel_Mode %in% c("unallocated", "unknown", "")
  )

df_total <- df_monthly %>%
  group_by(DATE) %>%
  summarise(
    Trips_total = sum(Trips),
    .groups = "drop"
  )

# For mode-level interactive filtering
df_monthly1 <- df_monthly %>%
  mutate(Year = as.integer(format(DATE, "%Y")))

mode_year_min <- min(df_monthly1$Year, na.rm = TRUE)
mode_year_max <- max(df_monthly1$Year, na.rm = TRUE)

# ---------- Pre / During / Post COVID comparisons ----------
df_year <- df_monthly %>%
  mutate(
    Year = format(DATE, "%Y")
  ) %>%
  group_by(Year, Travel_Mode) %>%
  summarise(
    Trips = sum(Trips, na.rm = TRUE),
    .groups = "drop"
  )

df_precovid <- df_year %>% filter(Year >= "2016", Year <= "2019")
df_duringcovid <- df_year %>% filter(Year >= "2020", Year <= "2022")
df_postcovid <- df_year %>% filter(Year >= "2023", Year <= "2025")

df_preavg <- df_precovid %>%
  group_by(Travel_Mode) %>%
  summarise(average_trips = mean(Trips, na.rm = TRUE), .groups = "drop")

df_duringavg <- df_duringcovid %>%
  group_by(Travel_Mode) %>%
  summarise(average_trips = mean(Trips, na.rm = TRUE), .groups = "drop")

df_postavg <- df_postcovid %>%
  group_by(Travel_Mode) %>%
  summarise(average_trips = mean(Trips, na.rm = TRUE), .groups = "drop")

df_compare <- df_preavg %>%
  left_join(df_duringavg, by = "Travel_Mode") %>%
  left_join(df_postavg, by = "Travel_Mode") %>%
  rename(
    Pre_Avg    = average_trips.x,
    During_Avg = average_trips.y,
    Post_Avg   = average_trips
  ) %>%
  mutate(
    decline_rate  = (During_Avg - Pre_Avg) / Pre_Avg,
    recovery_rate = Post_Avg / Pre_Avg,
    increase_rate = (Post_Avg - During_Avg) / During_Avg,
    drop_rate2    = (Post_Avg - Pre_Avg) / Pre_Avg
  )

df_longcompare <- df_compare %>%
  select(Travel_Mode, Pre_Avg, During_Avg, Post_Avg) %>%
  pivot_longer(
    cols = c(Pre_Avg, During_Avg, Post_Avg),
    names_to = "Period",
    values_to = "Trips"
  )

df_longcompare$Period <- factor(
  df_longcompare$Period,
  levels = c("Pre_Avg", "During_Avg", "Post_Avg"),
  labels = c("Pre-COVID", "During COVID", "Post-COVID")
)

# ---------- Mode share & bump chart ----------
df_share_mode <- df_year %>%
  group_by(Year) %>%
  mutate(
    year_total = sum(Trips),
    share      = Trips / year_total
  ) %>%
  ungroup()

df_rank_mode <- df_share_mode %>%
  group_by(Year) %>%
  arrange(desc(share)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(Year = as.numeric(Year))



# TRAINS---------------------------------------------------------------------

train <- read.csv("data/train.csv")

train_clean <- train %>%
  mutate(
    DATE_str = paste0("01-", Year_Month),
    DATE     = parse_date_time(DATE_str, orders = "d-b-y", locale = "C"),
    DATE     = as.Date(DATE),
    Year     = year(DATE),
    Month    = month(DATE, label = TRUE, abbr = TRUE),
    Card_type = str_trim(Card_type),
    Line      = str_trim(Line)
  ) %>%
  filter(!is.na(DATE)) %>%
  arrange(DATE)

# Total trips per line per date
train_total <- train_clean %>%
  group_by(DATE, Line) %>%
  summarise(
    Trips_total = sum(Trip, na.rm = TRUE),
    .groups     = "drop"
  )

# Total trips across all lines per date
train_total1 <- train_clean %>%
  group_by(DATE) %>%
  summarise(
    Trips_total = sum(Trip, na.rm = TRUE),
    .groups     = "drop"
  )

# For annual share by line
df_train_year <- train_total %>%
  mutate(
    DATE = as.Date(DATE),
    Year = as.integer(format(DATE, "%Y"))
  )

# BUSES------------------------------------------------------------------------------

bus <- read.csv("data/bus_clean.csv")

bus <- bus %>%
  mutate(
    Date = as.Date(paste0("01-", Year_Month), format = "%d-%b-%Y")
  )

monthly_total <- bus %>%
  group_by(Date) %>%
  summarise(Total_Trips_All = sum(Trip, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)

bus_year_min <- min(year(monthly_total$Date), na.rm = TRUE)
bus_year_max <- max(year(monthly_total$Date), na.rm = TRUE)

monthly_period <- monthly_total %>%
  mutate(
    Period = case_when(
      Date < as.Date("2020-03-01") ~ "Pre-COVID",
      Date < as.Date("2021-12-01") ~ "During COVID",
      TRUE ~ "Post-COVID"
    )
  )

bus_period_avg <- monthly_period %>%
  group_by(Period) %>%
  summarise(Average = mean(Total_Trips_All, na.rm = TRUE), .groups = "drop")

region_compare <- bus %>%
  mutate(
    Region_group = case_when(
      str_detect(Contract_region, "^Night Ride") ~ "Night Ride",
      str_detect(Contract_region, "^Sydney Metro Contract") |
        str_detect(Contract_region, "^Greater Sydney Contract") ~ "Sydney Metro",
      str_detect(Contract_region, "^Outer Sydney Metro Contract") |
        str_detect(Contract_region, "^NISC") |
        str_detect(Contract_region, "^SBSC") ~ "Outer Sydney",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Region_group %in% c("Night Ride", "Sydney Metro", "Outer Sydney")) %>%
  group_by(Date, Region_group) %>%
  summarise(
    Total = sum(Trip, na.rm = TRUE),
    .groups = "drop"
  )

region_compare_filtered <- region_compare %>%
  filter(Region_group %in% c("Sydney Metro", "Outer Sydney"))

#Light rail ----------------------------------------------------

df_lr <- read.csv("data/lightrail.csv")

df_lr1 <- df_lr %>%
  mutate(
    DATE = case_when(
      nchar(Year_Month) == 6 ~ as.Date(paste0("01-", Year_Month), format = "%d-%b-%y"),
      nchar(Year_Month) == 8 ~ as.Date(paste0("01-", Year_Month), format = "%d-%b-%Y"),
      TRUE ~ NA_Date_
    )
  ) %>%
  group_by(DATE, Line) %>%
  summarise(
    Trips = sum(Trip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DATE)

# monthly & yearly totals (all lines combined)
df_lrc_monthly <- df_lr1 %>%
  group_by(DATE) %>%
  summarise(total_trips = sum(Trips, na.rm = TRUE), .groups = "drop")

df_lrc_yearly <- df_lrc_monthly %>%
  mutate(year = lubridate::year(DATE)) %>%
  group_by(year) %>%
  summarise(total_trips = sum(total_trips), .groups = "drop")

# yearly totals by line
df_lrc_linemonthly <- df_lr1 %>%
  group_by(DATE, Line) %>%
  summarise(total_trips = sum(Trips, na.rm = TRUE), .groups = "drop")

df_lrc_lineyearly <- df_lrc_linemonthly %>%
  mutate(year = lubridate::year(DATE)) %>%
  group_by(year, Line) %>%
  summarise(total_trips = sum(total_trips), .groups = "drop")

# dataframe with Year column for interactive stuff
df_lrc1 <- df_lr1 %>%
  mutate(
    Year = as.integer(format(DATE, "%Y"))
  )


# ----------------------------------------UI-----------------------------------------------------------

ui <- navbarPage(
  title = tags$a(
    href = "#",
    class = "navbar-brand",
    onclick = "$('#mainnav a[data-value=\"Overview\"]').tab('show');",
    tags$img(
      src    = "covid.png",
      height = "36",
      style  = "display:inline-block; margin-top:-4px;"
    )
  ),
  id = "mainnav",   # used by the JS above
  header = tags$head(
    
    # Load Google Font
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"
    ),
    
    # Apply Inter font everywhere
    tags$style(HTML("
    body, .navbar, .tab-pane, .shiny-input-container, .container-fluid, p, li, div, span {
      font-family: 'Inter', sans-serif !important;
    }

    h1, h2, h3, h4, h5, h6 {
      font-family: 'Inter', sans-serif !important;
      font-weight: 600;
      letter-spacing: -0.3px;
    }

    /* --------------------------------------------------------------Navbar--------------------------------------------------------------------------- */
    .navbar.navbar-default {
      background-color: #111827;
      border-color: #111827;
    }

    .navbar-default .navbar-brand img {
      max-height: 40px;
    }

    .navbar-default .navbar-brand {
      padding-top: 5px !important;
      padding-bottom: 5px !important;
      height: 50px;
    }
    
    .navbar-default .navbar-nav > li > a,
    .navbar-default .navbar-nav > li > a:visited,
    .navbar-default .navbar-brand,
    .navbar-default .navbar-brand:visited {
      color: #ffffff !important;
      font-weight: 500 !important;
      font-size: 15px !important;
    }
    
    /*hover*/
    .navbar-default .navbar-nav > li > a:hover,
    .navbar-default .navbar-nav > li > a:focus {
      color: #ffffff !important;
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {
    background-color: rgba(255, 255, 255, 0.20) !important;
    color: #ffffff !important;
    }
  "))
  ),
  
#--------------------------------Panels-----------------------------------------------------------------

#---------------------------------------------Overview--------------------------------------------------
  tabPanel(
    "Overview",
    div(style="max-width: 1100px; margin: auto; padding: 20px;",
      
        tags$h1("1. How COVID-19 Changed Transport Patterns in NSW",
                style="font-weight:700; margin-bottom:10px;"),
        
        tags$hr(),
        
        #intro
        div(style="font-size:16px; line-height:1.6; margin-bottom:25px;",
            tags$p("Before 2020, NSW’s public transport system followed a predictable daily rhythm. 
                  Millions of passengers relied on buses, trains, light rail, and ferries for work, 
                  study, and travel. But when COVID-19 arrived, that rhythm broke almost overnight."),
            tags$p("This dashboard explores how each mode of transport reacted during the pandemic, 
                  how quickly services recovered after key lockdowns, and which patterns have 
                  permanently changed in the post-COVID era. The goal is not just to visualise data, 
                  but to understand how Sydney’s movement patterns evolved during one of the most 
                  disruptive periods in modern history.")
        ),
        
        tags$br(),
        tags$hr(),
        
        #Sections 1
        #Hypothesis
        tags$h2("1. What This Analysis Explores"),
        tags$br(),
        
        div(style="
          background:#f3f4f6;
          padding:18px;
          border-radius:10px;
          margin-bottom:25px;
          border-left:6px solid #3b82f6;",
            tags$ul(
              tags$li("How public transport usage collapsed during the 2020 and 2021 lockdowns."),
              tags$li("Which modes recovered the fastest, and which lagged behind."),
              tags$li("Whether NSW’s post-COVID travel behaviour returned to ‘normal’ — or created a new one."),
            )
        ),
        
        tags$br(),
        tags$hr(),
        
        #Sections 2
        tags$h2("2. NSW's Transport Story at a Glance"),
        tags$p("The chart below shows how total public transport trips changed from 2016 to 2025. 
              Notice the stability before 2020, the dramatic collapses during lockdowns, and the 
              uneven recovery that followed:"),
        
        #Plot
        plotlyOutput("totalTripsPlot"),
        
        #Plot interpretation
        div(style="margin-top:15px; font-size:15px; line-height:1.6;",
            tags$p(tags$b("What the plot reveals:")),
            tags$ul(
              tags$li("The first collapse in early 2020 was the sharpest drop in recorded transport history."),
              tags$li("The Delta lockdown in mid-2021 caused a second disruption, though smaller."),
              tags$li("Post-COVID ridership rises again, but never returns fully to pre-pandemic levels — 
                    suggesting a permanent behavioural shift.")
            )
        ),
        
        tags$br(),
        tags$hr(),

        
        #Section 3
        tags$h2("3. Where the Story Goes Next"),
        tags$p("The Overview gives a city-wide picture. The next sections break the story down by mode:"),
        tags$ul(
          tags$li(tags$b("Modes & COVID Impact: "), "How each mode was affected differently."),
          tags$li(tags$b("Light Rail: "), "Why its usage rose even during the pandemic."),
          tags$li(tags$b("Trains: "), "Which lines and card types changed the most."),
          tags$li(tags$b("Buses: "), "How region-specific recovery shaped overall mobility.")
        ),
        
        tags$br()
    )
  ),

#------------------------------All modes----------------------------------------------------------
  
  tabPanel(
    "Modes & COVID Impact",
    div(style = "max-width:1100px; margin:auto; padding:20px;",
        
        #title
        tags$h1("How Different Transport Modes Reacted to COVID-19",
                style = "font-weight:700; margin-bottom:10px;"),
        tags$hr(),
        
        #intro
        div(style = "font-size:16px; line-height:1.6; margin-bottom:25px;",
            tags$p("COVID-19 did not affect every transport mode in the same way. 
                  Trains and buses saw sharp collapses in usage, while metro and 
                  light rail followed different recovery paths. This section looks 
                  at how each mode behaved before, during, and after the pandemic."),
            tags$p("The goal here is to understand not just how much people travelled, 
                  but how their preferences across modes shifted over time.")
        ),
        
        #Overall trend plot
        #section1
        tags$h2("1. Overall Trends by Mode"),
        tags$br(),
        
        div(style = "background:#f9fafb; padding:15px; border-radius:10px; margin-bottom:20px;",
            tags$p(tags$b("Use the controls below to explore trends by mode and year range:"))
        ),
        
        # Filters row
        fluidRow(
          column(
            width = 4,
            tags$h4("Filters", style = "margin-top:0;"),
            selectInput(
              "mode_select",
              "Select modes",
              choices  = sort(unique(df_monthly1$Travel_Mode)),
              selected = unique(df_monthly1$Travel_Mode),
              multiple = TRUE
            ),
            sliderInput(
              "mode_year_range",
              "Year range",
              min = min(df_monthly1$Year, na.rm = TRUE),
              max = max(df_monthly1$Year, na.rm = TRUE),
              value = c(min(df_monthly1$Year, na.rm = TRUE),
                        max(df_monthly1$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
          ),
          
        #plot
          column(
            width = 8,
            tags$p("The line and bar charts below show monthly trips by mode across the selected years."),
            plotlyOutput("mode_line_plot"),
            tags$br(),
            plotlyOutput("mode_bar_plot")
          )
        
        ),
        
        tags$br(),
        
        div(style = "margin-top:10px; font-size:15px; line-height:1.6;",
            tags$b("Key observations:"),
            tags$ul(
              tags$li("Bus and train usage drop sharply during the main lockdown periods in 2020 and 2021."),
              tags$li("Metro and light rail show quicker and more consistent rebounds after restrictions ease."),
              tags$li("Ferry usage is more volatile and slower to recover compared with land-based modes.")
            )
        ),
        
        tags$br(),
        tags$hr(),
        tags$br(),
        
        #Mode Popularity Shifted
        #Section2
        
        tags$h2("2. How Mode Popularity Shifted"),
        tags$p("Even when overall trips dropped, the relative split between modes changed. 
              The pie chart focuses on each year’s share of trips by mode."),
        
        #filters row
        fluidRow(
          column(
            width = 3,
            tags$h4("Year Range"),
            sliderInput(
              "pie_year",
              NULL,
              min   = min(df_monthly1$Year, na.rm = TRUE),
              max   = max(df_monthly1$Year, na.rm = TRUE),
              value = 2019,
              step  = 1,
              sep   = ""
            )
          ),
          
          #plot
          column(
            width = 9,
            plotlyOutput("mode_pie")
          )
        ),
        
        
        tags$br(),
        
        div(style = "font-size:15px; line-height:1.6;",
            tags$p("Buses and trains still dominate total trips, but their combined share 
                  gradually declines. At the same time, metro and light rail gain share, 
                  reflecting slow but noticeable changes in how people move around the city.")
        ),
        
        tags$br(),
        tags$hr(),
        tags$br(),
        
        #Pre-COVID vs During COVID vs Post-COVID
        #section3
        
        tags$h2("3. Pre-COVID vs During COVID vs Post-COVID"),
        tags$p("To quantify the impact of COVID-19, we compare average annual trips for each mode across three periods:"),
        
        tags$ul(
          tags$li(tags$b("Pre-COVID:"), " 2016–2019"),
          tags$li(tags$b("During COVID:"), " 2020–2022"),
          tags$li(tags$b("Post-COVID:"), " 2023–2025")
        ),
        
        tags$br(),
        
        tags$h3("Average Annual Trips by Period"),
        #plot
        plotOutput("covid_group_bar", height = "380px"),
        
        tags$br(),
        
        div(style = "font-size:15px; line-height:1.6;",
            tags$p("Nearly all modes experience a steep drop during the COVID period. 
                  In the post-COVID years, ferry, metro and light rail recover more strongly, 
                  while bus and train remain below their pre-COVID averages.")
        ),
        
        tags$br(),
        
        #change rate plots------------------------------------------------------------------------------------------------
        tags$h3("4. How Much Did Each Mode Decline and Recover?"),
        
        div(style="
          background:#f3f4f6;
          padding:15px;
          border-radius:10px;
          margin-bottom:20px;
          border-left:5px solid #3b82f6;",
            tags$p(tags$b("Interpreting the rate charts:")),
            tags$ul(
              tags$li(tags$b("Decline rate:"), " how far a mode fell during the COVID years compared to pre-COVID."),
              tags$li(tags$b("Increase rate:"), " how much it grew from the COVID period to the post-COVID period."),
              tags$li(tags$b("Recovery rate:"), " how close it came to (or surpassed) its pre-COVID average.")
            )
        ),
        
        plotOutput("covid_decline_rate", height = "320px"),
        tags$br(),
        plotOutput("covid_increase_rate", height = "320px"),
        tags$br(),
        plotOutput("covid_recovery_rate", height = "320px"),
        
        #----------------------------------------------------------------------------------------------------------------------
        
        tags$br(),
        tags$hr(),
        tags$br(),
        
        #Section 4
        
        tags$h2("5. Which Modes Became More Popular Over Time?"),
        tags$p("The ranking chart below shows the relative position of each mode based on its annual share of total trips."),
        
        #plot
        plotOutput("bump_chart", height = "400px"),
        
        tags$br(),
        
        div(style = "font-size:15px; line-height:1.6;",
            tags$p("Bus and train remain the largest modes by volume, but the bump chart shows that light rail 
                  climbs steadily in the rankings. Even when total demand is lower, its relative importance 
                  grows, especially in the post-COVID period."),
            tags$p("This shift hints at deeper structural changes in how people use different types of public transport, 
                  rather than just a temporary pandemic shock.")
        ),
        
        tags$br()
    )
  ),
#-----------------------------------Light Rail------------------------------------------------------------
  tabPanel(
    "Light Rail",
    
    div(
      style = "max-width:1100px; margin:auto; padding:20px; font-size:16px; line-height:1.7;",
      
      #Intro
      tags$h1("Light Rail: From Supporting Actor to Rising Star",
              style = "font-weight:700; margin-bottom:10px;"),
      tags$hr(),
      
      tags$p(
        "In the earlier ranking chart, we saw that although bus and train still carry 
         the highest total volumes, their shares have been slowly declining. Light rail, 
         on the other hand, has been steadily climbing in the rankings."
      ),
      
      tags$p(
        "The climb actually starts before COVID-19, and unlike other modes, light rail 
         appears to be less affected by the pandemic. After only a short dip in 2020, 
         its total trips grow rapidly, with some peaks during the COVID period even 
         exceeding pre-pandemic levels. This makes light rail an interesting outlier 
         worth a closer look."
      ),
      
      tags$br(),
      
      
      # Overall ligh rail growth
      # Section 1
      tags$h2("1. Overall Light Rail Growth Over Time"),
      tags$br(),
      
      tags$p(
        "First, we look at total annual light rail trips, combining all lines. 
         This shows how the mode as a whole has evolved over time."
      ),
      
      #plot
      
      plotlyOutput("lr_total_yearly"),
      
      tags$br(),
      
      tags$p(
        "The pattern is clear: trips increase almost every year. There is a visible dip 
         during the strict COVID lockdowns around 2020–2021, but the recovery is strong 
         and continues upwards afterwards. Unlike buses and trains, light rail does not 
         just return to its old level; it exceeds it."
      ),
      
      tags$br(), tags$hr(), tags$br(),
      
      
      # Annaual trips per line
      # Section 2
      tags$h2("2. How Each Light Rail Line Contributes"),
      tags$br(),
      
      tags$p(
        "However, this growth is not evenly shared across all lines. The chart below 
         breaks annual trips down by individual lines."
      ),
      
      #plot
      plotOutput("lr_line_yearly"),
      
      tags$br(),
      
      tags$p(
        "Before 2020, the Dulwich Hill Line dominates the network and accounts for 
         almost all light rail trips. Starting from around 2018, its share gradually 
         declines, even before the pandemic. At the same time, trips on the Randwick 
         and Kingsford lines grow quickly once they open, and they become the main 
         drivers of light rail growth."
      ),
      
      tags$br(), tags$hr(), tags$br(),
      
      
      # share and time trends
      # Section 3
      tags$h2("3. Line Shares and Time Trends"),
      tags$br(),
      
      tags$p(
        "To understand how the balance between lines changes over time, we look at two 
         perspectives: the share of trips by line in a given year, and the detailed 
         time series for each line."
      ),
      
      # filters
      tags$h3("3.1 Annual Share of Trips by Line"),
      tags$br(),
      
      fluidRow(
        column(
          width = 3,
          sliderInput(
            "lr_pie_year",
            "Select Year",
            min   = min(df_lrc1$Year, na.rm = TRUE),
            max   = max(df_lrc1$Year, na.rm = TRUE),
            value = max(df_lrc1$Year, na.rm = TRUE),
            step  = 1
          )
        ),
        
        #plot
        column(
          width = 9,
          plotlyOutput("lr_pie")
        )
      ),
      
      tags$br(), tags$br(),
      
      tags$p(
        "In early years, Dulwich Hill takes almost 100% of trips. Over time, its share 
         falls sharply as Randwick and Kingsford come online and grow. By the later years, 
         most light rail trips are on these new lines rather than Dulwich Hill."
      ),
      
      tags$br(),
      
      #Trips by Line Over Time
      tags$h3("3.2 Trips by Line Over Time"),
      tags$br(),
      
      #filters
      fluidRow(
        column(
          width = 4,
          tags$h4("Filters"),
          selectInput(
            "lr_line_select",
            "Select Lines",
            choices  = unique(df_lrc1$Line),
            selected = unique(df_lrc1$Line),
            multiple = TRUE
          ),
          sliderInput(
            "lr_year_range",
            "Year Range",
            min   = min(df_lrc1$Year, na.rm = TRUE),
            max   = max(df_lrc1$Year, na.rm = TRUE),
            value = c(min(df_lrc1$Year, na.rm = TRUE),
                      max(df_lrc1$Year, na.rm = TRUE)),
            step  = 1
          )
        ),
        #plot
        column(
          width = 8,
          plotlyOutput("lr_line_ts")
        )
      ),
      
      tags$br(),
      
      tags$p(
        "The time series shows that Dulwich Hill's trips peak before the pandemic, 
         then decline and never fully return to those levels. Randwick and Kingsford, 
         by contrast, rise quickly and continue to grow, even through and after the 
         COVID period. The Kingsford Line is especially notable: it begins during 
         the pandemic yet shows strong growth almost immediately, with only a brief 
         dip around 2021."
      ),
      
      tags$br(), tags$hr(), tags$br(),
      
      
      # Interpretation
      # Section 4
      tags$h2("4. Why Light Rail Behaved Differently"),
      tags$br(),
      
      tags$p(
        "Taken together, these patterns explain why light rail looks so different 
         from buses and trains in the earlier mode comparison charts:"
      ),
      
      tags$ul(
        tags$li(
          "The overall growth in light rail is largely driven by the Randwick and Kingsford lines, 
           not by the older Dulwich Hill corridor."
        ),
        tags$li(
          "Dulwich Hill was already weakening before COVID, so the pandemic accelerated a change 
           that was already underway rather than causing it."
        ),
        tags$li(
          "New infrastructure and strong demand in inner suburbs helped Randwick and Kingsford 
           grow quickly, even when other modes were struggling."
        )
      ),
      
      tags$br(),
      
      tags$h2("Key Takeaway"),
      tags$br(),
      
      tags$p(
        "Although total light rail trips are still much lower than buses or trains, 
         it is the mode with the strongest proportional growth. Its resilience during 
         and after COVID-19, and the rapid expansion of the Randwick and Kingsford lines, 
         make light rail a key part of the 'new normal' in Sydney’s public transport."
      ),
      
      tags$br(), tags$br()
    )
  ),

#--------------------------------------------------Train----------------------------------------------------------------------------  
  tabPanel(
    "Trains",
    
    div(
      style = "max-width:1100px; margin:auto; padding:20px; font-size:16px; line-height:1.7;",
      
      tags$h1("Trains: From Stable Backbone to Volatile Demand",
              style = "font-weight:700; margin-bottom:10px;"),
      tags$hr(),
      
      tags$p("
        Before COVID-19, trains were one of the most stable and heavily used public 
        transport modes in NSW. Monthly trips moved with regular seasonal patterns, 
        but the overall trend was steady. COVID-19 broke that stability, with two 
        major shocks in 2020 and 2021.
      "),
      
      tags$p("
        Using the train dataset, we look at how total trips changed over time, how 
        individual lines behaved, and which parts of the network remained strongest 
        in the recovery period.
      "),
      
      tags$br(),
      
      #Overall trend
      #Section 1
      tags$h2("1. Overall Train Usage Over Time"),
      tags$br(),
      
      tags$p("
        This first chart shows total train trips across all lines. It highlights the 
        long period of stability before 2020, the dramatic collapse during the first 
        lockdown, and the partial recovery that follows.
      "),
      
      #plot
      plotlyOutput("train_total_all"),
      
      tags$br(),
      
      tags$p("
        Trips drop sharply in early 2020, reach a low point during the first lockdown, 
        then slowly climb back. A second dip appears around mid 2021 during the Delta 
        outbreak. Even by the later years, total usage remains below the strongest 
        pre-pandemic levels, which suggests a lasting change in commuting patterns.
      "),
      
      tags$br(), tags$hr(), tags$br(),
      
      #ggiraph plot
      #section 2
      tags$h2("2. How Individual Train Lines Behaved"),
      tags$br(),
      
      tags$p("
        The interactive chart below shows trips by line over time. Each line follows 
        the same overall pattern of collapse and recovery, but the speed and strength 
        of recovery differ between lines.
      "),
      
      #plot
      girafeOutput("train_line_ts", width = "100%", height = "500px"),
      
      tags$br(),
      
      tags$p("
        Before 2020, most lines move in sync with seasonal patterns. During COVID, 
        every line drops, but core urban corridors recover sooner than longer-distance 
        commuter lines. The second wave in 2021 produces another visible dip before 
        patronage climbs again.
      "),
      
      tags$br(), tags$hr(), tags$br(),
      
      #share of trains annually
      #section 3
      tags$h2("3. Annual Share of Trips by Line"),
      tags$br(),
      
      tags$p("
        With many lines in the network, it helps to look at which ones carry the 
        largest shares of trips in a given year. The pie chart below shows the 
        distribution of train trips by line for a selected year.
      "),
      
      #filters
      fluidRow(
        column(
          width = 3,
          sliderInput(
            "train_pie_year",
            "Select Year",
            min   = min(df_train_year$Year, na.rm = TRUE),
            max   = max(df_train_year$Year, na.rm = TRUE),
            value = 2019,
            step  = 1,
            sep   = ""
          )
        ),
        #plot
        column(
          width = 9,
          plotlyOutput("train_line_pie", height = "500px")
        )
      ),
      
      tags$br(),
      
      div(
        style = "
          max-width: 700px;
          margin: 10px auto 0;
          font-size: 15px;
          line-height: 1.6;
          white-space: normal;
          word-wrap: break-word;
      ",
        tags$p(
          "A small group of major lines account for most train trips, while other lines 
          carry much smaller volumes. When COVID hits, it doesn’t just reduce volumes 
          across all lines, but slightly reshapes where demand sits across the network."
          )
        ),
      
      tags$br(), tags$hr(), tags$br(),
      
      #conclusion of train
      #section 4
      tags$h2("What the Train Data Tells Us"),
      tags$br(),
      
      tags$ul(
        tags$li("Train usage was very stable before COVID, then collapsed in 2020 and again in 2021."),
        tags$li("Recovery is only partial, with post-COVID levels remaining below 2018–2019 peaks."),
        tags$li("All lines were affected, but some recovered faster than others depending on who they serve."),
        tags$li("The structure of the network stays similar, with key lines still carrying most of the patronage.")
      ),
      
      tags$br(),
      
      tags$p("
        Overall, trains remain a core part of NSW public transport, but the way they 
        are used has shifted. Hybrid work, fewer full-time office commutes, and changes 
        in daily routines mean that rail demand is unlikely to return to the old \"normal\"."
      ),
      
      tags$br(), tags$br()
    )
  ),

 
 #---------------------------------------------BuS-----------------------------------------------------------------------------------
  tabPanel(
    "Buses",
    
    div(
      style = "max-width:1100px; margin:auto; padding:20px; font-size:16px; line-height:1.7;",
      
      # TITLE
      tags$h1("Buses: The Most Impacted Mode During COVID",
              style = "font-weight:700; margin-bottom:10px;"),
      tags$hr(),
      
      # INTRO BLOCK
      tags$p("Among all public transport modes in NSW, buses experienced the most severe 
              and prolonged decline during COVID-19. Unlike metro and light rail, which 
              bounced back relatively quickly, bus usage dropped dramatically twice — 
              first during the 2020 nationwide lockdown, and again during the Delta 
              outbreak in mid-2021."),
      
      tags$p("Even after restrictions eased, bus patronage did not fully return to 
              pre-pandemic levels. The slower recovery reflects the way hybrid work, 
              changing travel routines, and reduced long-distance commuting reshaped 
              everyday travel in Sydney."),
      
      
      
      # 
      #total bus trips
      #section 1
      tags$br(),
      tags$h2("1. How Bus Usage Changed Over Time"),
      tags$br(),
      
      tags$p("The trend below shows the monthly total number of bus trips. Two major 
              drops stand out:"),
      
      tags$ul(
        tags$li(tags$b("April 2020:"), " the lowest point in NSW public transport history."),
        tags$li(tags$b("July–August 2021:"), " another sharp decline during the Delta lockdown.")
      ),
      
      sliderInput(
        "bus_year_filter",
        "Select Year Range:",
        min = min(year(monthly_total$Date)),
        max = max(year(monthly_total$Date)),
        value = c(min(year(monthly_total$Date)),
                  max(year(monthly_total$Date))),
        step = 1,
        sep = ""
      ),
      
      #plot
      plotlyOutput("bus_total_plot"),
      tags$br(),
      
      tags$p("The key pattern here is that buses recover much more slowly than rail-based 
              modes. Even by 2024–2025, total bus trips remain below the strong 
              2018–2019 levels."),
      
      
      
      #pre/during/post bar
      #section 2
      tags$hr(), tags$br(),
      tags$h2("2. Comparing Bus Usage Before, During, and After COVID"),
      tags$br(),
      
      tags$p("A clearer comparison comes from the average trips in each period. 
              This shows how demand structurally shifted after the pandemic."),
      
      plotOutput("bus_period_avg_plot"),
      tags$br(),
      
      tags$p("The post-COVID average improves compared to the pandemic period, but    
              remains noticeably lower than pre-COVID volumes. This suggests a 
              permanent change in travel behaviour, especially among long-distance commuters."),
      
      
      
      #comparing region
      #section 3
      tags$hr(), tags$br(),
      tags$h2("3. Sydney Metro vs Outer Sydney: Different Recovery Speeds"),
      tags$br(),
      
      tags$p("Grouping the bus contracts into Metro vs Outer Sydney reveals a strong pattern:"),
      
      tags$ul(
        tags$li(tags$b("Sydney Metro region:"), 
                "higher usage and faster recovery after each COVID wave."),
        tags$li(tags$b("Outer Sydney region:"), 
                "lower usage overall and slower return to normal."),
        tags$li("NightRide is excluded because it is not comparable to daytime services.")
      ),
      
      #Filters 
      checkboxGroupInput(
        "bus_region_select",
        "Select Regions:",
        choices = c("Sydney Metro", "Outer Sydney"),
        selected = c("Sydney Metro", "Outer Sydney")
      ),
      #plot
      plotlyOutput("bus_region_plot"),
      tags$br(),
      
      tags$p("The faster recovery in the Sydney Metro area reflects inner-city travel returning 
              earlier than long-distance suburban commuting — a common pattern in post-pandemic urban mobility."),
      
      
      
      #bus conclusion
      #section 4
      tags$hr(), tags$br(),
      tags$h2("Key Takeaway"),
      tags$br(),
      
      tags$p("Unlike light rail and metro, bus usage in NSW has not returned to 
              pre-pandemic levels. The dual shocks of 2020 and 2021 exposed how 
              sensitive bus travel is to changes in work and lifestyle patterns. 
              While demand is recovering steadily, the long-term trend suggests that 
              buses may never fully regain the dominance they held before COVID-19.")
    )
  ),


#-------------------------------Conclusion-----------------------------------------------------------------------
  tabPanel(
    "Conclusion",
    
    div(
      style = "max-width:1100px; margin:auto; padding:20px; font-size:16px; line-height:1.7;",
      
      tags$h1("Conclusion: How COVID-19 Reshaped Transport in NSW",
              style = "font-weight:700; margin-bottom:10px;"),
      tags$hr(),
      
      tags$p("COVID-19 created the most dramatic shock to public transport usage in New South Wales’ 
              modern history. Across buses, trains, metro and ferries, the first major lockdown in early 2020 
              resulted in a sudden collapse in movement, reaching historic lows. A second decline in mid-2021 
              during the Delta outbreak confirmed how strongly transport demand was tied to public health restrictions."),
      
      tags$br(),
      tags$h2("1. A Recovery — But Not a Return to the Old Normal"),
      tags$br(),
      
      tags$p("While ridership gradually increased after restrictions eased, the recovery was uneven across 
              transport modes. Buses and trains both recovered more slowly and remain well below their 
              pre-pandemic levels, suggesting a long-term shift in travel behaviours. Hybrid work, reduced 
              office commutes, and more flexible daily routines continue to shape how people move through 
              the city."),
      
      tags$br(),
      tags$h2("2. Light Rail: The Outlier Story"),
      tags$br(),
      
      tags$p("Light rail stands out as the only mode that not only recovered but exceeded 
              its pre-COVID usage. Its growth was driven largely by the Randwick and Kingsford lines, 
              which continued expanding even during the pandemic. Unlike traditional commuting modes, 
              light rail benefited from new infrastructure and strong inner-city demand."),
      
      tags$br(),
      tags$h2("3. A Transport System in Transition"),
      tags$br(),
      
      tags$ul(
        tags$li("Buses and trains remain essential but have settled into lower demand than before 2020."),
        tags$li("Metro and light rail enjoy more stable and faster recoveries."),
        tags$li("Inner-Sydney travel recovered faster than outer suburban commuting."),
        tags$li("The long-term shift toward flexible work continues to reduce peak-hour pressures.")
      ),
      
      tags$br(),
      tags$h2("4. Final Takeaway"),
      tags$br(),
      
      tags$p("NSW’s transport landscape has not simply bounced back — it has transformed. 
              COVID-19 accelerated changes that were already underway: decentralised travel, 
              more local movement, new transport corridors, and a weaker reliance on daily peak-hour commuting. 
              The system that emerges from this transition is more diverse, more flexible, 
              and shaped by a new relationship between work, home, and mobility."),
      
      tags$br(), tags$br()
    )
  )

)

# --------------------------------------------------SERVER----------------------------------------------------------------
 

server <- function(input, output, session) {
  
  
  
  # ------------------------------ Overview total trips ----------------------------
  output$totalTripsPlot <- renderPlotly({
    p_total <- ggplot(df_total, aes(x = DATE, y = Trips_total)) +
      geom_line(color = "black", linewidth = 0.8) +
      labs(
        title = "Total Public Transport Trips",
        x = "Year",
        y = "Total Trips"
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(
        labels = comma,
        breaks = seq(0, max(df_total$Trips_total) * 1.5, by = 10e6)
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p_total)
  })
  
  # ---------------------- All modes plots -----------------------------------------
  output$mode_line_plot <- renderPlotly({
    df <- df_monthly1 %>%
      filter(
        Year >= input$mode_year_range[1],
        Year <= input$mode_year_range[2]
      )
    
    if (!is.null(input$mode_select) && length(input$mode_select) > 0) {
      df <- df %>% filter(Travel_Mode %in% input$mode_select)
    }
    
    plot_ly(
      df,
      x = ~DATE,
      y = ~Trips,
      color = ~Travel_Mode,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        title = list(text = "Public Transport Trips by Mode (Line)", x = 0.35),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trips", tickformat = ",d")
      )
  })
  
  output$mode_bar_plot <- renderPlotly({
    df <- df_monthly1 %>%
      filter(
        Year >= input$mode_year_range[1],
        Year <= input$mode_year_range[2]
      )
    
    if (!is.null(input$mode_select) && length(input$mode_select) > 0) {
      df <- df %>% filter(Travel_Mode %in% input$mode_select)
    }
    
    plot_ly(
      df,
      x = ~DATE,
      y = ~Trips,
      color = ~Travel_Mode,
      type = "bar"
    ) %>%
      layout(
        title = list(text = "Public Transport Trips by Mode (Bar)", x = 0.35),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trips", tickformat = ",d")
      )
  })
  
  # ----- Mode share pie -----
  output$mode_pie <- renderPlotly({
    df <- df_monthly1 %>%
      filter(Year == input$pie_year) %>%
      group_by(Travel_Mode) %>%
      summarise(Trips = sum(Trips, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      df,
      labels = ~Travel_Mode,
      values = ~Trips,
      type = "pie"
    ) %>%
      layout(
        title = list(text = paste("Annual Share of Trips by Mode in", input$pie_year), x = 0.5)
      )
  })
  
  # ----- COVID comparison plots -----
  output$covid_group_bar <- renderPlot({
    ggplot(df_longcompare, aes(x = Travel_Mode, y = Trips, fill = Period)) +
      geom_col(position = position_dodge(width = 0.8)) +
      scale_y_continuous(
        labels = comma,
        breaks = seq(0, max(df_longcompare$Trips) * 1.5, by = 5e7)
      ) +
      labs(
        title = "Average Annual Public Transport Trips: Pre-, During, Post-COVID",
        x = "Transport Mode",
        y = "Average Annual Trips",
        fill = "Period"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$covid_decline_rate <- renderPlot({
    ggplot(df_compare, aes(x = Travel_Mode, y = decline_rate, fill = Travel_Mode)) +
      geom_col(width = 0.6) +
      scale_y_continuous(labels = percent) +
      labs(
        title = "Change Compared to Pre-COVID Levels (2016–2019 vs 2020–2022)",
        x = "Transport Mode",
        y = "Change Rate",
        fill = "Mode"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$covid_increase_rate <- renderPlot({
    ggplot(df_compare, aes(x = Travel_Mode, y = increase_rate, fill = Travel_Mode)) +
      geom_col(width = 0.6) +
      scale_y_continuous(labels = percent) +
      labs(
        title = "Change Compared to During-COVID Levels (2020–2022 vs 2023–2025)",
        x = "Transport Mode",
        y = "Change Rate",
        fill = "Mode"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$covid_recovery_rate <- renderPlot({
    ggplot(df_compare, aes(x = Travel_Mode, y = recovery_rate, fill = Travel_Mode)) +
      geom_col(width = 0.6) +
      scale_y_continuous(labels = percent) +
      labs(
        title = "Post-COVID Recovery Rate (2023–2025 vs 2016–2019)",
        x = "Transport Mode",
        y = "Recovery Rate",
        fill = "Mode"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # ----- rank plot -----
  output$bump_chart <- renderPlot({
    ggplot(df_rank_mode, aes(x = Year, y = rank, color = Travel_Mode, group = Travel_Mode)) +
      geom_line(size = 1.4) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = sort(unique(df_rank_mode$Year))) +
      scale_y_reverse(breaks = 1:length(unique(df_rank_mode$Travel_Mode))) +
      labs(
        title = "Ranking of Public Transport Modes by Annual Share",
        x = "Year",
        y = "Rank",
        color = "Mode"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8)
      )
  })
  
  # ---------------------- light rail -------------------------------------
  
  # ----  annual trips (all lines) ----
  output$lr_total_yearly <- renderPlotly({
    p <- ggplot(df_lrc_yearly, aes(x = year, y = total_trips)) +
      geom_line(size = 1.1, color = "blue") +
      geom_point(size = 3) +
      scale_x_continuous(breaks = df_lrc_yearly$year) +
      scale_y_continuous(
        labels = comma,
        breaks = seq(0, max(df_lrc_yearly$total_trips) * 1.5, by = 5e6)
      ) +
      labs(
        title = "Annual Trips on Light Rail",
        x = "Year",
        y = "Total Trips"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p)
  })
  
  # ---- annual trips per line (bar) ----
  output$lr_line_yearly <- renderPlot({
    ggplot(df_lrc_lineyearly, aes(x = year, y = total_trips, fill = Line)) +
      geom_col() +
      scale_x_continuous(breaks = df_lrc_yearly$year) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Annual Trips per Light Rail Line",
        x = "Year",
        y = "Annual Trips",
        fill = "Line"
      ) +
      theme(
        plot.title      = element_text(hjust = 0.5, face = "bold"),
        legend.key.size = unit(0.4, "cm"),
        legend.text     = element_text(size = 8)
      )
  })
  
  # ---- Light Rail trips by line ----
  output$lr_line_ts <- renderPlotly({
    df <- df_lrc1 %>%
      filter(
        Year >= input$lr_year_range[1],
        Year <= input$lr_year_range[2]
      )
    
    if (!is.null(input$lr_line_select) && length(input$lr_line_select) > 0) {
      df <- df %>% filter(Line %in% input$lr_line_select)
    }
    
    plot_ly(
      df,
      x     = ~DATE,
      y     = ~Trips,
      color = ~Line,
      type  = "scatter",
      mode  = "lines"
    ) %>%
      layout(
        title = list(text = "Light Rail Trips by Line", x = 0.35),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trips", tickformat = ",d")
      )
  })
  
  # ---- Light Rail: pie chart by line for selected year ----
  output$lr_pie <- renderPlotly({
    df <- df_lrc1 %>%
      filter(Year == input$lr_pie_year) %>%
      group_by(Line) %>%
      summarise(Trips = sum(Trips, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      df,
      labels = ~Line,
      values = ~Trips,
      type   = "pie"
    ) %>%
      layout(
        title = list(
          text = paste("Light Rail Trips by Line in", input$lr_pie_year),
          x    = 0.5
        ),
        margin = list(t = 80)
      )
  })
  
  
  # ---------------------- Train -------------------------------------
  output$train_total_all <- renderPlotly({
    p <- ggplot(train_total1, aes(x = DATE, y = Trips_total)) +
      geom_line(color = "steelblue") +
      scale_y_continuous(labels = comma) +
      labs(
        x     = "Year",
        y     = "Trips",
        title = "Total Train Trips per Month"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p)
  })
  
  output$train_line_ts <- renderGirafe({
    g <- ggplot(
      train_total,
      aes(x = DATE, y = Trips_total, color = Line, group = Line)
    ) +
      geom_line_interactive(aes(tooltip = Line, data_id = Line)) +
      scale_y_continuous(labels = comma) +
      scale_color_viridis_d(guide = "none") +
      labs(
        title = "Total Train Trips per Line",
        x     = "Year",
        y     = "Trips"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    girafe(
      ggobj   = g,
      width_svg  = 8,
      height_svg = 6,
      options = list(
        opts_hover_inv(css = "opacity:0.1;"),
        opts_hover(css = "stroke-width:2;")
      )
    )
  })
  
  output$train_line_pie <- renderPlotly({
    df <- df_train_year %>%
      filter(Year == input$train_pie_year)
    
    plot_ly(
      df,
      labels   = ~Line,
      values   = ~Trips_total,
      type     = "pie",
      textinfo = "none"
    ) %>%
      layout(
        title  = list(text = paste("Annual Train Share of Trips by Line in", input$train_pie_year),
                      x = 0.35),
        margin = list(t = 80),
        height = 500
      )
  })
  
  
  
  
  # ---------------------- buses -------------------------------------
  
  output$bus_total_plot <- renderPlotly({
    df <- monthly_total %>%
      mutate(Year = year(Date)) %>%
      filter(
        Year >= input$bus_year_filter[1],
        Year <= input$bus_year_filter[2]
      )
    
    plot_ly(
      df,
      x = ~Date,
      y = ~Total_Trips_All,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Total Bus Trips in Sydney",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Trips", tickformat = ".2s")
      )
  })
  
  output$bus_period_avg_plot <- renderPlot({
    ggplot(bus_period_avg, aes(Period, Average, fill = Period)) +
      geom_col() +
      scale_y_continuous(
        labels = label_number(scale = 1e-6, suffix = "M")
      ) +
      labs(
        title = "Average Monthly Bus Trips: Pre-, During, Post-COVID",
        x = "",
        y = "Average Trips (Millions)",
        fill = "Period"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$bus_region_plot <- renderPlotly({
    df <- region_compare_filtered
    
    if (!is.null(input$bus_region_select) && length(input$bus_region_select) > 0) {
      df <- df %>% filter(Region_group %in% input$bus_region_select)
    }
    
    plot_ly(
      df,
      x = ~Date,
      y = ~Total,
      color = ~Region_group,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Bus Trips by Contract Region",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Trips", tickformat = ".2s"),
        legend = list(title = list(text = "Region"))
      )
  })
  
}


shinyApp(ui, server)
