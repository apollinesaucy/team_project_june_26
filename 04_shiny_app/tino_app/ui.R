# In this app, we show a relative metric of forecasted mortality risk due to heat exposure for Switzerland
# through a map for a selected day (today - today+5days) and for selected postal codes
# over the entire 5-day forecast.


# packages
library(lubridate); library(sf); library(tidyverse); library(shiny); library(leaflet)
library(httr); library(jsonlite)

# load data
rm(list = ls())
impacts <- read_csv("data/dummy_shiny_data.csv") |>
    mutate(date_EU = format(timestep, "%d.%m.%y"))
shp_distr <- read_sf("/Volumes/FS/_ISPM/CCH/AnnualTeamProject2026/Boundaries_G1_District_20260101/Boundaries_G1_District_20260101.shp") |>
    dplyr::select(BEZNAME, geometry)
shp_distr <- st_transform(shp_distr, crs = 4326)

# extract dates from data
unique_dates_US <- unique(impacts$timestep)

# User interface
ui <- fluidPage(
  # title
    titlePanel("Heat mortality risk forecast for Switzerland"),

    # date choice button
    tags$div(
      style = "display: flex; gap: 8px; padding: 8px 15px;",
      lapply(seq_along(unique_dates_US), function(i) {
        actionButton(
          inputId = paste0("btn_date_", i),
          label   = unique_dates_US[i],
          style   = "flex: 1; border-radius: 6px; font-weight: 500;
                 background-color: #f5f5f5; border: 1px solid #ddd;
                 color: #333; height: 38px;") }) ),

    # map
    leafletOutput("heat_map", height = "600px"),

    tags$div(
      style = "padding: 8px 15px;",
      textInput("address_input",
                label       = "Search address:",
                placeholder = "e.g. Bundesplatz 3, Bern"),
      uiOutput("address_suggestions")
    ),
    plotOutput("risk_plot", height = "250px", width = "700px")


)

source("04_shiny_app/tino_app/server.R")

shinyApp(ui = ui, server = server)

