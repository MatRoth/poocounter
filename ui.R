library(shiny)
library(DBI)
library(shinythemes)
library(RSQLite)
library(tibble)
library(DBI)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("PooTracker"),
  tabsetPanel(
    tabPanel(
      "Data tracking",
      fluidRow(
        title = "Select ",
        actionButton("log_feed","Feeding"),
        actionButton("log_poo","Poo"),
        actionButton("log_pee","Pee"),
        actionButton("log_scream","Scream"),
        actionButton("delete_last","delete last log")),
      fluidRow(
        tableOutput("last_poo"))
    ),
    tabPanel(
      "Data analysis"

    )
  ),
)
