library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(RSQLite)
library(tibble)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)


# Establish database connection (or create database)
db <- dbConnect(SQLite(),"poobase.sqlite")


# Functions to interact with the databas
log_value <- function(event,db,table){
  current_table <- dbReadTable(db,table)
  cur_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if(nrow(current_table) == 0) cur_id = 1 else cur_id = max(current_table$id)+1
  next_values <- tibble(id = cur_id,time = cur_time,event = event)
  dbWriteTable(db,table,next_values,append = T)
}

create_table <- function(name,db){
  stopifnot(is.character(name))
  table_to_write <- tibble(id = integer(),time = character(),event = character())
  dbWriteTable(conn = db,name = name, table_to_write)
}

retrive_table <- function(name,db){
  cur_table <- dbReadTable(db,name) |> as_tibble()
  cur_table
}

reset_table <- function(name,db){
  command <- dbExecute(conn = db,paste("DROP TABLE",name))
  create_table(name,db)
}

# Create table in database if necessary
if(!("pootable" %in% dbListTables(db))) create_table("pootable",db)


get_time_since <- function(cond){
  cur_table<-retrive_table("pootable",db)
  last_time <- cur_table[cur_table$event %in% cond,2] |>
    tail(n=1)|>pull()
  last_seconds <- as.duration((as_datetime(last_time)-Sys.time()))|>
    round()|>
    as.numeric()
  last_hour <- abs(floor(last_seconds/60/60))
  last_minute <- abs(60-round((last_seconds/60) %% 60))
  if(last_hour < 10) last_hour <- paste0(0,as.character(last_hour)) else last_hour <- as.character(last_hour)
  if(last_minute < 10) last_minute <- paste0(0,as.character(last_minute)) else last_minute <- as.character(last_minute)
  list(last_hour,last_minute)
}

#server functions
server <- function(input, output, session) {
  #logging
  # Modals for similar logging events
  observeEvent(input$feed_1,{log_value(event = "Links",db = db,table = "pootable")})
  observeEvent(input$feed_2,{log_value(event = "Rechts",db = db,table = "pootable")})
  observeEvent(input$feed_3,{log_value(event = "Links + Rechts",db = db,table = "pootable")})
  observeEvent(input$poo_1 ,{log_value(event = "Pipi",db = db,table = "pootable")})
  observeEvent(input$poo_2 ,{log_value(event = "Kacka",db = db,table = "pootable")})
  observeEvent(input$poo_3 ,{log_value(event = "Pipi + Kacka",db = db,table = "pootable")})

  observeEvent(input$delete_last,
               {dbExecute(conn = db,paste0("DELETE FROM pootable WHERE ID=",nrow(retrive_table("pootable",db))))})
  observeEvent(input$reset_table,
               {f7Dialog(
                 id = "reset_table_dialog",
                 title = "Löschen aller Daten",
                 text = "Sicher, dass alle Daten gelöscht werden sollen?",
                 type = "confirm"
               )})
  observeEvent(input$reset_table_dialog,
               {reset_table("pootable",db)})


  # log of the last five entries into the db
  # refreshes if something is submitted or deleted from the db
  lpt<-reactive({
    cur_table<-retrive_table("pootable",db)
    nrow_cur_table <-nrow(cur_table)
    tail(cur_table,n=pmin(5,nrow_cur_table))[rev(1:pmin(5,nrow_cur_table)),]
  })|> bindEvent(input$feed_1,
                 input$feed_2,
                 input$feed_3,
                 input$poo_1,
                 input$poo_2,
                 input$poo_3,
                 input$delete_last,
                 input$reset_table_dialog,ignoreNULL = T)
  output$last_poo <- renderUI({
    f7Table(lpt())
  })

  # Analysis
  # Timers since the last poo/feed
  render_feed_timer <- reactive({
    invalidateLater(30000, session)
    cur_times<-get_time_since(c("Links","Rechts","Links + Rechts"))
    paste0(cur_times[[1]],":",cur_times[[2]])
  }) |> bindEvent(input$feed_1,
                  input$feed_2,
                  input$feed_3,
                  input$delete_last,
                  input$reset_table_dialog,ignoreNULL = T)
  output$timer_feed <- renderText({render_feed_timer()})

  render_poo_timer <- reactive({
    invalidateLater(30000, session)
    cur_times<-get_time_since(c("Pipi","Kacka","Pipi + Kacka"))
    paste0(cur_times[[1]],":",cur_times[[2]])})|>
    bindEvent(input$poo_1,
              input$poo_2,
              input$poo_3,
              input$delete_last,
              input$reset_table_dialog,ignoreNULL = T)
  output$timer_poo <- renderText({render_poo_timer()})
  #output$test <- renderPlot(ggplot(retrive_table("pootable",db),
  #                      aes(time,as.factor(event))))
}
