library(shiny)
library(shinyMobile)
library(RSQLite)
library(tibble)
library(DBI)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)


# Establish database connection (or create database)
db <- dbConnect(SQLite(),"/srv/shiny-server/poosrv/poobase.sqlite")


# Functions to interact with the databas
log_value <- function(event,db,table,cur_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")){
  current_table <- dbReadTable(db,table)
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

get_time_since_min <- function(cond){
  cur_table<-retrive_table("pootable",db)
  last_time <- cur_table[cur_table$event %in% cond,2] |>
    pull()|>
    sort()
  last_time <- last_time[length(last_time)]
  last_seconds <- as.duration((as_datetime(last_time)-Sys.time()))|>
    round()|>
    as.numeric()
  round(abs(last_seconds/60))
}

# analysis functions
get_poo_analysis_data <- function(){
  data<-dbReadTable(db,"pootable") |>
    as_tibble() |>
    mutate(time = map_dbl(time,as_datetime))|>
    arrange(time)
  poo_data<-data |>
    dplyr::filter(str_detect(event,"Kacka")) |>
    mutate(time = as_datetime(time),
           diff_to_last = c(NA_real_,map2_dbl(time[-length(time)],time[-1],
                                              \(left,right){
                                                difftime(left,right,units="mins") |> as.double()
                                              })) |>
        abs() |>
        round()) |>
    drop_na() |>
    filter(diff_to_last < 300) |>
    mutate(diff_to_last_lag = lag(diff_to_last)) |>
    drop_na()

  poo_data
}

prob_of_event <- function(cur_time,time_ahead,shape,rate){
  pgamma(cur_time+time_ahead,shape,rate)
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

  # db manipulation buttons
  # delete the last entry
  observeEvent(input$delete_last,
               {f7Dialog(
                 id = "delete_last_dialog",
                 title = "Löschen des letzten Eintrags",
                 text = "Sicher, dass der letzte Eintrag gelöscht werden soll?",
                 type = "confirm"
               )})
  observeEvent(input$delete_last_dialog,
               {dbExecute(conn = db,paste0("DELETE FROM pootable WHERE ID=",nrow(retrive_table("pootable",db))))})

  observeEvent(input$openentr,
               { f7Popup(id = "entrylater",
                         title = "Daten Nachtragen",closeButton = T,
                         f7DatePicker(inputId = "date",label = "Daten eintragen", dateFormat = "yyyy-mm-dd"),
                         f7Stepper(
                           inputId = "stepper_hour",
                           label = "Stunde",
                           min = 0,
                           max = 24,
                           size = "small",
                           value = hour(Sys.time()),
                           wraps = TRUE,
                           autorepeat = TRUE,
                           rounded = FALSE,
                           raised = FALSE,
                           manual = FALSE
                         ),
                         f7Stepper(
                           inputId = "stepper_minute",
                           label = "Minute",
                           min = 0,
                           max = 59,
                           size = "small",
                           value = minute(Sys.time()),
                           wraps = TRUE,
                           autorepeat = TRUE,
                           rounded = FALSE,
                           raised = FALSE,
                           manual = FALSE
                         ),
                         f7SmartSelect(inputId = "data_entry_event",
                                       label="Eventtyp",
                                       choices = c("Links","Rechts","Links + Rechts",
                                                   "Pipi","Kacka","Pipi + Kacka"),
                                       openIn = "sheet"),
                         f7Button(inputId = "log_data_entry","Eintragen")
                        )
                 })
  observeEvent(input$log_data_entry,
               {log_value(event = input$data_entry_event,
                          db = db,
                          table = "pootable",
                          cur_time = paste0(input$date,
                                            " ",
                                            ifelse(input$stepper_hour < 10,
                                                   paste0(0,input$stepper_hour),
                                                   input$stepper_hour),
                                            ":",
                                            ifelse(input$stepper_minute < 10,
                                                   paste0(0,input$stepper_minute),
                                                   input$stepper_minute),
                                            ":",
                                            "00") )})

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
    setNames(tail(cur_table,n=pmin(5,nrow_cur_table))[rev(1:pmin(5,nrow_cur_table)),2:3],
             c("Uhrzeit","Event")
    )
  })|> bindEvent(input$feed_1,
                 input$feed_2,
                 input$feed_3,
                 input$poo_1,
                 input$poo_2,
                 input$poo_3,
                 input$delete_last_dialog,
                 input$reset_table_dialog,
                 input$log_data_entry,
                 ignoreNULL = T)
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
                  input$delete_last_dialog,
                  input$log_data_entry,
                  input$reset_table_dialog,ignoreNULL = T)
  output$timer_feed <- renderText({render_feed_timer()})

  render_poo_timer <- reactive({
    invalidateLater(30000, session)
    cur_times<-get_time_since(c("Pipi","Kacka","Pipi + Kacka"))
    paste0(cur_times[[1]],":",cur_times[[2]])})|>
    bindEvent(input$poo_1,
              input$poo_2,
              input$poo_3,
              input$delete_last_dialog,
              input$log_data_entry,
              input$reset_table_dialog,ignoreNULL = T)
  output$timer_poo <- renderText({render_poo_timer()})
  render_poo_prob <- reactive({
    poo_data <- get_poo_analysis_data()

    #gamma_model <- MASS::fitdistr(poo_data$diff_to_last,"Gamma")
    shape <- 3.99
    rate <- 0.0267
    prob<-prob_of_event(get_time_since_min(c("Kacka","Pipi + Kacka")),30,shape = shape ,rate = rate)|>
      round(2)
    paste0(prob*100,"%")})|>
    bindEvent(input$poo_1,
              input$poo_2,
              input$poo_3,
              input$delete_last_dialog,
              input$log_data_entry,
              input$reset_table_dialog,ignoreNULL = T)
  output$prob_poo <- renderText({render_poo_prob()})
  #output$prob_poo <- renderText({get_time_since_min(c("Kacka","Pipi + Kacka"))})
  #output$test <- renderPlot(ggplot(retrive_table("pootable",db),
  #                      aes(time,as.factor(event))))
}
