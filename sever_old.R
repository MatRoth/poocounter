library(shiny)
library(DBI)
library(shinythemes)
library(RSQLite)
library(tibble)
library(DBI)
library(dplyr)

# Establish database connection (or create database)
db <- dbConnect(SQLite(),"poobase.sqlite")


# Functions to interact with the databas
log_value <- function(event,intensity,db,table){
  current_table <- dbReadTable(db,table)
  cur_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if(nrow(current_table) == 0) cur_id = 1 else cur_id = max(current_table$id)+1
  next_values <- tibble(id = cur_id,time = cur_time,event = event,intensity = intensity)
  dbWriteTable(db,table,next_values,append = T)
}

create_table <- function(name,db){
  stopifnot(is.character(name))
  table_to_write <- tibble(id = integer(),time = character(),event = character(),intensity = integer())
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


#server functions

# Create modals which are similar
modal_factory <- function(b_name,min_name,max_name,cur_rad){
  function(){
    cur_rad(b_name)
    modalDialog(
      title = "How much?",
      radioButtons("rb","Select one:",
                   setNames(c(1:5),
                            c(min_name," "," "," ",max_name)),
                   selected = character(0)),
      easyClose = T,
      size = "s",
      footer = tagList(
        actionButton("submit","Submit")))
  }
}



# Create special feeding modal

feed_modal <- function(cur_rad){
  cur_rad("log_feed")
  modalDialog(
    title = "Did you feed with...",
    radioButtons("rb","Select one:",
                 setNames(c(1:3),
                          c("left","right","both")),
                          selected = character(0)),
    easyClose = T,
    size = "s",
    footer = tagList(
      actionButton("submit","Submit")
    )
  )
}

server <- function(input, output, session) {
  # Reactive for checking which modal is being used
  cur_rad <- reactiveVal()

  # Modals for similar logging events
  poo_modal <- modal_factory("log_poo","Very little","A lot",cur_rad)
  pee_modal <- modal_factory("log_pee","Very little","A lot",cur_rad)
  scream_modal <- modal_factory("log_scream","Very little","A lot",cur_rad)

  observeEvent(input$log_feed,
               showModal(feed_modal(cur_rad)))

  observeEvent(input$log_poo,
               {showModal(poo_modal())})

  observeEvent(input$log_pee,
               {showModal(pee_modal())})

  observeEvent(input$log_scream,
               {showModal(scream_modal())})

  observeEvent(input$delete_last,
               {dbExecute(conn = db,paste0("DELETE FROM pootable WHERE ID=",nrow(retrive_table("pootable",db))))})

  # Check submit button of the modal (popup window) and log correct value based on the name of the window
  observeEvent(input$submit,
               {which_modal <-case_when(
                 grepl("poo",cur_rad()) == T ~ "poo",
                 grepl("pee",cur_rad()) == T ~ "pee",
                 grepl("scream",cur_rad()) == T ~ "scream",
                 grepl("feed",cur_rad()) == T ~ "feed")
               log_value(event = which_modal,
                         intensity = input$rb,
                         db = db,
                         table = "pootable")
               removeModal()
               })

  # log of the last five entries into the db
  # refreshes if something is submitted or deleted from the db
  lpt<-reactive({
    cur_table<-retrive_table("pootable",db)
    nrow_cur_table <-nrow(cur_table)
    tail(cur_table,n=pmin(5,nrow_cur_table))[rev(1:pmin(5,nrow_cur_table)),]
  })|> bindEvent(input$submit,input$delete_last,ignoreNULL = T)


  output$last_poo <- renderTable(lpt())
}


