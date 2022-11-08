library(shiny)
library(DBI)

db <- DBI::dbConnect(RSQLite::SQLite(),"poobase.sqlite")

log_value <- function(event,db,table){
  current_table <- dbReadTable(db,table)
  cur_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if(nrow(current_table) == 0) cur_id = 1 else cur_id = max(current_table$id)+1
  next_values <- tibble::tibble(id = cur_id,time = cur_time,event = event)
  dbWriteTable(db,table,next_values,append = T)
}

create_table <- function(name,db){
  stopifnot(is.character(name))
  table_to_write <- tibble::tibble(id = integer(),time = character(),event = character())
  dbWriteTable(conn = db,name = name, table_to_write)
}

retrive_table <- function(name,db){
  cur_table <- dbReadTable(db,name) |> tibble::as_tibble()
  cur_table
}

reset_table <- function(name,db){
  command <- DBI::dbExecute(conn = db,paste("DROP TABLE",name))
  create_table(name,db)
}


if(!("pootable" %in% dbListTables(db))) create_table("pootable",db)

ui <- fluidPage(
  actionButton("log_poo","poo"),
  actionButton("log_pee","pee"),
  actionButton("log_scream","scream"),
  actionButton("delete_last","delete last log"),
  tableOutput("last_poo")
)

server <- function(input, output, session) {

  observeEvent(input$log_poo,
               {log_value(event = "poo",db = db,table = "pootable")})

  observeEvent(input$log_pee,
               {log_value(event = "pee",db = db,table = "pootable")})

  observeEvent(input$log_scream,
               {log_value(event = "scream",db = db,table = "pootable")})

  observeEvent(input$delete_last,
               {DBI::dbExecute(conn = db,paste0("DELETE FROM pootable WHERE ID=",nrow(retrive_table("pootable",db))))})

  lpt<-reactive({
    cur_table<-retrive_table("pootable",db)
    nrow_cur_table <-nrow(cur_table)
    tail(cur_table,n=pmin(5,nrow_cur_table))[rev(1:pmin(5,nrow_cur_table)),]
    })|> bindEvent(input$log_poo,input$log_pee,input$log_scream,input$delete_last)
  output$last_poo <- renderTable(lpt())
}

shinyApp(ui, server)

