library(shiny)
library(DBI)
library(shinythemes)

db <- DBI::dbConnect(RSQLite::SQLite(),"poobase.sqlite")

log_value <- function(event,intensity,db,table){
  current_table <- dbReadTable(db,table)
  cur_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if(nrow(current_table) == 0) cur_id = 1 else cur_id = max(current_table$id)+1
  next_values <- tibble::tibble(id = cur_id,time = cur_time,event = event,intensity = intensity)
  dbWriteTable(db,table,next_values,append = T)
}

create_table <- function(name,db){
  stopifnot(is.character(name))
  table_to_write <- tibble::tibble(id = integer(),time = character(),event = character(),intensity = integer())
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
  theme = shinytheme("united"),
  titlePanel("PooTracker"),
  tabsetPanel(
    tabPanel(
      "Data tracking",
      fluidRow(
        title = "Select ",
        actionButton("log_poo","poo"),
        actionButton("log_pee","pee"),
        actionButton("log_scream","scream"),
        actionButton("delete_last","delete last log")),
      fluidRow(
        tableOutput("last_poo"))
    ),
    tabPanel(
      "Data analysis"
    )
  ),

)

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

server <- function(input, output, session) {
  cur_rad <- reactiveVal()
  poo_modal <- modal_factory("log_poo","Very litte","A lot",cur_rad)
  pee_modal <- modal_factory("log_pee","Very litte","A lot",cur_rad)
  scream_modal <- modal_factory("log_scream","Very litte","A lot",cur_rad)

  observeEvent(input$log_poo,
               {showModal(poo_modal())})

  observeEvent(input$log_pee,
               {showModal(pee_modal())})

  observeEvent(input$log_scream,
               {showModal(scream_modal())})

  observeEvent(input$delete_last,
               {DBI::dbExecute(conn = db,paste0("DELETE FROM pootable WHERE ID=",nrow(retrive_table("pootable",db))))})

  observeEvent(input$submit,
               {which_modal <-dplyr::case_when(
                  grepl("poo",cur_rad()) == T ~ "poo",
                  grepl("pee",cur_rad()) == T ~ "pee",
                  grepl("scream",cur_rad()) == T ~ "scream")
               log_value(event = which_modal,
                         intensity = input$rb,
                         db = db,
                         table = "pootable")
                removeModal()
               })

  lpt<-reactive({
    cur_table<-retrive_table("pootable",db)
    nrow_cur_table <-nrow(cur_table)
    tail(cur_table,n=pmin(5,nrow_cur_table))[rev(1:pmin(5,nrow_cur_table)),]
    })|> bindEvent(input$submit,input$delete_last,ignoreNULL = T)
  output$last_poo <- renderTable(lpt())
}

shinyApp(ui, server)

