library(shiny)
library(shinyMobile)
library(shinyWidgets)


ui <- f7Page(
  title = "Still tracker",options = list(theme = "md",dark = T,pullToRefresh = T),
  f7TabLayout(
    panels = tagList(
      f7Panel(title = "Anleitung",
              side = "left",
              theme = "dark",
              effect = "cover",
              "Im linken Tab kannst du Daten eintragen.\n Im rechten Tab kannst du dir Ergebnisse ansehen.",
              f7Button(inputId = "reset_table",label = "Daten zurücksetzen"))
    ),
    navbar = f7Navbar(
      title = "Still tracker",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = F
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        title = "Daten Eintragen",
        tabName = "dataentry",
        icon = f7Icon("pencile"),
        active = TRUE,
        f7BlockTitle(title = "Stillen"),
        f7Segment(
          shadow = TRUE,
          container = "segment",
          f7Button(inputId = "feed_1",label = "Links", outline = TRUE, fill = FALSE),
          f7Button(inputId = "feed_2",label = "Rechts", outline = TRUE, fill = FALSE),
          f7Button(inputId = "feed_3",label = "Links + Rechts", outline = TRUE, fill = FALSE)
        ),
        f7BlockTitle(title = "Stuhlgang"),
        f7Segment(
          shadow = TRUE,
          container = "segment",
          f7Button(inputId = "poo_1",label = "Pipi", outline = TRUE, fill = FALSE),
          f7Button(inputId = "poo_2",label = "Kacka", outline = TRUE, fill = FALSE),
          f7Button(inputId = "poo_3",label = "Pipi + Kacka", outline = TRUE, fill = FALSE)
        ),
        f7Card(
          title = "Letze Einträge"),
          uiOutput("last_poo"),
          f7Button(inputId = "openentr",label = "Daten Nachtragen"),
          f7Button(inputId = "delete_last",label = "Letzten Eintrag löschen")
        ),
      f7Tab(
        title = "Analyse",
        tabName = "analysis",
        icon = f7Icon("graph_square"),
        active = FALSE,
        f7Card(
            title = "Zeit seit dem letzten Stillen",
            h2(textOutput("timer_feed")),
            "(Stunden/Minuten)"
          ),
        f7Card(
          title = "Zeit seit dem letzten Stuhlgang",
          h2(textOutput("timer_poo")),
          "(Stunden/Minuten)"
          )
        )
      )
    )
  )


