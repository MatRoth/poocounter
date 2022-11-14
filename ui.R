library(shiny)
library(shinyMobile)
library(apexcharter)
library(shinyWidgets)

ui <- f7Page(
  title = "Still tracker",
  f7TabLayout(
    panels = tagList(
      f7Panel(title = "Anleitung", side = "left", theme = "light", "Im linken Tab kannst du Daten eintragen.\n Im rechten Tab kannst du dir Ergebnisse ansehen.", effect = "cover"),
      f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
    ),
    options = list(theme = "md"),
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
        tabName = "Daten eintragen",
        icon = f7Icon("pencile"),
        active = TRUE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Was willst du eintragen?",
            f7List(
              f7ListItem("Stillen",f7Button(inputId = "log_feed",label="Eintragen",size = "small")),
              f7ListItem("Stuhlgang",f7Button(inputId = "log_food",label="Eintragen",size = "small")),
              f7ListItem("Letzen Eintrag löschen",f7Button(inputId = "delete_last",label = "Löschen",size = "small"))
            )
          )
        )
      ),
      f7Tab(
        tabName = "Analyse",
        icon = f7Icon("graph_square"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Letztes Stillen",
          )
        )
      )
    )
  )
)
