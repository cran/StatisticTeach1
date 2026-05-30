tab0_about <- shinydashboard::tabItem(
  tabName = "tab0",
  shiny::fluidPage(
    tags$iframe(
      src = "about.html",
      width = "80%",
      height = "800px",
      frameborder = 0,
      scrolling = "auto"
    )
  )
)
