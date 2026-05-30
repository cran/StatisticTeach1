# header.R

header <- dashboardHeader(
  title = "StatisticTeach1",
  titleWidth = 280,
  tags$li(
    class = "dropdown",
    actionLink(
      "stop_radiant",
      "Stop",
      icon = icon("power-off"),
      onclick = "setTimeout(function(){window.close();}, 100);")
  )
)

