# sidebar.R

sidebar <- dashboardSidebar(
  width = 280, 
  sidebarMenu(
    menuItem("About StatisticTeach1", tabName = "tab0"),
    menuItem("Descriptive statistics", tabName = "tab3", icon = icon("chart-bar"),
             menuItem("Load data", tabName = "Load_data", icon = icon("file-import")),
             menuSubItem("Continuous variables (univariate)", tabName = "Continuous_variables_univariate"),
             menuSubItem("Continuous variables (bivariate)", tabName = "Continuous_variables_bivariate"),
             menuSubItem("Qualitative variables", tabName = "Qualitative_variables"),
             menuSubItem("Multivariate", tabName = "Multivariate")
    ),
    menuItem("Probability distributions", tabName = "tab1", icon = icon("dice"),
             menuSubItem("Discrete", tabName = "Discrete", icon = icon("chart-bar")),
             menuSubItem("Continuous", tabName = "Continuous", icon = tags$img(src = "normal1.png", width = "35px"))
    )
  )
)
