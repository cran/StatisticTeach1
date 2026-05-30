source("pages/bivariate_graphic_summary.R")
source("pages/dep_correlation_study.R")
continuous_variables_bivariate <- shinydashboard::tabItem(
  tabName = "Continuous_variables_bivariate",
  shinydashboard::box(
    height = "100%", width = 12, title = "Continuos variables",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    sidebarPanel(
      shinyWidgets::actionBttn(
        "helpbivariate",
        "Help",
        icon = icon("question-circle"),
        style = "stretch",
        block = FALSE,
        color = "primary"
      ),
      shinyBS::bsModal(
        id = "modalHelpBivariate",
        title = "Help: Continuous variables (bivariate)",
        trigger = "helpbivariate",
        size = "large",
        tags$iframe(src = "continuous_variables_bivariate.html", width = "100%", height = "600px", frameborder = 0)
      ),
      selectizeInput("selvar1",
        "Variable 1 (x)",
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput("selvar2",
        "Variable 2 (y)",
        choices = NULL,
        multiple = FALSE
      ),
    conditionalPanel(
  condition = "input.bivariateTabs != 'Dependency_and_Correlation_Study'",
  shinyWidgets::prettyCheckbox(
    inputId = "Gruposcatter",
    label = "Statistics by group?",
    value = FALSE,
    status = "success"
  )
),
      conditionalPanel(
        condition = "input.Gruposcatter == 1",
        selectizeInput("selgruposcatter",
          "group by",
          choices = NULL,
          multiple = FALSE
        )
      )
    ),
    mainPanel(
      tabsetPanel(id = "bivariateTabs",
        Dependency_and_Correlation_Study,
        Graphic_summary_ContinuousBivariate
      )
    )
  )
)
