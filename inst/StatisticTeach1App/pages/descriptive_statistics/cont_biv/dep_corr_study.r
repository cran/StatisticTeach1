Dependency_and_Correlation_Study <- tabPanel(
  "Dependency and Correlation Study", value = "Dependency_and_Correlation_Study",
  div(
    style = "display: inline-block;vertical-align:top;width: 220px;",
    shinyWidgets::pickerInput(
      inputId = "correlation",
      label = "Correlation method:",
      choices = c("All", "Pearson", "Spearman", "Kendall"),
      selected = "All",
      options = list(style = "btn-primary")
    )
  ),
  br(),
  DT::DTOutput("bivar_metrics")
)
