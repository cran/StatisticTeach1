numerical_summary <- tabPanel("Numerical summary",
              div(style="display: inline-block;vertical-align:top;
                        width: 180px;",
                  shinyWidgets::pickerInput(
                    inputId = "NumSu",
                    label = "Select measures of:",
                    choices = c("",
                                "Central tendency",
                                "Dispersion or variability",
                                "Relative position",
                                "Shape"),
                    options = list(
                      style = "btn-primary"))),
              conditionalPanel(
                condition = "input.NumSu == 'Central tendency'",
                DT::DTOutput("central"),
              ),
              conditionalPanel(
                condition = "input.NumSu == 'Dispersion or variability'",
                DT::DTOutput("variability"),
              ),
              conditionalPanel(
                condition = "input.NumSu == 'Relative position'",
                div(style="display: inline-block;vertical-align:top;
                        width: 180px;",
                    shinyWidgets::pickerInput(
                      inputId = "relpos",
                      label = "Select measures of:",
                      choices = c("Quartiles","Percentiles"),
                      options = list(
                        style = "btn-default")))
              ),
              conditionalPanel( condition = "input.relpos == 'Quartiles' && input.NumSu == 'Relative position'",
                div(style="display: inline-block;vertical-align:top;
                        width: 180px;",
                    numericInput(inputId= "nquartiles", "Number of quartiles to calculate:",
                                 value = 3, min = 2, max = 10, step = 1, width = "90%")),
                DT::DTOutput("quart")
              ),
              conditionalPanel(
                condition = "input.relpos == 'Percentiles'",
                div(style="display: inline-block;vertical-align:top;
                        width: 180px;",
                numericInput(inputId= "npercent", "Enter the desired percentile (between 1 and 99):",
                             value = 90, min = 1, max = 99, step = 1, width = "90%")),
                   verbatimTextOutput("perc"),
                DT::DTOutput("perct")
              ),
              conditionalPanel( condition = "input.NumSu == 'Shape'",
                DT::DTOutput("shape")
              )
              )