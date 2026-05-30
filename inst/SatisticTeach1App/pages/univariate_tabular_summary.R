tabular_summary <- tabPanel("Tabular summary",
                     div(style="display: inline-block;vertical-align:top;
                        width: 150px;",shinyWidgets::pickerInput(
                          inputId = "Selectrule",
                          label = "Select rule :",
                          choices = c("",
                                      "Sturges",
                                      "Sqrt",
                                      "Scott",
                                      "Freedman–Diaconis",
                                      "Number of bin"),
                          options = list(
                            style = "btn-primary")),

                     ),
                     conditionalPanel(
                       condition = "input.Selectrule == 'Number of bin'",
                       sliderInput("intervalnum",
                                   "",
                                   min = 4,
                                   max = 50,
                                   value = 5,
                                   step = 1)
                     ),
                     br(),
                     actionButton(
                       inputId = "getTable",
                       label="Get table"), br(),br(),

                     DT::DTOutput("tablefreq")
                     )