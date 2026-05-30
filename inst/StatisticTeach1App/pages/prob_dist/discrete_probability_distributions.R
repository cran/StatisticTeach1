discrete_probability_distributions  <-   shinydashboard::tabItem(
      tabName = "Discrete",
      shinydashboard::box(width = "100%",
          height = "100%",
          title = "Discrete Probability Distributions",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
         sidebarPanel(width = 3, helpText(h5("Discrete Probability Distributions")),
                      shinyWidgets::actionBttn("helploadD",
                                               "",
                                               icon = icon("question-circle"),
                                               style = "stretch",
                                               block = FALSE,
                                               color = "primary"
                      ),
                      shinyBS::bsModal(id="helpD",
                                       title = "",
                                       trigger = "helploadD",
                                       size="large",
                                       tags$iframe(
                                         src = "discretas.html",
                                         width = "100%",
                                         height = "1000px",
                                         frameborder = 0,
                                         scrolling = "auto"
                                       )
                      ),
                      shinyWidgets::pickerInput(
                         inputId = "discretas",
                         label = "Distribution",
                         choices = c("Select distribution",
                                     "Binomial",
                                     "Geometric",
                                     #"Geometric (II)",
                                     "Hypergeometric",
                                     "Negative Binomial",
                                     #"Negative Binomial (II)",
                                     "Poisson"),
                         options = list(style = "btn-primary")),
                      conditionalPanel(
                        condition = "input.discretas != 'Select distribution'",
                        radioButtons(
                          inputId = "tipo",
                          label = NULL,
                          inline = TRUE,
                          choices = c("Values", "Probabilities"))
                      ),

                      conditionalPanel(
                        condition = "input.discretas == 'Binomial'",
                        numericInput("n_binomial",
                                     "Trial \\(n\\):",
                                      value = 20, min = 0, step = 1),
                         numericInput("p_binomial",
                                      "Probability of success \\(p\\):",
                                      value = 0.5, min = 0, max = 1,
                                      step = 0.01)
                        ),
                       conditionalPanel(
                         condition = "input.discretas == 'Geometric'",
                         numericInput("p_geometric",
                                      "Probability of success \\(p\\):",
                                      value = 0.5, min = 0,
                                      max = 1, step = 0.01)
                       ),
                      conditionalPanel(
                         condition = "input.discretas == 'Hypergeometric'",
                         numericInput("n_hypergeometric",
                                      "Sample size \\(n\\):",
                                      value = 20, min = 0, step = 1),
                         numericInput("N_hypergeometric",
                                      "Population size  \\(N\\):",
                                      value = 50, min = 0, step = 1),
                         numericInput("M_hypergeometric", "
                                     Number of successes \\(M\\):",
                                      value = 15, min = 0, step = 1)
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Negative Binomial'",
                         numericInput("r_negativebinomial",
                                      "Number of successes \\(r\\):",
                                      value = 5, min = 1, step = 1 ),
                         numericInput("p_negativebinomial",
                                      "Probability of success \\(p\\):",
                                      value = 0.5, min = 0,
                                      max = 1, step = 0.01 )
                       ),
                    conditionalPanel(
                         condition = "input.discretas == 'Poisson'",
                         numericInput("lambda_poisson",
                                      "Rate \\(\\lambda\\):",
                                      value = 4, min = 1, step = 1)
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Binomial'&& input.tipo== 'Values'",
                         radioButtons(
                           inputId = "lower_tail_binomial",
                           label = NULL,
                           choices = c(
                             "Exact value: \\(P(X = x)\\)" = "exact",
                             "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                             "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail",
                             "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                             "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"

                           )
                         )
                       ),
                    conditionalPanel(
                      condition = "input.discretas == 'Binomial'&& input.tipo == 'Probabilities'",
                      numericInput(inputId = "probB", label = "Probability of interest:",
                                   value = 0.5, step = 0.01),
                      radioButtons(
                        inputId = "lower_tail_binomialP",
                        label = "Type of desired outcome",
                        choices = c(
                          #"Exact value: \\(P(X = x)\\)" = "exact",
                          "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                          "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                        )
                      )
                    ),
                       conditionalPanel(
                         condition = "input.discretas == 'Geometric'&& input.tipo== 'Values'",

                         radioButtons(
                           inputId = "lower_tail_geometric",
                           label = NULL,
                           choices = c(
                             "Exact value: \\(P(X = x)\\)" = "exact",
                             "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                             "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail",
                             "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                             "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                           )
                         )
                       ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric'&& input.tipo== 'Probabilities'",
                      numericInput(inputId = "probG", label = "Probability of interest:",
                                   value = 0.25, step = 0.01),
                      radioButtons(
                        inputId = "lower_tail_geometricP",
                        label = "Type of desired outcome",
                        choices = c(
                         # "Exact value: \\(P(X = x)\\)" = "exact",
                          "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                          "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                        )
                      )
                    ),







                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric'&& input.tipo== 'Probabilities'",
                      numericInput(inputId = "probH", label = "Probability of interest:",
                                   value = 0.15, step = 0.01),
                      radioButtons(
                        inputId = "lower_tail_hyperP",
                        label = "Type of desired outcome",
                        choices = c(
                         # "Exact value: \\(P(X = x)\\)" = "exact",
                          "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                          "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial'&& input.tipo == 'Probabilities'",
                      numericInput(inputId = "probN", label = "Probability of interest:",
                                   value = 0.05, step = 0.01),
                      radioButtons(
                        inputId = "lower_tail_negativeP",
                        label = "Type of desired outcome",
                        choices = c(
                          #"Exact value: \\(P(X = x)\\)" = "exact",
                          "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                          "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                        )
                      )),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson'&& input.tipo == 'Probabilities'",
                      numericInput(inputId = "probP", label = "Probability of interest:",
                                   value = 0.05, step = 0.01),
                      radioButtons(
                        inputId = "lower_tail_poissonP",
                        label = "Type of desired outcome",
                        choices = c(
                          #"Exact value: \\(P(X = x)\\)" = "exact",
                          "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                          "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                        )
                      )),

                    conditionalPanel(
                         condition = "input.discretas == 'Hypergeometric'&& input.tipo== 'Values'",
                         radioButtons(
                           inputId = "lower_tail_hypergeometric",
                           label = NULL,
                           choices = c(
                             "Exact value: \\(P(X = x)\\)" = "exact",
                             "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                             "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail",
                             "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                             "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                           )
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Negative Binomial'&& input.tipo== 'Values'",
                         radioButtons(
                           inputId = "lower_tail_negativebinomial",
                           label = NULL,
                           choices = c(
                             "Exact value: \\(P(X = x)\\)" = "exact",
                             "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                             "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail",
                             "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                             "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                           )
                         )
                       ),
                    conditionalPanel(
                         condition = "input.discretas == 'Poisson'&& input.tipo== 'Values'",
                         radioButtons(
                           inputId = "lower_tail_poisson",
                           label = NULL,
                           choices = c(
                             "Exact value: \\(P(X = x)\\)" = "exact",
                             "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                             "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail",
                             "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                             "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                           )
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'exact'&& input.tipo== 'Values'",
                        numericInput("x_binomial", "x:",
                                     value = 9, min = 1, step = 1
                        )
                      ),
                       conditionalPanel(
                         condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'lower.tail'&& input.tipo== 'Values'",
                         numericInput("x1_binomial", "x:",
                                      value = 8, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'upper.tail'&& input.tipo== 'Values'",
                         numericInput("x2_binomial", "x:",
                                      value = 8, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'interval'&& input.tipo== 'Values'",
                         numericInput("a_binomial", "a:",
                                      value = 8, min = 0, step = 1
                         ),
                         numericInput("b_binomial", "b: \\( (a \\leq b) \\)",
                                      value = 12, min = 0, step = 1
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'tails'&& input.tipo== 'Values'",
                        numericInput("at_binomial", "a:",
                                     value = 5, min = 0, step = 1
                        ),
                        numericInput("bt_binomial", "b: ",
                                     value = 15, min = 0, step = 1
                        )
                      ),
                      conditionalPanel(
                        condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'exact'&& input.tipo== 'Values'",
                        helpText("Number of failures before  \\(1^{st}\\) success"),
                        numericInput("x_geometric", "x:",
                                     value = 1, min = 0, step = 1
                        )
                      ),
                       conditionalPanel(
                         condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'lower.tail'&& input.tipo== 'Values'",

                         numericInput("x1_geometric", "x:",
                                      value = 5, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'upper.tail'&& input.tipo== 'Values'",

                         numericInput("x2_geometric", "x:",
                                      value = 1, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'interval'&& input.tipo== 'Values'",

                         numericInput("a_geometric", "a:",
                                      value = 1, min = 0, step = 1
                         ),
                         numericInput("b_geometric", "b: ",
                                      value = 3, min = 0, step = 1
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'tails'&& input.tipo== 'Values'",

                        numericInput("at_geometric", "a:",
                                     value = 1, min = 0, step = 1
                        ),
                        numericInput("bt_geometric", "b: ",
                                     value = 3, min = 0, step = 1
                        )
                      ),
                      conditionalPanel(
                        condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'exact'&& input.tipo== 'Values'",
                        numericInput("x_hypergeometric", "x:",
                                     value = 8, min = 0, step = 1
                        )
                      ),
                       conditionalPanel(
                         condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'lower.tail'&& input.tipo== 'Values'",
                         numericInput("x1_hypergeometric", "x:",
                                      value = 8, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'upper.tail'&& input.tipo== 'Values'",
                         numericInput("x2_hypergeometric", "x:",
                                      value = 8, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'interval'&& input.tipo== 'Values'",
                         numericInput("a_hypergeometric", "a:",
                                      value = 8, min = 0, step = 1
                         ),
                         numericInput("b_hypergeometric", "b: \\( (a \\leq b) \\)",
                                      value = 12, min = 0, step = 1
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'tails'&& input.tipo== 'Values'",
                        numericInput("at_hypergeometric", "a:",
                                     value = 8, min = 0, step = 1
                        ),
                        numericInput("bt_hypergeometric", "b:",
                                     value = 12, min = 0, step = 1
                        )
                      ),
                      conditionalPanel(
                        condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'exact'&& input.tipo== 'Values'",
                        helpText("Número de fallas antes del  \\(r^{th}\\) success"),
                        numericInput("x_negativebinomial", "x:",
                                     value = 2, min = 0, step = 1
                        )
                      ),
                       conditionalPanel(
                         condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'lower.tail'&& input.tipo== 'Values'",
                         helpText("Número de fallas antes del  \\(r^{th}\\) success"),
                         numericInput("x1_negativebinomial", "x:",
                                      value = 2, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'upper.tail'&& input.tipo== 'Values'",
                         helpText("Number of failures before  \\(r^{th}\\) success"),
                         numericInput("x2_negativebinomial", "x:",
                                      value = 2, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'interval'&& input.tipo== 'Values'",
                         helpText("Number of failures before  \\(r^{th}\\) success"),
                         numericInput("a_negativebinomial", "a:",
                                      value = 2, min = 0, step = 1
                         ),
                         numericInput("b_negativebinomial", "b: \\( (a \\leq b) \\)",
                                      value = 4, min = 0, step = 1
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'tails'&& input.tipo== 'Values'",
                        helpText("Number of failures before  \\(r^{th}\\) success"),
                        numericInput("at_negativebinomial", "a:",
                                     value = 2, min = 0, step = 1
                        ),
                        numericInput("bt_negativebinomial", "b:",
                                     value = 4, min = 0, step = 1
                        )
                      ),
                    conditionalPanel(
                        condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'exact'&& input.tipo== 'Values'",
                        numericInput("x_poisson", "x:",
                                     value = 6, min = 0, step = 1
                        )
                      ),
                       conditionalPanel(
                         condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'lower.tail'&& input.tipo== 'Values'",
                         numericInput("x1_poisson", "x:",
                                      value = 6, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'upper.tail'&& input.tipo== 'Values'",
                         numericInput("x2_poisson", "x:",
                                      value = 6, min = 0, step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'interval'&& input.tipo== 'Values'",
                         numericInput("a_poisson", "a:",
                                      value = 6, min = 0, step = 1
                         ),
                         numericInput("b_poisson", "b: \\( (a \\leq b) \\)",
                                      value = 10, min = 0, step = 1
                         )
                       ),
                      conditionalPanel(
                        condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'tails'&& input.tipo== 'Values'",
                        numericInput("at_poisson", "a:",
                                     value = 6, min = 0, step = 1
                        ),
                        numericInput("bt_poisson", "b:",
                                     value = 10, min = 0, step = 1
                        )
                      ),
                    actionButton("refresh1" ,"Refresh")
          ),
          mainPanel(height = "100%",
                    tags$b("Distribution details:"),
                    uiOutput("parameters_distribution1"),
                    br(),
                    tags$b("Solution:"),
                    uiOutput("results_distribution2"),
                    fluidRow(
                      column(width = 9,

                    conditionalPanel(
                     condition = "input.discretas == 'Binomial' && input.tipo == 'Probabilities' && input.lower_tail_binomialP == 'upper.tail'",
                     plotOutput("binomialPlot_Probabilities_upper")
                             ),
                    conditionalPanel(
                             condition = "input.discretas == 'Binomial' && input.tipo == 'Probabilities' && input.lower_tail_binomialP == 'lower.tail'",
                             plotOutput("binomialPlot_Probabilities_lower")
                             ),

                    conditionalPanel(
                      condition = "input.discretas == 'Binomial' && input.tipo == 'Values' && input.lower_tail_binomial == 'exact'",
                      plotOutput("binomialPlot_exact")

                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'lower.tail'",
                      plotOutput("binomialPlot_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'upper.tail'",
                      plotOutput("binomialPlot_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'interval'",
                      plotOutput("binomialPlot_interval")
                    ),

                    conditionalPanel(
                      condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'tails'",
                      plotOutput("binomialPlot_tails")
                    ),






                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.tipo == 'Probabilities' && input.lower_tail_geometricP=='lower.tail'",
                      plotOutput("geometricPlot_probabilities_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.tipo == 'Probabilities' && input.lower_tail_geometricP=='upper.tail'",
                      plotOutput("geometricPlot_probabilities_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'exact'",
                      plotOutput("geometricPlot_exact")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'lower.tail'",
                      plotOutput("geometricPlot_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'upper.tail'",
                      plotOutput("geometricPlot_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'interval'",
                      plotOutput("geometricPlot_interval")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'tails'",
                      plotOutput("geometricPlot_tails")
                    ),



                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.tipo == 'Probabilities' && input.lower_tail_hyperP=='lower.tail'",
                      plotOutput("hypergeometricPlot_probabilities_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.tipo == 'Probabilities' && input.lower_tail_hyperP=='upper.tail'",
                      plotOutput("hypergeometricPlot_probabilities_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'exact'",
                      plotOutput("hypergeometricPlot_exact")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'lower.tail'",
                      plotOutput("hypergeometricPlot_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'upper.tail'",
                      plotOutput("hypergeometricPlot_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'interval'",
                      plotOutput("hypergeometricPlot_interval")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Hypergeometric' && input.lower_tail_hypergeometric == 'tails'",
                      plotOutput("hypergeometricPlot_tails")
                    ),







                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.tipo == 'Probabilities' && input.lower_tail_negativeP == 'lower.tail'",
                      plotOutput("negativebinomialPlot_Probalities_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.tipo == 'Probabilities' && input.lower_tail_negativeP == 'upper.tail'",
                      plotOutput("negativebinomialPlot_Probabilities_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'exact'",
                      plotOutput("negativebinomialPlot_exact")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'lower.tail'",
                      plotOutput("negativebinomialPlot_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'upper.tail'",
                      plotOutput("negativebinomialPlot_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'interval'",
                      plotOutput("negativebinomialPlot_interval")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Negative Binomial' && input.lower_tail_negativebinomial == 'tails'",
                      plotOutput("negativebinomialPlot_tails")
                    ),


                    conditionalPanel(
                      condition = "input.discretas == 'Poisson'  && input.tipo == 'Probabilities' && input.lower_tail_poissonP == 'lower.tail'",
                      plotOutput("poissonPlot_Probabilities_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson'   && input.tipo == 'Probabilities' && input.lower_tail_poissonP == 'upper.tail'",
                      plotOutput("poissonPlot_Probabilities_upper")
                    ),

                    conditionalPanel(
                      condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'exact'",
                      plotOutput("poissonPlot_exact")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'lower.tail'",
                      plotOutput("poissonPlot_lower")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'upper.tail'",
                      plotOutput("poissonPlot_upper")
                    ),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'interval'",
                      plotOutput("poissonPlot_interval"),
                      ),
                    conditionalPanel(
                      condition = "input.discretas == 'Poisson' && input.lower_tail_poisson == 'tails'",
                      plotOutput("poissonPlot_tails"),
                    )
                    ), column(width = 3, DT::DTOutput("table_discrete")),
                    ),
                    shinyjs::useShinyjs(),
                    downloadButton('downloadPlot', 'Download Plot'),
                    br()
                    #fluidRow(
                     # column(width = 9,
                                   # conditionalPanel(
                                     # condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'lower.tail'",
                                     # plotOutput("binomialPlot_loweracum")
                                    # ),
                             #conditionalPanel(
                              # condition = "input.discretas == 'Binomial' && input.lower_tail_binomial == 'upper.tail'"#,
                               # plotOutput("binomialPlot_upperacum")
                            # ),
                             #conditionalPanel(
                              # condition = "input.discretas == 'Geometric' && input.lower_tail_geometric == 'lower.tail'",
                              # plotOutput("geometricPlot_loweracum"))
                          #  )#,
                      #column(width = 3,DT::DTOutput("table_discrete_acum"))

                   # )



          ) #finmainpanel
      )

    )
