continuous_probability_distribution <- shinydashboard::tabItem(
     tabName = "Continuous",
     shinydashboard::box(width = "100%",
                height = "100%",
                title = "Continuous probability distributions",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                sidebarPanel(helpText(h5("Continuous probability distributions")
                ),
                shinyWidgets::actionBttn("helpcontinuas",
                                         "",
                                         icon = icon("question-circle"),
                                         style = "stretch",
                                         block = FALSE,
                                         color = "primary"
                ),
                shinyBS::bsModal(id="dataset",
                                 title = "",
                                 trigger = "helpcontinuas",
                                 size="large",
                                 tags$iframe(
                                   src = "continuas.html",
                                   width = "100%",
                                  height = "100%",
                                   frameborder = 0,
                                   scrolling = "auto"
                                 ),

                                 # CSS para personalizar el tamaño del modal
                                 tags$style(HTML("
    #dataset .modal-dialog {
      width: 95%;  /* Adjusts the modal's width to 95% of the screen */
      height: 90%; /* Adjusts the modal's height to 90% of the screen */
      max-width: none; /* Removes the default max-width constraint */
    }
    #dataset .modal-content {
      height: 100%; /* Ensures that the content fills the modal */
      overflow: auto; /* Adds scrolling if the content exceeds the modal's height */
    }
    #dataset .modal-body {
      height: calc(100vh - 200px); /* Ensures the body takes full height with proper spacing */
      overflow-y: auto; /* Allows the body content to be scrollable */
    }
  "))
                ),
                shinyWidgets::pickerInput(
                  inputId = "continuas",
                  label = "Distribution",
                  choices = c("Select distribution",
                              "Normal",
                              "t-Student",
                              "Chi^2",
                              "F-Fisher",
                              "Exponential",
                              "Logistics",
                              "Log-Normal",
                              "Gamma",
                              "Weibull",
                              "Beta"),
                  options = list(
                    style = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.continuas != 'Select distribution'",
                  radioButtons(
                    inputId = "tipoC",
                    label = NULL,
                    inline = TRUE,
                    choices = c("Values", "Probabilities"))
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal'",
                  numericInput("mean_normal", "mean \\(\\mu\\):",
                               value = 0, step = 1
                  ),
                  radioButtons(
                    inputId = "variance_sd",
                    label = NULL,
                    choices = c(
                      "Variance \\(\\sigma^2\\)" = "variance_true",
                      "Standard deviation \\(\\sigma\\)" = "variance_false"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.variance_sd == 'variance_true'",
                  numericInput("variance_normal", "Variance \\(\\sigma^2\\):",
                               value = 1, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.variance_sd == 'variance_false'",
                  numericInput("sd_normal", "Standard deviation \\(\\sigma\\):",
                               value = 1, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_normal",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_normal", "x:",
                               value = 1, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_normal", "x:",
                               value = 1, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_normal", "a:",
                               value = -1, step = 1
                  ),
                  numericInput("b_normal", "b: \\( (a \\leq b) \\)",
                               value = 1, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_normal", "a:",
                               value = -1, step = 1
                  ),
                  numericInput("bt_normal", "b:",
                               value = 1, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student'",
                  numericInput("df_student", "Degrees of freedom \\(gl\\):",
                               value = 10, min = 1, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_student",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta'",
                  numericInput("alpha_beta", "Shape \\(\\alpha\\):",
                               value = 1,
                               min = 0,
                               step = 1
                  ),
                  numericInput("beta_beta", "Shape \\(\\beta\\):",
                               value = 3,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_beta",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Chi^2'",
                  numericInput("df_chisquare", "Degrees of freedom \\(gl\\):",
                               value = 6,
                               min = 1,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Chi^2'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_chisquare",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential'",
                  numericInput("rate_exponential", "Rate \\(\\lambda\\):",
                               value = 1,
                               min = 0,
                               step = 0.5
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_exponential",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher'",
                  numericInput("df1_fisher", "Degrees of freedom \\(gl_1\\):",
                               value = 10,
                               min = 1,
                               step = 1
                  ),
                  numericInput("df2_fisher", "Degrees of freedom \\(gl_2\\):",
                               value = 5,
                               min = 1,
                               step = 1
                  )
                ),

                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_fisher",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma'",
                  numericInput("alpha_gamma", "Shape \\(\\alpha\\):",
                               value = 3,
                               min = 0,
                               step = 1
                  ),
                  numericInput("beta_gamma", "Rate \\(\\beta\\):",
                               value = 2,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_gamma",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics'",
                  numericInput("location_logistic", "Location \\(\\mu\\):",
                               value = 0,
                               step = 1
                  ),
                  numericInput("scale_logistic", "Scale \\(s\\):",
                               value = 1,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_logistic",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal'",
                  numericInput("mean_lognormal",
                               "Mean \\(\\mu\\):",
                               value = 0,
                               step = 1
                  ),
                  radioButtons(
                    inputId = "variance_sd_lognormal",
                    label = NULL,
                    choices = c(
                      "Variance \\(\\sigma^2\\)" = "variance_true",
                      "Standard deviation \\(\\sigma\\)" = "variance_false"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.variance_sd_lognormal == 'variance_true'",
                  numericInput("variance_lognormal", "Variance \\(\\sigma^2\\):",
                               value = 1,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.variance_sd_lognormal == 'variance_false'",
                  numericInput("sd_lognormal", "Standard deviation \\(\\sigma\\):",
                               value = 1,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_lognormal",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull'",
                  numericInput("alpha_weibull", "Shape \\(\\alpha\\):",
                               value = 5,
                               min = 0,
                               step = 1
                  ),
                  numericInput("beta_weibull", "Scale \\(\\beta\\):",
                               value = 1,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull'&& input.tipoC== 'Values'",
                  radioButtons(
                    inputId = "lower_tail_weibull",
                    label = NULL,
                    choices = c(
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X > x)\\)" = "upper.tail",
                      "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval",
                      "Tails : \\(P(X \\leq a )+ P(X \\geq b)\\)" = "tails"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student' && input.lower_tail_student == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_student", "x:",
                               value = 1,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student' && input.lower_tail_student == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_student", "x:",
                               value = 1,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student' && input.lower_tail_student == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_student", "a:",
                               value = -1,
                               step = 1
                  ),
                  numericInput("b_student", "b: \\( (a \\leq b) \\)",
                               value = 1,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 't-Student' && input.lower_tail_student == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_student", "a:",
                               value = -1,
                               step = 1
                  ),
                  numericInput("bt_student", "b: \\( (a \\leq b) \\)",
                               value = 1,
                               step = 1
                  )
                ),

                conditionalPanel(
                  condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_chisquare", "x:",
                               value = 9.6,
                               min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_chisquare", "x:",
                               value = 9.6,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_chisquare", "a:",
                               value = 9.6,
                               min = 0,
                               step = 1
                  ),
                  numericInput("b_chisquare", "b: \\( (a \\leq b) \\)",
                               value = 14.4,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_chisquare", "a:",
                               value = 9.6,
                               min = 0,
                               step = 1
                  ),
                  numericInput("bt_chisquare", "b: \\( (a \\leq b) \\)",
                               value = 14.4,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_beta", "x:",
                               value = 0.45,
                               min = 0,
                               max = 1,
                               step = 0.01
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_beta", "x:",
                               value = 0.45,
                               min = 0,
                               max = 1,
                               step = 0.01
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_beta", "a:",
                               value = 0.25,
                               min = 0,
                               max = 1,
                               step = 0.01
                  ),
                  numericInput("b_beta", "b: \\( (a \\leq b) \\)",
                               value = 0.45,
                               min = 0,
                               max = 1,
                               step = 0.01
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_beta", "a:",
                               value = 0.25,
                               min = 0,
                               max = 1,
                               step = 0.01
                  ),
                  numericInput("bt_beta", "b: \\( (a \\leq b) \\)",
                               value = 0.45,
                               min = 0,
                               max = 1,
                               step = 0.01
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_exponential", "x:",
                               value = 2.24,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_exponential", "x:",
                               value = 2.24,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_exponential", "a:",
                               value = 2.24,
                               min = 0,
                               step = 1
                  ),
                  numericInput("b_exponential", "b: \\( (a \\leq b) \\)",
                               value = 3.36,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_exponential", "a:",
                               value = 2.24,
                               min = 0,
                               step = 1
                  ),
                  numericInput("bt_exponential", "b: \\( (a \\leq b) \\)",
                               value = 3.36,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_fisher", "x:",
                               value = 4.14,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_fisher", "x:",
                               value = 4.14,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_fisher", "a:",
                               value = 2.76,
                               min = 0,
                               step = 1
                  ),
                  numericInput("b_fisher", "b: \\( (a \\leq b) \\)",
                               value = 4.14,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_fisher", "a:",
                               value = 2.76,
                               min = 0,
                               step = 1
                  ),
                  numericInput("bt_fisher", "b: \\( (a \\leq b) \\)",
                               value = 4.14,
                               min = 0,
                               step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_gamma", "x:",
                               value = 2.4, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_gamma", "x:",
                               value = 2.4, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_gamma", "a:",
                               value = 0.8, min = 0, step = 1
                  ),
                  numericInput("b_gamma", "b: \\( (a \\leq b) \\)",
                               value = 2.4, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_gamma", "a:",
                               value = 0.8, min = 0, step = 1
                  ),
                  numericInput("bt_gamma", "b: \\( (a \\leq b) \\)",
                               value = 2.4, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_logistic", "x:",
                               value = 1.2, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_logistic", "x:",
                               value = 1.2, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_logistic", "a:",
                               value = -1.2, step = 1
                  ),
                  numericInput("b_logistic", "b: \\( (a \\leq b) \\)",
                               value = 1.2, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_logistic", "a:",
                               value = -1.2, step = 1
                  ),
                  numericInput("bt_logistic", "b: \\( (a \\leq b) \\)",
                               value = 1.2, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_lognormal", "x:",
                               value = 1, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_lognormal", "x:",
                               value = 1, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_lognormal", "a:",
                               value = 1, min = 0, step = 1
                  ),
                  numericInput("b_lognormal", "b: \\( (a \\leq b) \\)",
                               value = 2, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_lognormal", "a:",
                               value = 1, min = 0, step = 1
                  ),
                  numericInput("bt_lognormal", "b: \\( (a \\leq b) \\)",
                               value = 2, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'lower.tail'&& input.tipoC == 'Values'",
                  numericInput("x1_weibull", "x:",
                               value = 0.8, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'upper.tail'&& input.tipoC == 'Values'",
                  numericInput("x2_weibull", "x:",
                               value = 0.8, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'interval'&& input.tipoC == 'Values'",
                  numericInput("a_weibull", "a:",
                               value = 0.8, min = 0, step = 1
                  ),
                  numericInput("b_weibull", "b: \\( (a \\leq b) \\)",
                               value = 1.2, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'tails'&& input.tipoC == 'Values'",
                  numericInput("at_weibull", "a:",
                               value = 0.8, min = 0, step = 1
                  ),
                  numericInput("bt_weibull", "b: \\( (a \\leq b) \\)",
                               value = 1.2, min = 0, step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.continuas != 'Select distribution'&& input.tipoC == 'Probabilities'",
                  numericInput(inputId = "probC", label = "Probability of interest:",
                               value = 0.05, step = 0.001),
                  radioButtons(
                    inputId = "Continuasx",
                    label = "Type of desired outcome",
                    choices = c(
                      #"Exact value: \\(P(X = x)\\)" = "exact",
                      "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
                      "Upper tail : \\(P(X \\geq x)\\)" = "upper.tail"
                    )
                  )
                ), br(),
                actionButton("refresh2" ,"Refresh")

                ),
                mainPanel(
                  tags$b("Distribution Details:"),
                  br(),
                  uiOutput("parameters_distribution"),
                  tags$b("Solution:"),
                  uiOutput("results_distribution"),
                  br(),

                  conditionalPanel(

                    condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("normalPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("normalPlot_upper")),
                  conditionalPanel(
                    condition = "input.continuas == 'Normal' && input.Continuasx  == 'lower.tail' && input.tipoC == 'Probabilities'",
                    plotOutput("normalPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Normal' && input.Continuasx == 'upper.tail' && input.tipoC == 'Probabilities'",
                    plotOutput("normalPlot_upper_Probabilities")),

                  conditionalPanel(
                    condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'interval' && input.tipoC== 'Values'",
                    plotOutput("normalPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Normal' && input.lower_tail_normal == 'tails' && input.tipoC== 'Values'",
                    plotOutput("normalPlot_tails")
                  ),


                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.lower_tail_student == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("studentPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.lower_tail_student == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("studentPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("studentPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("studentPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.lower_tail_student == 'interval' && input.tipoC== 'Values'",
                    plotOutput("studentPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 't-Student' && input.lower_tail_student == 'tails' && input.tipoC== 'Values'",
                    plotOutput("studentPlot_tails")
                  ),






                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("betaPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("betaPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("betaPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("betaPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'interval' && input.tipoC== 'Values'",
                    plotOutput("betaPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Beta' && input.lower_tail_beta == 'tails'&& input.tipoC== 'Values'",
                    plotOutput("betaPlot_tails")
                  ),



                  #### chi cuadrada continua

                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("chisquarePlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("chisquarePlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("chisquarePlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("chisquarePlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'interval' && input.tipoC== 'Values'",
                    plotOutput("chisquarePlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Chi^2' && input.lower_tail_chisquare == 'tails' && input.tipoC== 'Values'",
                    plotOutput("chisquarePlot_tails")
                  ),



                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("exponentialPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("exponentialPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("exponentialPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("exponentialPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'interval' && input.tipoC== 'Values'",
                    plotOutput("exponentialPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Exponential' && input.lower_tail_exponential == 'tails' && input.tipoC== 'Values'",
                    plotOutput("exponentialPlot_tails")
                  ),





                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("fisherPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("fisherPlot_upper")
                  ),

                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("fisherPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("fisherPlot_upper_Probabilities")
                  ),


                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'interval' && input.tipoC== 'Values'",
                    plotOutput("fisherPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'F-Fisher' && input.lower_tail_fisher == 'tails' && input.tipoC== 'Values'",
                    plotOutput("fisherPlot_tails")
                  ),





                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("gammaPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("gammaPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("gammaPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("gammaPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'interval' && input.tipoC== 'Values'",
                    plotOutput("gammaPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Gamma' && input.lower_tail_gamma == 'tails' && input.tipoC== 'Values'",
                    plotOutput("gammaPlot_tails")
                  ),




                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("logisticPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("logisticPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("logisticPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("logisticPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'interval' && input.tipoC== 'Values'",
                    plotOutput("logisticPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Logistics' && input.lower_tail_logistic == 'tails' && input.tipoC== 'Values'",
                    plotOutput("logisticPlot_tails")
                  ),





                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("lognormalPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("lognormalPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("lognormalPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("lognormalPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'interval' && input.tipoC== 'Values'",
                    plotOutput("lognormalPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Log-Normal' && input.lower_tail_lognormal == 'tails' && input.tipoC== 'Values'",
                    plotOutput("lognormalPlot_tails")
                  ),




                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'lower.tail' && input.tipoC== 'Values'",
                    plotOutput("weibullPlot_lower")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'upper.tail' && input.tipoC== 'Values'",
                    plotOutput("weibullPlot_upper")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.Continuasx == 'lower.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("weibullPlot_lower_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.Continuasx == 'upper.tail' && input.tipoC== 'Probabilities'",
                    plotOutput("weibullPlot_upper_Probabilities")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'interval' && input.tipoC== 'Values'",
                    plotOutput("weibullPlot_interval")
                  ),
                  conditionalPanel(
                    condition = "input.continuas == 'Weibull' && input.lower_tail_weibull == 'tails' && input.tipoC== 'Values'",
                    plotOutput("weibullPlot_tails")
                  ),
                  br(),
                  shinyjs::useShinyjs(),
                  downloadButton('downloadPlot2', 'Download Plot'),
                  br()
                )# fin main panel
            )
    )
