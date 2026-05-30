
shiny::shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 20000 * 1024^2)
  values <- reactiveValues()
  ############ modulo distribuciones de probabilidad #############################
  #-----------Discretas
  #----------------------------
  output$results_distribution2 <- renderUI({
    if (input$discretas == "Binomial" && input$tipo == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_binomial == "exact" ~ paste0("\\(P(X = \\)", " ", input$x_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$x_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
            input$lower_tail_binomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
            input$lower_tail_binomial == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x2_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE), 4)),
            input$lower_tail_binomial == "interval" ~ paste0("\\(P(\\)", input$a_binomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_binomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_binomial > input$b_binomial, "a must be less than or equal to b", round(pbinom(input$b_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$a_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4))),
            input$lower_tail_binomial == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_binomial, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$bt_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE) + pbinom(input$at_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4))
          )
        )
      )
    } else if (input$discretas == "Binomial" && input$tipo == "Probabilities") {
      if (input$lower_tail_binomialP == "lower.tail") {
        x <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = TRUE)
        return(paste("The value of x is:", x))
      } else {
        x <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = FALSE)

        return(paste("The value of x is:", x))
      }
    } else if (input$discretas == "Geometric" && input$tipo == "Values") {
      withMathJax(
        paste0(case_when(
          input$lower_tail_geometric == "exact" ~ paste0("\\(P(X = \\)", " ", input$x_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x_geometric, prob = input$p_geometric, lower.tail = TRUE) - pgeom(input$x_geometric - 1, prob = input$p_geometric, lower.tail = TRUE), 4)),
          input$lower_tail_geometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geometric, prob = input$p_geometric, lower.tail = TRUE), 4)),
          input$lower_tail_geometric == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geometric - 1, prob = input$p_geometric, lower.tail = FALSE), 4)),
          input$lower_tail_geometric == "interval" ~ paste0("\\(P(\\)", input$a_geometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_geometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geometric > input$b_geometric, "a must be less than or equal to b", round(pgeom(input$b_geometric, prob = input$p_geometric, lower.tail = TRUE) - pgeom(input$a_geometric - 1, prob = input$p_geometric, lower.tail = TRUE), 4))),
          input$lower_tail_geometric == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_geometric, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_geometric, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$b_geometric - 1, prob = input$p_geometric, lower.tail = FALSE) + pgeom(input$a_geometric, prob = input$p_geometric, lower.tail = TRUE), 4))
        ))
      )
    } else if (input$discretas == "Geometric" && input$tipo == "Probabilities") {
      if (input$lower_tail_geometricP == "lower.tail") {
        x <- qgeom(input$probG, input$p_geometric, lower.tail = TRUE)
        return(paste("The value of x is:", x))
      } else {
        x <- qgeom(input$probG, input$p_geometric, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    } else if (input$discretas == "Hypergeometric" && input$tipo == "Values") {
      withMathJax(
        paste0(case_when(
          input$lower_tail_hypergeometric == "exact" ~ paste0("\\(P(X = \\)", " ", input$x_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$x_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
          input$lower_tail_hypergeometric == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
          input$lower_tail_hypergeometric == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x2_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE), 4)),
          input$lower_tail_hypergeometric == "interval" ~ paste0("\\(P(\\)", input$a_hypergeometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_hypergeometric > input$b_hypergeometric, "a must be less than or equal to b", round(phyper(input$b_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$a_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4))),
          input$lower_tail_hypergeometric == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_hypergeometric, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$b_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE) + phyper(input$a_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4))
        ))
      )
    } else if (input$discretas == "Hypergeometric" && input$tipo == "Probabilities") {
      if (input$lower_tail_hyperP == "lower.tail") {
        n <- input$n_hypergeometric # Tamaño de la muestra
        N <- input$N_hypergeometric # Tamaño de la población
        M <- input$M_hypergeometric # Número de éxitos en la población
        p <- input$probH # Probabilidad de interés

        x <- qhyper(p, M, N - M, n, lower.tail = TRUE)

        return(paste("The value of x for which P(X < x) =", p, "es:", x, "\n"))
      } else {
        n <- input$n_hypergeometric # Tamaño de la muestra
        N <- input$N_hypergeometric # Tamaño de la población
        M <- input$M_hypergeometric # Número de éxitos en la población
        p <- input$probH # Probabilidad de interés

        x <- phyper(p, M, N - M, n, lower.tail = FALSE)



        return(paste("The value of x for which P(X >= x) =", p, "es:", x, "\n"))
      }
    } else if (input$discretas == "Negative Binomial" && input$tipo == "Values") {
      withMathJax(
        paste0(case_when(
          input$lower_tail_negativebinomial == "exact" ~ paste0("\\(P(X = \\)", " ", input$x_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE) - pnbinom(input$x_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)),
          input$lower_tail_negativebinomial == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x1_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4)),
          input$lower_tail_negativebinomial == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$x2_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE), 4)),
          input$lower_tail_negativebinomial == "interval" ~ paste0("\\(P(\\)", input$a_negativebinomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_negativebinomial > input$b_negativebinomial, "a must be less than or equal to b", round(pnbinom(input$b_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE) - pnbinom(input$a_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4))),
          input$lower_tail_negativebinomial == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_negativebinomial, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_negativebinomial, "\\()\\)", " ", "\\( = \\)", " ", round(pnbinom(input$bt_negativebinomial - 1, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE) + pnbinom(input$at_negativebinomial, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), 4))
        ))
      )
    } else if (input$discretas == "Negative Binomial" && input$tipo == "Probabilities") {
      if (input$lower_tail_negativeP == "lower.tail") {
        size <- input$r_negativebinomial
        p <- input$p_negativebinomial
        prob <- input$probN

        w <- qnbinom(prob, size, p, lower.tail = TRUE)

        return(paste("The value of x for which P(X < x) =", prob, "es:", w, "\n"))
      } else {
        size <- input$r_negativebinomial
        p <- input$p_negativebinomial
        prob <- input$probN

        w <- qnbinom(prob, size, p, lower.tail = FALSE)

        return(paste("The value of x for which P(X >= x) =", prob, "es:", w, "\n"))
      }
    } else if (input$discretas == "Poisson" && input$tipo == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_poisson == "exact" ~ paste0("\\(P(X = \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$x_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
            input$lower_tail_poisson == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x1_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
            input$lower_tail_poisson == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x2_poisson - 1, lambda = input$lambda_poisson, lower.tail = FALSE), 4)),
            input$lower_tail_poisson == "interval" ~ paste0("\\(P(\\)", input$a_poisson, " ", "\\(\\leq X\\leq \\)", " ", input$b_poisson, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_poisson > input$b_poisson, "a must be less than or equal to b", round(ppois(input$b_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$a_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4))),
            input$lower_tail_poisson == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_poisson, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_poisson, "\\()\\)", " ", "\\( = \\)", round(ppois(input$b_poisson - 1, lambda = input$lambda_poisson, lower.tail = FALSE) + ppois(input$a_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4))
          )
        )
      )
    } else if (input$discretas == "Poisson" && input$tipo == "Probabilities") {
      if (input$lower_tail_poissonP == "lower.tail") {
        w <- qpois(input$probP, input$lambda_poisson, lower.tail = TRUE)


        return(paste("The value of x for which P(X < x) =", input$probP, "es:", w, "\n"))
      } else {
        w <- qpois(input$probP, input$lambda_poisson, lower.tail = FALSE)

        return(paste("The value of x for which P(X >= x) =", input$probP, "es:", w, "\n"))
      }
    }
  })


  output$parameters_distribution1 <- renderUI({
    if (input$discretas == "Binomial") {
      withMathJax(
        helpText(
          "\\(X \\sim Bin(n = \\)", " ", input$n_binomial, ", ", "\\(p = \\)", " ", input$p_binomial, "\\()\\)", "  ", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X)= \\)", round(input$n_binomial * input$p_binomial, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) =\\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 3)
        )
      )
    } else if (input$discretas == "Geometric") {
      withMathJax(
        helpText(
          "\\(X \\sim Geom(p = \\)", " ", input$p_geometric, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) =\\)", round((1 - input$p_geometric) / input$p_geometric, 4), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt((1 - input$p_geometric) / (input$p_geometric^2)), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) =\\)", round((1 - input$p_geometric) / (input$p_geometric^2), 4)
        )
      )
    } else if (input$discretas == "Hypergeometric") {
      withMathJax(
        helpText(
          "\\(X \\sim HG(n = \\)", " ", input$n_hypergeometric, ", ", "\\(N = \\)", " ", input$N_hypergeometric, ", ", "\\(M = \\)", " ", input$M_hypergeometric, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(input$n_hypergeometric * (input$M_hypergeometric / input$N_hypergeometric), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) =\\)", round(sqrt(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1))), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(input$n_hypergeometric * input$M_hypergeometric / input$N_hypergeometric * (1 - (input$M_hypergeometric / input$N_hypergeometric)) * ((input$N_hypergeometric - input$n_hypergeometric) / (input$N_hypergeometric - 1)), 3)
        )
      )
    } else if (input$discretas == "Negative Binomial") {
      withMathJax(
        helpText(
          "\\(X \\sim NG(r = \\)", " ", input$r_negativebinomial, ", ", "\\(p = \\)", " ", input$p_negativebinomial, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / input$p_negativebinomial), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2))), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round((input$r_negativebinomial * (1 - input$p_negativebinomial) / (input$p_negativebinomial^2)), 3)
        )
      )
    } else if (input$discretas == "Poisson") {
      withMathJax(
        helpText(
          "\\(X \\sim Pois(\\lambda = \\)", " ", input$lambda_poisson, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\lambda = \\)", round(input$lambda_poisson, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\sqrt{\\lambda} = \\)", round(sqrt(input$lambda_poisson), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\lambda = \\)", round(input$lambda_poisson, 3)
        )
      )
    }
  })


  observe({
    req(input$n_binomial)
    values$Binomial <- data.frame(
      x = 0:input$n_binomial,
      pmf = dbinom(
        x = 0:input$n_binomial,
        size = input$n_binomial,
        prob = input$p_binomial
      ),
      cdf = pbinom(
        q = 0:input$n_binomial,
        size = input$n_binomial,
        prob = input$p_binomial,
        lower.tail = TRUE
      )
    )
  })



  output$binomialPlot_exact <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= input$x_binomial & x <= input$x_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    } else {
      return(NULL)
    }
  })

  output$binomialPlot_Probabilities_lower <- renderPlot({
    if (input$tipo == "Probabilities") {
      y <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = TRUE)
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x <= y, "2", "Other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$binomialPlot_Probabilities_upper <- renderPlot({
    if (input$tipo == "Probabilities") {
      y <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = FALSE)
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= y, "2", "Other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$binomialPlot_lower <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x <= input$x1_binomial, "2", "Other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$binomialPlot_upper <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= input$x2_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))

      values$p
    }
  })
  output$binomialPlot_interval <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= input$a_binomial & x <= input$b_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$binomialPlot_tails <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x <= input$at_binomial | x >= input$bt_binomial, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$result <- renderUI({
    if (input$discretas == "Binomial" && input$tipo == "Probabilities") {
      if (input$lower_tail_binomialP == "lower.tail") {
        x <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = TRUE)
        return(paste("The value of x is:", x))
      } else {
        x <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    }

    if (input$discretas == "Geometric" && input$tipo == "Probabilities") {
      if (input$lower_tail_geometricP == "lower.tail") {
        x <- qgeom(input$probG, input$p_geometric, lower.tail = TRUE)
        return(paste("The value of x is:", x))
      } else {
        x <- qgeom(input$probG, input$p_geometric, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    } else if (input$discretas == "Hypergeometric" && input$tipo == "Probabilities") {
      if (input$lower_tail_hyperP == "lower.tail") {
        x <- qhyper(input$probH, m = input$N_hypergeometric, n = input$M_hypergeometric, k = input$n_hypergeometric, lower.tail = TRUE)
        return(paste("The value of x is:", x + 1))
      } else {
        x <- qhyper(input$probH, m = input$N_hypergeometric, n = input$M_hypergeometric, k = input$n_hypergeometric, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    } else if (input$discretas == "Negative Binomial" && input$tipo == "Probabilities") {
      if (input$lower_tail_negativeP == "lower.tail") {
        x <- qnbinom(input$probN, input$r_negativebinomial, input$p_negativebinomial, lower.tail = TRUE)
        return(paste("The value of x is:", x + 1))
      } else {
        x <- qnbinom(input$probN, input$r_negativebinomial, input$p_negativebinomial, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    } else if (input$discretas == "Poisson" && input$tipo == "Probabilities") {
      if (input$lower_tail_poissonP == "lower.tail") {
        x <- qpois(input$probP, input$lambda_poisson, lower.tail = TRUE)
        return(paste("The value of x is:", x + 1))
      } else {
        x <- qpois(input$probP, input$lambda_poisson, lower.tail = FALSE)
        return(paste("The value of x is:", x))
      }
    }
  })
  # output$binomialPlot_loweracum <- renderPlot({
  # if (input$tipo== "Values") {
  # values$pacum <- data.frame(heads = 0:input$n_binomial,
  #  pmf = dbinom(x = 0:input$n_binomial,
  #            size = input$n_binomial,
  #            prob = input$p_binomial),
  # cdf = pbinom(q = 0:input$n_binomial,
  #             size = input$n_binomial,
  #             prob = input$p_binomial,
  #             lower.tail = TRUE)) %>%
  # dplyr::mutate(Heads = ifelse(heads == input$x1_binomial, "2", "Other")) %>%
  # ggplot(aes(x = factor(heads), y = cdf, group=1)) +
  # geom_step(color="blue") + annotate("point",x=input$x1_binomial+1,y=0,size=3,shape=25,fill = "blue")+
  # geom_segment(aes(x = input$x1_binomial+1, y = 0.01, xend = input$x1_binomial+1, yend =cdf[match(input$x1_binomial,heads)]),linetype="dashed", color = "blue")+
  # geom_segment(aes(x = 0, y = cdf[match(input$x1_binomial, heads)], xend = input$x1_binomial+1, yend =cdf[match(input$x1_binomial, heads)]),linetype="dashed", color = "blue")+
  # geom_text(aes(label = round(cdf, 3), y = cdf + 0.05),
  #   position = position_dodge(0.9),size = 3, vjust = 0) +
  # theme_minimal() +
  # theme(legend.position = "none",axis.line = element_line(color="gray"),axis.ticks = element_line(color="gray")) +
  # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
  # theme(plot.title = element_text(face = "bold", hjust = 0)) +
  # ylab("Density") +
  # xlab("x")+ scale_fill_manual(values = c("#659bf7", "white"))+scale_x_discrete()
  # values$pacum}

  # })

  # output$binomialPlot_upperacum <- renderPlot({
  # if (input$tipo== "Values") {
  # q<- input$n_binomial-1
  # values$pacum <- data.frame(x = 0:input$n_binomial,
  # cdf = pbinom(q = -1:q, size = input$n_binomial, prob = input$p_binomial,
  # lower.tail = FALSE)) %>%
  # dplyr::mutate(Heads = ifelse(x== input$x2_binomial, "-", "+"))%>%
  # ggplot(aes(x = factor(x), y = cdf, group=1)) +
  # geom_step(color="blue") + annotate("point",x=input$x2_binomial+1,y=0,size=3,shape=25,fill = "blue")+
  # geom_segment(aes(x = input$x2_binomial+1, y = 0.01, xend = input$x2_binomial+1, yend =cdf[match(input$x2_binomial,x)]),linetype="dashed", color = "blue")+geom_segment(aes(x = 0, y = cdf[match(input$x2_binomial, x)], xend = input$x2_binomial+1, yend =cdf[match(input$x2_binomial, x)]),linetype="dashed", color = "blue")+
  # geom_text(aes(label = round(cdf, 3), y = cdf + 0.05),
  #   position = position_dodge(0.9),size = 3,vjust = 0) +
  # theme_minimal() +
  # theme(legend.position = "none",axis.line = element_line(color="gray"),axis.ticks = element_line(color="gray")) +
  # ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
  # theme(plot.title = element_text(face = "bold", hjust = 0)) +
  #  ylab("Density") +
  # xlab("x")+ scale_fill_manual(values = c("#659bf7", "white"))+scale_x_discrete()
  # values$pacum}

  # })

  observe({
    req(input$p_geometric)
    values$Geometric <- data.frame(
      x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))),
      pmf = dgeom(
        x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))),
        prob = input$p_geometric
      ),
      cdf = pgeom(q = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))
      ), prob = input$p_geometric)
    )
  })

  output$geometricPlot_exact <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x >= input$x_geometric & x <= input$x_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 2), y = pmf + 0.05),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " Distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$geometricPlot_lower <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x <= input$x1_geometric, "2", "Other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$geometricPlot_upper <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x >= input$x2_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$geometricPlot_probabilities_upper <- renderPlot({
    if (input$tipo == "Probabilities") {
      w <- qgeom(input$probG, input$p_geometric, lower.tail = FALSE)
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x >= w, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$geometricPlot_probabilities_lower <- renderPlot({
    if (input$tipo == "Probabilities") {
      w <- qgeom(input$probG, input$p_geometric, lower.tail = TRUE)
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x <= w, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })




  output$geometricPlot_interval <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x >= input$a_geometric & x <= input$b_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " Distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$geometricPlot_tails <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- values$Geometric %>%
        # data.frame(heads = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = dgeom(x = 0:(input$p_geometric + (5 * sqrt((1 - input$p_geometric) / (input$p_geometric^2)))), prob = input$p_geometric)) %>%
        dplyr::mutate(Heads = ifelse(x <= input$at_geometric | x >= input$bt_geometric, "2", "other")) %>%
        ggplot(aes(x = factor(x), y = pmf, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(pmf, 3), y = pmf + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " Distribution: Geom(", input$p_geometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })



  observe({
    values$Hypergeometric <- data.frame(
      x = 0:input$n_hypergeometric,
      cdf = phyper(
        q = 0:input$n_hypergeometric,
        m = input$M_hypergeometric,
        n = (input$N_hypergeometric - input$M_hypergeometric),
        k = input$n_hypergeometric
      ),
      pmf = dhyper(
        x = 0:input$n_hypergeometric,
        m = input$M_hypergeometric,
        n = (input$N_hypergeometric - input$M_hypergeometric),
        k = input$n_hypergeometric
      )
    )
  })

  output$hypergeometricPlot_probabilities_lower <- renderPlot({
    if (input$tipo == "Probabilities") {
      n <- input$n_hypergeometric # Tamaño de la muestra
      N <- input$N_hypergeometric # Tamaño de la población
      M <- input$M_hypergeometric # Número de éxitos en la población
      p <- input$probH # Probabilidad de interés
      # Encontrar el valor de x tal que P(X < x) = p
      w <- qhyper(p, M, N - M, n, lower.tail = TRUE)
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads <= w, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })



  output$hypergeometricPlot_probabilities_upper <- renderPlot({
    if (input$tipo == "Probabilities") {
      n <- input$n_hypergeometric # Tamaño de la muestra
      N <- input$N_hypergeometric # Tamaño de la población
      M <- input$M_hypergeometric # Número de éxitos en la población
      p <- input$probH # Probabilidad de interés

      w <- qhyper(p, M, N - M, n, lower.tail = FALSE)
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads >= w, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$hypergeometricPlot_exact <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        # values$p <- values$Hypergeometric %>%
        dplyr::mutate(Heads = ifelse(heads <= input$x_hypergeometric & heads >= input$x_hypergeometric, "-", "+")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$hypergeometricPlot_lower <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$x1_hypergeometric, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$hypergeometricPlot_upper <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$x2_hypergeometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$hypergeometricPlot_interval <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$a_hypergeometric & heads <= input$b_hypergeometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$hypergeometricPlot_tails <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$at_hypergeometric | heads >= input$bt_hypergeometric, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: HG(", input$n_hypergeometric, ", ", input$N_hypergeometric, ", ", input$M_hypergeometric, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })






  output$negativebinomialPlot_exact <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$x_negativebinomial & heads <= input$x_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$negativebinomialPlot_lower <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$x1_negativebinomial, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$negativebinomialPlot_upper <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$x2_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$negativebinomialPlot_Probalities_lower <- renderPlot({
    if (input$tipo == "Probabilities") {
      size <- input$r_negativebinomial
      p <- input$p_negativebinomial
      prob <- input$probN

      W <- qnbinom(prob, size, p, lower.tail = TRUE)


      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = TRUE), size = input$r_negativebinomial, prob = p)) %>%
        dplyr::mutate(Heads = ifelse(heads <= W, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$negativebinomialPlot_Probabilities_upper <- renderPlot({
    if (input$tipo == "Probabilities") {
      size <- input$r_negativebinomial
      p <- input$p_negativebinomial
      prob <- input$probN

      W <- qnbinom(prob, size, p, lower.tail = FALSE)


      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = p, lower.tail = TRUE), size = input$r_negativebinomial, prob = p)) %>%
        dplyr::mutate(Heads = ifelse(heads >= W, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })





  output$negativebinomialPlot_interval <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$a_negativebinomial & heads <= input$b_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$negativebinomialPlot_tails <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$at_negativebinomial | heads >= input$bt_negativebinomial, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: NG(", input$r_negativebinomial, ", ", input$p_negativebinomial, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$poissonPlot_exact <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$x_poisson & heads <= input$x_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Poisson(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })


  output$poissonPlot_lower <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$x1_poisson, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$poissonPlot_upper <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads > input$x2_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$poissonPlot_interval <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads >= input$a_poisson & heads <= input$b_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Poisson(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$poissonPlot_tails <- renderPlot({
    if (input$tipo == "Values") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads <= input$at_poisson | heads >= input$bt_poisson, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Poisson(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$poissonPlot_Probabilities_lower <- renderPlot({
    w <- qpois(input$probP, input$lambda_poisson, lower.tail = TRUE)
    if (input$tipo == "Probabilities") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads <= w, "2", "Other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })
  output$poissonPlot_Probabilities_upper <- renderPlot({
    w <- qpois(input$probP, input$lambda_poisson, lower.tail = FALSE)
    if (input$tipo == "Probabilities") {
      values$p <- data.frame(heads = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
        dplyr::mutate(Heads = ifelse(heads >= w, "2", "other")) %>%
        ggplot(aes(x = factor(heads), y = prob, fill = Heads)) +
        geom_col(color = "blue", width = 0.3) +
        geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        # ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x") +
        scale_fill_manual(values = c("#659bf7", "white"))
      values$p
    }
  })

  output$table_discrete <- DT::renderDT({
    if (input$tipo == "Probabilities") {
      if (input$discretas == "Select distribution") {
        return(NULL)
      }
      if (input$discretas == "Binomial" && input$tipo == "Probabilities" && input$lower_tail_binomialP == "lower.tail") {
        y <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = TRUE)
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x <= y, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Binomial" && input$tipo == "Probabilities" && input$lower_tail_binomialP == "upper.tail") {
        y <- qbinom(input$probB, input$n_binomial, input$p_binomial, lower.tail = TRUE)
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x >= y, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      }


      if (input$discretas == "Geometric" & input$lower_tail_geometricP == "lower.tail") {
        w <- qgeom(input$probG, input$p_geometric, lower.tail = TRUE)
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x <= w, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Geometric" & input$lower_tail_geometricP == "upper.tail") {
        w <- qgeom(input$probG, input$p_geometric, lower.tail = FALSE)
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x >= w, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      }

      if (input$discretas == "Hypergeometric" & input$lower_tail_hyperP == "lower.tail") {
        n <- input$n_hypergeometric # Tamaño de la muestra
        N <- input$N_hypergeometric # Tamaño de la población
        M <- input$M_hypergeometric # Número de éxitos en la población
        p <- input$probH # Probabilidad de interés

        w <- qhyper(p, M, N - M, n, lower.tail = TRUE)
        tabla <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
          dplyr::mutate(Heads = ifelse(heads <= w, "+", "-"))
        tabla
      } else if (input$discretas == "Hypergeometric" & input$lower_tail_hyperP == "upper.tail") {
        n <- input$n_hypergeometric # Tamaño de la muestra
        N <- input$N_hypergeometric # Tamaño de la población
        M <- input$M_hypergeometric # Número de éxitos en la población
        p <- input$probH # Probabilidad de interés

        w <- qhyper(p, M, N - M, n, lower.tail = FALSE)
        tabla <- values$Hypergeometric %>%
          dplyr::mutate(Heads = ifelse(x >= w, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      }


      if (input$discretas == "Negative Binomial" & input$lower_tail_negativeP == "lower.tail") {
        size <- input$r_negativebinomial
        p <- input$p_negativebinomial
        prob <- input$probN

        w <- qnbinom(prob, size, p, lower.tail = TRUE)

        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = size, prob = p, lower.tail = FALSE):qnbinom(0.999, size = size, prob = p, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = size, prob = p, lower.tail = FALSE):qnbinom(0.999, size = size, prob = p, lower.tail = TRUE), size = size, prob = p)) %>%
          dplyr::mutate(Heads = ifelse(x <= w, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativeP == "upper.tail") {
        size <- input$r_negativebinomial
        p <- input$p_negativebinomial
        prob <- input$probN

        w <- qnbinom(prob, size, p, lower.tail = FALSE)

        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = size, prob = p, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x >= w, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      }



      if (input$discretas == "Poisson" & input$lower_tail_poissonP == "lower.tail") {
        w <- qpois(input$probP, input$lambda_poisson, lower.tail = TRUE)

        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x <= w, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poissonP == "upper.tail") {
        w <- qpois(input$probP, input$lambda_poisson, lower.tail = FALSE)
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x >= w, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      }
    }



    if (input$tipo == "Values") {
      if (input$discretas == "Select distribution") {
        return(NULL)
      }



      if (input$discretas == "Binomial" && input$tipo == "Values" & input$lower_tail_binomial == "lower.tail") {
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x <= input$x1_binomial, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Binomial" && input$tipo == "Values" & input$lower_tail_binomial == "upper.tail") {
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x >= input$x1_binomial, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Binomial" && input$tipo == "Values" & input$lower_tail_binomial == "interval") {
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x >= input$a_binomial & x <= input$b_binomial, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Binomial" && input$tipo == "Values" & input$lower_tail_binomial == "tails") {
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x <= input$at_binomial | x >= input$bt_binomial, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Binomial" && input$tipo == "Values" & input$lower_tail_binomial == "exact") {
        tabla <- values$Binomial %>%
          dplyr::mutate(Heads = ifelse(x >= input$x_binomial & x <= input$x_binomial, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      }





      if (input$discretas == "Geometric" & input$lower_tail_geometric == "lower.tail") {
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x <= input$x1_geometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Geometric" & input$lower_tail_geometric == "upper.tail") {
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$x2_geometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Geometric" & input$lower_tail_geometric == "interval") {
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$a_geometric & x <= input$b_geometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Geometric" & input$lower_tail_geometric == "tails") {
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x <= input$at_geometric | x >= input$bt_geometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Geometric" & input$lower_tail_geometric == "exact") {
        tabla <- values$Geometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$x_geometric & x <= input$x_geometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      }




      if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "lower.tail") {
        tabla <- data.frame(heads = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), prob = dhyper(x = qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE):qhyper(0.99999, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric)) %>%
          dplyr::mutate(Heads = ifelse(heads <= input$x1_hypergeometric, "+", "-"))

        # dplyr::mutate(Heads = ifelse(x <= input$x1_hypergeometric, "+", "-"))#%>%
        # dplyr::select(x,prob,Heads)
        tabla
      } else if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "upper.tail") {
        tabla <- values$Hypergeometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$x2_hypergeometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "interval") {
        tabla <- values$Hypergeometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$a_hypergeometric & x <= input$b_hypergeometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "tails") {
        tabla <- values$Hypergeometric %>%
          dplyr::mutate(Heads = ifelse(x <= input$at_hypergeometric | x >= input$bt_hypergeometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "exact") {
        tabla <- values$Hypergeometric %>%
          dplyr::mutate(Heads = ifelse(x >= input$x_hypergeometric & x <= input$x_hypergeometric, "+", "-")) %>%
          dplyr::select(x, pmf, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativebinomial == "lower.tail") {
        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x <= input$x1_negativebinomial, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativebinomial == "upper.tail") {
        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$x2_negativebinomial, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativebinomial == "interval") {
        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$a_negativebinomial & x <= input$b_negativebinomial, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativebinomial == "tails") {
        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x <= input$at_negativebinomial | x >= input$bt_negativebinomial, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Negative Binomial" & input$lower_tail_negativebinomial == "exact") {
        tabla <- values$p <- data.frame(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), prob = dnbinom(x = qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = FALSE):qnbinom(0.999, size = input$r_negativebinomial, prob = input$p_negativebinomial, lower.tail = TRUE), size = input$r_negativebinomial, prob = input$p_negativebinomial)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$x_negativebinomial & x <= input$x_negativebinomial, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poisson == "lower.tail") {
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x <= input$x1_poisson, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poisson == "upper.tail") {
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$x2_poisson, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poisson == "interval") {
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$a_poisson & x <= input$b_poisson, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poisson == "tails") {
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x <= input$at_poisson | x >= input$bt_poisson, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      } else if (input$discretas == "Poisson" & input$lower_tail_poisson == "exact") {
        tabla <- data.frame(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), prob = dpois(x = qpois(0.99999, lambda = input$lambda_poisson, lower.tail = FALSE):qpois(0.99999, lambda = input$lambda_poisson, lower.tail = TRUE), lambda = input$lambda_poisson)) %>%
          dplyr::mutate(Heads = ifelse(x >= input$x_poisson & x <= input$x_poisson, "+", "-")) %>%
          dplyr::select(x, prob, Heads)
        tabla
      }
    }
    DT::datatable(
      data = tabla, extensions = "Buttons", width = "10%",
      rownames = FALSE,
      options = list(
        dom = "Brt", scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "excel",
          "copy"
        ),
        pagelength = 21,
        lengthMenu = list(
          c(21, 45, 100, -1),
          c("21", "45", "100", "All")
        )
      )
    ) %>%
      DT::formatRound(columns = c(2:3), digits = 4) %>%
      DT::formatStyle("Heads",
        target = "row", lineHeight = "45%", fontSize = "80%",
        backgroundColor = DT::styleEqual(c("+", "-"), c("#659bf7", "white"))
      )
  })

  observe({
    if (input$discretas == "Select distribution") {
      return(NULL)
    }
    if (input$discretas == "Binomial" & input$tipo == "Probabilities") {
      tabla <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= input$x1_binomial & x <= input$x1_binomial, "+", "-")) %>%
        dplyr::select(x, cdf, Heads)
      values$tabla <- tabla
    } else if (input$discretas == "Binomial" & input$lower_tail_binomial == "lower.tail") {
      tabla <- values$Binomial %>%
        dplyr::mutate(Heads = ifelse(x >= input$x1_binomial & x <= input$x1_binomial, "+", "-")) %>%
        dplyr::select(x, cdf, Heads)
      values$tabla <- tabla
    } else if (input$discretas == "Geometric" & input$lower_tail_geometric == "lower.tail") {
      tabla <- values$Geometric %>%
        dplyr::mutate(Heads = ifelse(x >= input$x1_geometric & x <= input$x1_geometric, "+", "-")) %>%
        dplyr::select(x, cdf, Heads)
      values$tabla <- tabla
    } else if (input$discretas == "Hypergeometric" & input$lower_tail_hypergeometric == "lower.tail") {
      tabla <- values$Hypergeometric %>%
        dplyr::mutate(Heads = ifelse(x <= input$x1_hypergeometric & x >= input$x1_hypergeometric, "+", "-")) %>%
        dplyr::select(x, cdf, Heads)
      values$tabla <- tabla
    }
  })

  output$table_discrete_acum <- DT::renderDT({
    req(values$tabla)
    if (input$discretas == "Select distribution") {
      return(NULL)
    }
    DT::datatable(
      data = values$tabla, extensions = "Buttons", width = "10%",
      rownames = FALSE,
      options = list(
        dom = "Brt",
        scrollX = TRUE,
        fixedColumns = FALSE,
        buttons = c(
          "pageLength",
          "excel",
          "copy",
        ),
        pagelength = -1
      )
    ) %>%
      DT::formatRound(columns = c(2:3), digits = 4) %>%
      DT::formatStyle("Heads",
        target = "row", lineHeight = "45%", fontSize = "80%",
        backgroundColor = DT::styleEqual(c("+", "-"), c("#659bf7", "white"))
      )
  })

  # actualizar
  observeEvent(input$refresh1, {
    shinyWidgets::updatePickerInput(session,
      inputId = "discretas",
      label = "Distribution",
      choices = c(
        "Select distribution",
        "Binomial",
        "Geometric",
        # "Geometric (II)",
        "Hypergeometric",
        "Negative Binomial",
        # "Negative Binomial (II)",
        "Poisson"
      ),
      options = list(style = "btn-primary")
    )
  })


  ## ---------Fin discretas



  ## ----------Continuas
  output$results_distribution <- renderUI({
    if (input$continuas == "Normal" && input$tipoC == "Values") {
      withMathJax(
        paste0(case_when(
          input$lower_tail_normal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", round((input$x1_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x1_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)),
          input$lower_tail_normal == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", round((input$x2_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x2_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), 4)),
          input$lower_tail_normal == "interval" ~ paste0("\\(P(\\)", input$a_normal, " ", "\\(\\leq X\\leq \\)", " ", input$b_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(\\)", round((input$a_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), " ", "\\(\\leq Z\\leq \\)", " ", round((input$b_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_normal > input$b_normal, "a must be less than or equal to b", round(pnorm(input$b_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$a_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4))),
          input$lower_tail_normal == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_normal, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_normal, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(\\)", round((input$at_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), " ", "\\(\\geq Z\\geq \\)", " ", round((input$bt_normal - input$mean_normal) / ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), 2), "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_normal > input$bt_normal, "a must be less than or equal to b", round(1 - (pnorm(input$bt_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$at_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE)), 4)))

        ))
      )
    } else if (input$continuas == "Normal" && input$tipoC == "Probabilities") {
      sd_prev <- ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
      c <- qnorm(input$probC, mean = input$mean_normal, sd = sd_prev)
      b <- qnorm(1 - input$probC, mean = input$mean_normal, sd = sd_prev)

      withMathJax(
        paste0(case_when(
          input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z \\leq \\)", " ", round((c - input$mean_normal) / sd_prev, 2), "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          input$Continuasx == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", b, "\\()\\)", " ", "\\( = \\)", " ", "\\(P(Z > \\)", " ", round((b - input$mean_normal) / sd_prev, 2), "\\()\\)", " ", "\\( = \\)", " ", input$probC)
        ))
      )
    } else if (input$continuas == "t-Student" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_student == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x1_student, df = input$df_student, lower.tail = TRUE), 4)),
            input$lower_tail_student == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_student, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x2_student, df = input$df_student, lower.tail = FALSE), 4)),
            input$lower_tail_student == "interval" ~ paste0("\\(P(\\)", input$a_student, " ", "\\(\\leq X\\leq \\)", " ", input$b_student, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_student > input$b_student, "a must be less than or equal to b", round(pt(input$b_student, df = input$df_student, lower.tail = TRUE) - pt(input$a_student, df = input$df_student, lower.tail = TRUE), 4))),
            input$lower_tail_student == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_student, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_student, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_student > input$bt_student, "a must be less than or equal to b", round(1 - (pt(input$bt_student, df = input$df_student, lower.tail = TRUE) - pt(input$at_student, df = input$df_student, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "t-Student" && input$tipoC == "Probabilities") {
      w <- qt(input$probC, df = input$df_student, lower.tail = FALSE)
      t <- qt(input$probC, df = input$df_student, lower.tail = TRUE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", t, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Beta" && input$tipoC == "Values") {
      withMathJax(
        paste0(case_when(
          input$lower_tail_beta == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x1_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4)),
          input$lower_tail_beta == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_beta, "\\()\\)", " ", "\\( = \\)", " ", round(pbeta(input$x2_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), 4)),
          input$lower_tail_beta == "interval" ~ paste0("\\(P(\\)", input$a_beta, " ", "\\(\\leq X\\leq \\)", " ", input$b_beta, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_beta > input$b_beta, "a must be less than or equal to b", round(pbeta(input$b_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE) - pbeta(input$a_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE), 4))),
          input$lower_tail_beta == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_beta, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_beta, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_beta > input$bt_beta, "a must be less than or equal to b", round(1 - (pbeta(input$bt_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE) - pbeta(input$at_beta, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE)), 4)))
        ))
      )
    } else if (input$continuas == "Beta" && input$tipoC == "Probabilities") {
      c <- qbeta(input$probC, input$alpha_beta, input$beta_beta, lower.tail = TRUE)
      w <- qbeta(input$probC, input$alpha_beta, input$beta_beta, lower.tail = FALSE)
      withMathJax(
        paste0(case_when(
          input$lower_tail_beta == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          input$lower_tail_beta == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
        ))
      )
    } else if (input$continuas == "Chi^2" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_chisquare == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x1_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4)),
            input$lower_tail_chisquare == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", input$x2_chisquare, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x2_chisquare, df = input$df_chisquare, lower.tail = FALSE), 4)),
            input$lower_tail_chisquare == "interval" ~ paste0("\\(P(\\)", input$a_chisquare, " ", "\\(\\leq X\\leq \\)", " ", input$b_chisquare, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_chisquare > input$b_chisquare, "a must be less than or equal to b", round(pchisq(input$b_chisquare, df = input$df_chisquare, lower.tail = TRUE) - pchisq(input$a_chisquare, df = input$df_chisquare, lower.tail = TRUE), 4))),
            input$lower_tail_chisquare == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_chisquare, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_chisquare, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_chisquare > input$bt_chisquare, "a must be less than or equal to b", round(1 - (pchisq(input$bt_chisquare, df = input$df_chisquare, lower.tail = TRUE) - pchisq(input$at_chisquare, df = input$df_chisquare, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Chi^2" && input$tipoC == "Probabilities") {
      c <- qchisq(input$probC, input$df_chisquare, lower.tail = TRUE)
      w <- qchisq(input$probC, input$df_chisquare, lower.tail = FALSE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X \\geq \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Exponential" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_exponential == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x1_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4)),
            input$lower_tail_exponential == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_exponential, "\\()\\)", " ", "\\( = \\)", " ", round(pexp(input$x2_exponential, rate = input$rate_exponential, lower.tail = FALSE), 4)),
            input$lower_tail_exponential == "interval" ~ paste0("\\(P(\\)", input$a_exponential, " ", "\\(\\leq X\\leq \\)", " ", input$b_exponential, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_exponential > input$b_exponential, "a must be less than or equal to b", round(pexp(input$b_exponential, rate = input$rate_exponential, lower.tail = TRUE) - pexp(input$a_exponential, rate = input$rate_exponential, lower.tail = TRUE), 4))),
            input$lower_tail_exponential == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_exponential, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_exponential, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_exponential > input$bt_exponential, "a must be less than or equal to b", round(1 - (pexp(input$bt_exponential, rate = input$rate_exponential, lower.tail = TRUE) - pexp(input$at_exponential, rate = input$rate_exponential, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Exponential" && input$tipoC == "Probabilities") {
      c <- qexp(input$probC, input$rate_exponential, lower.tail = TRUE)
      w <- qexp(input$probC, input$rate_exponential, lower.tail = FALSE)

      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC)
          )
        )
      )
    } else if (input$continuas == "F-Fisher" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_fisher == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x1_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE), 4)),
            input$lower_tail_fisher == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_fisher, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x2_fisher, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE), 4)),
            input$lower_tail_fisher == "interval" ~ paste0("\\(P(\\)", input$a_fisher, " ", "\\(\\leq X\\leq \\)", " ", input$b_fisher, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_fisher > input$b_fisher, "a must be less than or equal to b", round(pf(input$b_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE) - pf(input$a_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE), 4))),
            input$lower_tail_fisher == "tails" ~paste0("\\(P(X \\leq \\)", input$at_fisher, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_fisher, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_fisher > input$bt_fisher, "a must be less than or equal to b", round(1 - (pf(input$bt_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE) - pf(input$at_fisher, df1 = input$df1_fisher, df = input$df2_fisher, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "F-Fisher" && input$tipoC == "Probabilities") {
      w <- qf(input$probC, input$df1_fisher, input$df2_fisher, lower.tail = FALSE)
      c <- qf(input$probC, input$df1_fisher, input$df2_fisher, lower.tail = TRUE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Gamma" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_gamma == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x1_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4)),
            input$lower_tail_gamma == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_gamma, "\\()\\)", " ", "\\( = \\)", " ", round(pgamma(input$x2_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), 4)),
            input$lower_tail_gamma == "interval" ~ paste0("\\(P(\\)", input$a_gamma, " ", "\\(\\leq X\\leq \\)", " ", input$b_gamma, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_gamma > input$b_gamma, "a must be less than or equal to b", round(pgamma(input$b_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE) - pgamma(input$a_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE), 4))),
            input$lower_tail_gamma == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_gamma, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_gamma, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_gamma > input$bt_gamma, "a must be less than or equal to b", round(1 - (pgamma(input$bt_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE) - pgamma(input$at_gamma, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Gamma" && input$tipoC == "Probabilities") {
      c <- qgamma(input$probC, input$alpha_gamma, input$beta_gamma, lower.tail = TRUE)
      w <- qgamma(input$probC, input$alpha_gamma, input$beta_gamma, lower.tail = FALSE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Logistics" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_logistic == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x1_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4)),
            input$lower_tail_logistic == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_logistic, "\\()\\)", " ", "\\( = \\)", " ", round(plogis(input$x2_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), 4)),
            input$lower_tail_logistic == "interval" ~ paste0("\\(P(\\)", input$a_logistic, " ", "\\(\\leq X\\leq \\)", " ", input$b_logistic, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_logistic > input$b_logistic, "a must be less than or equal to b", round(plogis(input$b_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE) - plogis(input$a_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE), 4))),
            input$lower_tail_logistic == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_logistic, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_logistic, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_logistic > input$bt_logistic, "a must be less than or equal to b", round(1 - (plogis(input$bt_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE) - plogis(input$at_logistic, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Logistics" && input$tipoC == "Probabilities") {
      c <- qlogis(input$probC, input$location_logistic, input$scale_logistic, lower.tail = TRUE)
      w <- qlogis(input$probC, input$location_logistic, input$scale_logistic, lower.tail = FALSE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Log-Normal" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_lognormal == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_lognormal, "\\()\\)", " ", "\\( = \\)", " ", round(plnorm(input$x1_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4)),
            input$lower_tail_lognormal == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_lognormal, "\\()\\)", " ", "\\( = \\)", " ", "\\( = \\)", " ", round(plnorm(input$x2_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = FALSE), 4)),
            input$lower_tail_lognormal == "interval" ~ paste0("\\(P(\\)", input$a_lognormal, " ", "\\(\\leq X\\leq \\)", " ", input$b_lognormal, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_lognormal > input$b_lognormal, "a must be less than or equal to b", round(plnorm(input$b_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE) - plnorm(input$a_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE), 4))),
            input$lower_tail_lognormal == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_lognormal, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_lognormal, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_lognormal > input$bt_lognormal, "a must be less than or equal to b", round(1 - (plnorm(input$bt_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE) - plnorm(input$at_lognormal, meanlog = input$mean_lognormal, sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal), lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Log-Normal" && input$tipoC == "Probabilities") {
      sd_n <- ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
      c <- qlnorm(input$probC, input$mean_lognormal, sd_n, lower.tail = FALSE)
      w <- qlnorm(input$probC, input$mean_lognormal, sd_n, lower.tail = TRUE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    } else if (input$continuas == "Weibull" && input$tipoC == "Values") {
      withMathJax(
        paste0(
          case_when(
            input$lower_tail_weibull == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x1_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4)),
            input$lower_tail_weibull == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_weibull, "\\()\\)", " ", "\\( = \\)", " ", round(pweibull(input$x2_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), 4)),
            input$lower_tail_weibull == "interval" ~ paste0("\\(P(\\)", input$a_weibull, " ", "\\(\\leq X\\leq \\)", " ", input$b_weibull, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_weibull > input$b_weibull, "a must be less than or equal to b", round(pweibull(input$b_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE) - pweibull(input$a_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE), 4))),
            input$lower_tail_weibull == "tails" ~ paste0("\\(P(X \\leq \\)", input$at_weibull, "\\()\\)", "+", "\\(P(X \\geq \\)", " ", input$bt_weibull, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$at_weibull > input$bt_weibull, "a must be less than or equal to b", round(1 - (pweibull(input$bt_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE) - pweibull(input$at_weibull, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE)), 4)))
          )
        )
      )
    } else if (input$continuas == "Weibull" && input$tipoC == "Probabilities") {
      c <- qweibull(input$probC, input$alpha_weibull, input$beta_weibull, lower.tail = TRUE)
      w <- qweibull(input$probC, input$alpha_weibull, input$beta_weibull, lower.tail = FALSE)
      withMathJax(
        paste0(
          case_when(
            input$Continuasx == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", c, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
            input$Continuasx == "upper.tail" ~ paste0("\\(P(X > \\)", " ", w, "\\()\\)", " ", "\\( = \\)", " ", input$probC),
          )
        )
      )
    }
  })

  output$normalPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x > input$x1_normal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$normalPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x < input$x2_normal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  ################### Normal Plot continua

  output$normalPlot_lower_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      sd_prev <- ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
      c <- qnorm(input$probC, mean = input$mean_normal, sd = sd_prev)

      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = sd_prev
        )
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })



  output$normalPlot_upper_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      sd_prev <- ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
      c <- qnorm(1 - input$probC, mean = input$mean_normal, sd = sd_prev)

      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x < c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })





  output$normalPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x < input$a_normal | x > input$b_normal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$normalPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dnorm(x,
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )
        y[x > input$at_normal & x < input$bt_normal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), qnorm(0.99999, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(
          mean = input$mean_normal,
          sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: N(", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", input$variance_normal, (input$sd_normal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })


  output$studentPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x > input$x1_student] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$studentPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x < input$x2_student] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })


  #### Student probabilities
  output$studentPlot_lower_Probabilities <- renderPlot({
    w <- qt(input$probC, df = input$df_student, lower.tail = TRUE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x > w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$studentPlot_upper_Probabilities <- renderPlot({
    w <- qt(input$probC, df = input$df_student, lower.tail = FALSE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x < w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$studentPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x < input$a_student | x > input$b_student] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$studentPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dt(x, df = input$df_student)
        y[x > input$at_student & x < input$bt_student] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qt(0.99999, df = input$df_student, lower.tail = FALSE), qt(0.99999, df = input$df_student, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = input$df_student)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: t(", input$df_student, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })



  output$betaPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x > input$x1_beta] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$betaPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x < input$x2_beta] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$betaPlot_lower_Probabilities <- renderPlot({
    c <- qbeta(input$probC, input$alpha_beta, input$beta_beta, lower.tail = TRUE)

    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$betaPlot_upper_Probabilities <- renderPlot({
    w <- qbeta(input$probC, input$alpha_beta, input$beta_beta, lower.tail = FALSE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x < w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$betaPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x < input$a_beta | x > input$b_beta] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$betaPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dbeta(x, shape1 = input$alpha_beta, shape2 = input$beta_beta)
        y[x > input$at_beta & x < input$bt_beta] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE), qbeta(0.99999, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dbeta, args = list(shape1 = input$alpha_beta, shape2 = input$beta_beta)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Beta(", input$alpha_beta, ", ", input$beta_beta, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$chisquarePlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x > input$x1_chisquare] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$chisquarePlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x < input$x2_chisquare] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$chisquarePlot_interval <- renderPlot({
  if (input$tipoC == "Values") {
    funcShaded <- function(x) {
      # Usamos dchisq con el parámetro de grados de libertad correspondiente
      y <- dchisq(x, df = input$df_chisquare)
      # Definimos el sombreado entre los límites 'a' y 'b'
      y[x < input$at_chisquare | x > input$bt_chisquare] <- NA
      return(y)
    }

    # Ajustamos el rango de x (ejemplo de 0 a 30, o dinámico según df)
    values$pC <- ggplot(data.frame(x = c(0, max(30, input$df_chisquare * 2))), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
      stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0)) +
      ylab("Density") +
      xlab("x")

    values$pC
  }
})
output$chisquarePlot_tails <- renderPlot({
  if (input$tipoC == "Values") {
    funcShaded <- function(x) {
      y <- dchisq(x, df = input$df_chisquare)
      # Definimos el sombreado fuera del intervalo (at, bt)
      y[x > input$at_chisquare & x < input$bt_chisquare] <- NA
      return(y)
    }

    values$pC <- ggplot(data.frame(x = c(0, max(30, input$df_chisquare * 2))), aes(x = x)) +
      stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
      stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0)) +
      ylab("Density") +
      xlab("x")

    values$pC
  }
})
  output$chisquarePlot_lower_Probabilities <- renderPlot({
    c <- qchisq(input$probC, input$df_chisquare, lower.tail = TRUE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$chisquarePlot_upper_Probabilities <- renderPlot({
    c <- qchisq(input$probC, input$df_chisquare, lower.tail = FALSE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dchisq(x, df = input$df_chisquare)
        y[x < c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qchisq(0.99999, df = input$df_chisquare, lower.tail = FALSE), qchisq(0.99999, df = input$df_chisquare, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = input$df_chisquare)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Chi(", input$df_chisquare, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
# --- FISHER: LOWER TAIL (VALUES) ---
  output$fisherPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x > input$x1_fisher] <- NA
        return(y)
      }
      # Definimos límites del gráfico (de 0 al cuantil 0.999)
      limit_upper <- qf(0.999, df1 = input$df1_fisher, df2 = input$df2_fisher)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("F")
      values$pC
    }
  })

  # --- FISHER: UPPER TAIL (VALUES) ---
  output$fisherPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x < input$x2_fisher] <- NA
        return(y)
      }
      limit_upper <- qf(0.999, df1 = input$df1_fisher, df2 = input$df2_fisher)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("F")
      values$pC
    }
  })

  # --- FISHER: LOWER TAIL (PROBABILITIES) ---
  output$fisherPlot_lower_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      # Calculamos el valor crítico 'c' basado en la probabilidad
      c_val <- qf(input$probC, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE)

      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x > c_val] <- NA
        return(y)
      }
      limit_upper <- qf(0.999, df1 = input$df1_fisher, df2 = input$df2_fisher)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("F")
      values$pC
    }
  })

  # --- FISHER: UPPER TAIL (PROBABILITIES) ---
  output$fisherPlot_upper_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      # Calculamos el valor crítico 'c' para la cola superior
      c_val <- qf(input$probC, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE)

      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x < c_val] <- NA
        return(y)
      }
      limit_upper <- qf(0.999, df1 = input$df1_fisher, df2 = input$df2_fisher)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("F")
      values$pC
    }
  })
  output$fisherPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x < input$a_fisher | x > input$b_fisher] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$fisherPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- df(x, df1 = input$df1_fisher, df2 = input$df2_fisher)
        y[x > input$at_fisher & x < input$bt_fisher] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = input$df1_fisher, df2 = input$df2_fisher)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: F(", input$df1_fisher, ", ", input$df2_fisher, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })


# --- EXPONENTIAL: LOWER TAIL (VALUES) ---
  output$exponentialPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x > input$x1_exponential] <- NA
        return(y)
      }
      # Límite superior dinámico para que se vea bien la cola
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })

  # --- EXPONENTIAL: UPPER TAIL (VALUES) ---
  output$exponentialPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x < input$x2_exponential] <- NA
        return(y)
      }
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })

  # --- EXPONENTIAL: LOWER TAIL (PROBABILITIES) ---
  output$exponentialPlot_lower_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      c_val <- qexp(input$probC, rate = input$rate_exponential, lower.tail = TRUE)
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x > c_val] <- NA
        return(y)
      }
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })

  # --- EXPONENTIAL: UPPER TAIL (PROBABILITIES) ---
  output$exponentialPlot_upper_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      c_val <- qexp(input$probC, rate = input$rate_exponential, lower.tail = FALSE)
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x < c_val] <- NA
        return(y)
      }
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })

  # --- EXPONENTIAL: INTERVAL ---
  output$exponentialPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x < input$a_exponential| x > input$b_exponential] <- NA
        return(y)
      }
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })

  # --- EXPONENTIAL: TAILS ---
  output$exponentialPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dexp(x, rate = input$rate_exponential)
        y[x > input$at_exponential & x < input$bt_exponential] <- NA
        return(y)
      }
      limit_upper <- qexp(0.999, rate = input$rate_exponential)

      values$pC <- ggplot(data.frame(x = c(0, limit_upper)), aes(x = x)) +
        stat_function(fun = dexp, args = list(rate = input$rate_exponential)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") + xlab("x")
      values$pC
    }
  })


  output$gammaPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x > input$x1_gamma] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$gammaPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x < input$x2_gamma] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$gammaPlot_lower_Probabilities <- renderPlot({
    c <- qgamma(input$probC, input$alpha_gamma, input$beta_gamma, lower.tail = TRUE)

    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$gammaPlot_upper_Probabilities <- renderPlot({
    w <- qgamma(input$probC, input$alpha_gamma, input$beta_gamma, lower.tail = FALSE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x < w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$gammaPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x < input$a_gamma | x > input$b_gamma] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$gammaPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dgamma(x, shape = input$alpha_gamma, rate = input$beta_gamma)
        y[x > input$at_gamma & x < input$bt_gamma] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE), qgamma(0.99999, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dgamma, args = list(shape = input$alpha_gamma, rate = input$beta_gamma)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Gamma(", input$alpha_gamma, ", ", input$beta_gamma, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })




  output$logisticPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x > input$x1_logistic] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$logisticPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x < input$x2_logistic] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$logisticPlot_lower_Probabilities <- renderPlot({
    c <- qlogis(input$probC, input$location_logistic, input$scale_logistic, lower.tail = TRUE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$logisticPlot_upper_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      c <- qlogis(input$probC, input$location_logistic, input$scale_logistic, lower.tail = FALSE)
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x < c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$logisticPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x < input$a_logistic | x > input$b_logistic] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$logisticPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlogis(x, location = input$location_logistic, scale = input$scale_logistic)
        y[x > input$at_logistic & x < input$bt_logistic] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE), qlogis(0.99999, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dlogis, args = list(location = input$location_logistic, scale = input$scale_logistic)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Logi(", input$location_logistic, ", ", input$scale_logistic, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })








  output$lognormalPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x > input$x1_lognormal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$lognormalPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x < input$x2_lognormal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })

  output$lognormalPlot_lower_Probabilities <- renderPlot({
    sd_n <- ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)

    w <- qlnorm(input$probC, input$mean_lognormal, sd_n, lower.tail = TRUE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x > w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$lognormalPlot_upper_Probabilities <- renderPlot({
    if (input$tipoC == "Probabilities") {
      sd_n <- ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
      c <- qlnorm(input$probC, input$mean_lognormal, sd_n, lower.tail = FALSE)

      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x < c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })



  output$lognormalPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x < input$a_lognormal | x > input$b_lognormal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$lognormalPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dlnorm(x,
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )
        y[x > input$at_lognormal & x < input$bt_lognormal] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(0, qlnorm(0.9, meanlog = input$mean_lognormal, sdlog = input$sd_lognormal))), aes(x = x)) +
        stat_function(fun = dlnorm, args = list(
          meanlog = input$mean_lognormal,
          sdlog = ifelse(input$variance_sd_lognormal == "variance_true", sqrt(input$variance_lognormal), input$sd_lognormal)
        )) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Lognormal(", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })




  output$weibullPlot_lower <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x > input$x1_weibull] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$weibullPlot_upper <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x < input$x2_weibull] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$weibullPlot_lower_Probabilities <- renderPlot({
    c <- qweibull(input$probC, input$alpha_weibull, input$beta_weibull, lower.tail = TRUE)

    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x > c] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$weibullPlot_upper_Probabilities <- renderPlot({
    w <- qweibull(input$probC, input$alpha_weibull, input$beta_weibull, lower.tail = FALSE)
    if (input$tipoC == "Probabilities") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x < w] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$weibullPlot_interval <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x < input$a_weibull | x > input$b_weibull] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$weibullPlot_tails <- renderPlot({
    if (input$tipoC == "Values") {
      funcShaded <- function(x) {
        y <- dweibull(x, shape = input$alpha_weibull, scale = input$beta_weibull)
        y[x > input$at_weibull & x < input$bt_weibull] <- NA
        return(y)
      }
      values$pC <- ggplot(data.frame(x = c(qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE), qweibull(0.99999, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dweibull, args = list(shape = input$alpha_weibull, scale = input$beta_weibull)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.95, fill = "sky blue") +
        theme_minimal() +
        # ggtitle(paste0(input$distribution, " Distribution: Weibull(", input$alpha_weibull, ", ", input$beta_weibull, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0)) +
        ylab("Density") +
        xlab("x")
      values$pC
    }
  })
  output$parameters_distribution <- renderUI({
    if (input$continuas == "Beta") {
      withMathJax(
        helpText(
          "\\(X \\sim Beta(\\alpha = \\)", " ", input$alpha_beta, ", ", "\\(\\beta = \\)", " ", input$beta_beta, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) =\\)", round(input$alpha_beta / (input$alpha_beta + input$beta_beta), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) =\\)", round(sqrt((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1))), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X)= \\)", round((input$alpha_beta * input$beta_beta) / (((input$alpha_beta + input$beta_beta)^2) * (input$alpha_beta + input$beta_beta + 1)), 3)
        )
      )
    } else if (input$continuas == "Chi^2") {
      withMathJax(
        helpText(
          "\\(X \\sim \\chi^2(gl = \\)", " ", input$df_chisquare, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(input$df_chisquare, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt(2 * input$df_chisquare), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(2 * input$df_chisquare, 3)
        )
      )
    } else if (input$continuas == "Exponential") {
      withMathJax(
        helpText(
          "\\(X \\sim \\ Exp(\\lambda = \\)", " ", input$rate_exponential, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(1 / input$rate_exponential, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(1 / input$rate_exponential, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(1 / (input$rate_exponential^2), 3)
        )
      )
    } else if (input$continuas == "F-Fisher") {
      withMathJax(
        helpText(
          "\\(X \\sim F(gl_1 = \\)", " ", input$df1_fisher, ", ", "\\(gl_2\\)", " = ", input$df2_fisher, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", ifelse(input$df2_fisher > 2, round(input$df2_fisher / (input$df2_fisher - 2), 3), "Undefined"), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", ifelse(input$df2_fisher > 4, round(sqrt((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4))), 3), "Undefined"), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", ifelse(input$df2_fisher > 4, round((2 * input$df2_fisher^2 * (input$df1_fisher + input$df2_fisher - 2)) / (input$df1_fisher * (input$df2_fisher - 2)^2 * (input$df2_fisher - 4)), 3), "Undefined")
        )
      )
    } else if (input$continuas == "Gamma") {
      withMathJax(
        helpText(
          "\\(X \\sim Gamma(\\alpha = \\)", " ", input$alpha_gamma, ", ", "\\(\\beta = \\)", " ", input$beta_gamma, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(input$alpha_gamma / input$beta_gamma, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt(input$alpha_gamma / (input$beta_gamma^2)), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(input$alpha_gamma / (input$beta_gamma^2), 3)
        )
      )
    } else if (input$continuas == "Logistics") {
      withMathJax(
        helpText(
          "\\(X \\sim Logi(\\mu = \\)", " ", input$location_logistic, ", ", "\\(s = \\)", " ", input$scale_logistic, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(input$location_logistic, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt(((input$scale_logistic^2) * (pi^2)) / 3), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(((input$scale_logistic^2) * (pi^2)) / 3, 3)
        )
      )
    } else if (input$continuas == "Log-Normal") {
      withMathJax(
        helpText(
          "\\(X \\sim Lognormal(\\mu = \\)", " ", input$mean_lognormal, ", ", ifelse(input$variance_sd_lognormal == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_lognormal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_lognormal^2)), "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(exp(input$mean_lognormal + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2) / 2)), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(sqrt((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2)))), 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round((exp(ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))) - 1) * exp((2 * input$mean_lognormal) + ifelse(input$variance_sd_lognormal == "variance_true", input$variance_lognormal, (input$sd_lognormal^2))), 3)
        )
      )
    } else if (input$continuas == "Normal") {
      withMathJax(
        helpText(
          "\\(X \\sim \\mathcal{N}(\\mu = \\)", " ", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_normal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_normal^2)), "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(input$mean_normal, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", ifelse(input$variance_sd == "variance_true", round(sqrt(input$variance_normal), 3), round(input$sd_normal, 3)), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", ifelse(input$variance_sd == "variance_true", round(input$variance_normal, 3), round(input$sd_normal^2, 3))
        )
      )
    } else if (input$continuas == "t-Student") {
      withMathJax(
        helpText(
          "\\(X \\sim St(gl = \\)", " ", input$df_student, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", ifelse(input$df_student > 1, 0, "Undefined"), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", ifelse(input$df_student > 2, round(sqrt(input$df_student / (input$df_student - 2)), 3), "Undefined"), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X)= \\)", ifelse(input$df_student > 2, round(input$df_student / (input$df_student - 2), 3), "Undefined")
        )
      )
    } else if (input$continuas == "Weibull") {
      withMathJax(
        helpText(
          "\\(X \\sim Weibull(\\alpha = \\)", " ", input$alpha_weibull, ", ", "\\(\\beta = \\)", " ", input$beta_weibull, "\\()\\)", HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\mu = E(X) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$mu, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma = SD(X) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma, 3), HTML("&nbsp;"), HTML("&nbsp;"),
          "\\(\\sigma^2 = Var(X) = \\)", round(weibullparinv(shape = input$alpha_weibull, scale = input$beta_weibull, loc = 0)$sigma^2, 3)
        )
      )
    }
  })




  plotInput <- reactive({
    values$p
  })

  plotInputC <- reactive({
    values$pC
  })

  observe({
    if (is.null(values$p)) {
      shinyjs::disable("downloadPlot")
    } else {
      shinyjs::enable("downloadPlot")
    }
  })


  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("distributionD", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plotInput(), width = 8, height = 5)
    }
  )

  observe({
    if (is.null(values$pC)) {
      shinyjs::disable("downloadPlot2")
    } else {
      shinyjs::enable("downloadPlot2")
    }
  })

  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("distributionC", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plotInputC(), width = 8, height = 5)
    }
  )


  # actualizar
  observeEvent(input$refresh2, {
    shinyWidgets::updatePickerInput(session,
      inputId = "continuas",
      label = "Distribution",
      choices = c(
        "Select distribution",
        "Normal",
        "t-Student",
        "Chi^2",
        "F-Fisher",
        "Exponential",
        "Logistics",
        "Log-Normal",
        "Gamma",
        "Weibull",
        "Beta"
      ),
      options = list(
        style = "btn-primary"
      )
    )
  })


  output$resultContinuas <- renderText({
    if (input$continuas == "t-Student" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qt(input$probC, df = input$df_student, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qt(input$probC, df = input$df_student, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "Chi^2" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qchisq(input$probC, df = input$df_chisquare, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qchisq(input$probC, df = input$df_chisquare, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "F-Fisher" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qf(input$probC, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qf(input$probC, df1 = input$df1_fisher, df2 = input$df2_fisher, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "Exponential" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qexp(input$probC, rate = input$rate_exponential, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qexp(input$probC, rate = input$rate_exponential, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "Logistics" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qlogis(input$probC, location = input$location_logistic, scale = input$scale_logistic, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qlogis(input$probC, location = input$location_logistic, scale = input$scale_logistic, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    }
    # else if(input$continuas == "Log-Normal" && input$tipoC== "Probabilities") {
    # if (input$Continuasx == "lower.tail") {
    # x <- qlnorm(input$probC,meanlog = input$mean_lognormal, sdlog = input$, lower.tail = TRUE)
    # return(paste("The value of x is:", round(x,4)))}
    # else {
    # x <- qlnorm(input$probC,meanlog = input$mean_lognormal, sdlog = input$, lower.tail = FALSE)
    # return(paste("The value of x is:", round(x,4)))
    # }
    # }
    else if (input$continuas == "Gamma" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qgamma(input$probC, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qgamma(input$probC, shape = input$alpha_gamma, rate = input$beta_gamma, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "Weibull" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qweibull(input$probC, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qweibull(input$probC, shape = input$alpha_weibull, scale = input$beta_weibull, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    } else if (input$continuas == "Beta" && input$tipoC == "Probabilities") {
      if (input$Continuasx == "lower.tail") {
        x <- qbeta(input$probC, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = TRUE)
        return(paste("The value of x is:", round(x, 4)))
      } else {
        x <- qbeta(input$probC, shape1 = input$alpha_beta, shape2 = input$beta_beta, lower.tail = FALSE)
        return(paste("The value of x is:", round(x, 4)))
      }
    }
  })


  #################### Fin modulo probabilidad ##################################
  #--------------# Upload data #---------------#
  observe({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      filePath <<- inFile$datapath
      ext <- tools::file_ext(inFile$name)
      if (tolower(ext) %in% c("xls", "xlsx")) {
        # Es Excel: muestra selección de hoja
        selectionWorksheet <- sort(unique(readxl::excel_sheets(inFile$datapath)))
        updateSelectInput(session, "worksheet", choices = selectionWorksheet)
        shinyjs::show("worksheet") # si usas shinyjs para mostrar/ocultar
      } else if (tolower(ext) == "csv") {
        # Es CSV: oculta selección de hoja
        shinyjs::hide("worksheet")
      }
    }
  })

  observe({
    if (is.null(input$file1)) {
      shinyjs::disable("getData")
    } else if (input$example == TRUE) {
      shinyjs::disable("getData")
    } else {
      shinyjs::enable("getData")
    }
  })

  observeEvent(input$getData, {
    ext <- tools::file_ext(input$file1$name)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      dat <- readxl::read_excel(filePath, sheet = input$worksheet)
    } else if (tolower(ext) == "csv") {
      dat <- read.csv(filePath, stringsAsFactors = TRUE)
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    values$dat <- as.data.frame(unclass(dat), stringsAsFactors = TRUE)
    values$dat
  })
  observe({
    if (input$example == FALSE) {
      return()
    } else if (input$example == TRUE & input$selexample == "iris") {
      values$dat <- as.data.frame(datasets::iris, stringsAsFactors = FALSE)
      values$dat
    } else if (input$example == TRUE & input$selexample == "fishes") {
      dataexample <- system.file(
        "extdata", "fishes.csv",
        package = "StatisticTeach1"
      )
      values$dat <- read.csv(
        dataexample,
        stringsAsFactors = TRUE
      )
      values$dat
    }
  })


  # --- Selección de tipos de columna y separador decimal ---
  # Genera la UI dinámica para seleccionar tipo y separador de columnas
  # Función para limpiar nombres para inputId válidos en JS
  clean_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

  output$type_selector <- renderUI({
    req(values$dat)
    cols <- names(values$dat)
    tipos_actuales <- sapply(values$dat, function(x) {
      if (is.factor(x)) {
        return("Factor")
      } else if (is.numeric(x)) {
        return("Numeric")
      } else {
        return("String")
      }
    })
    tagList(
      lapply(seq_along(cols), function(i) {
        cid <- clean_id(cols[i])
        fluidRow(
          column(12, selectInput(
            inputId = paste0("tipo_col_", cid),
            label = cols[i],
            choices = c("Numeric", "Factor", "String", "Date", "DateTime"),
            selected = tipos_actuales[i]
          )),
          column(12, conditionalPanel(
            condition = sprintf("input.tipo_col_%s == 'Numeric'", cid),
            selectInput(
              inputId = paste0("sep_col_", cid),
              label = paste0("Decimal separator (", cols[i], "):"),
              choices = c("Dot" = ".", "Comma" = ","),
              selected = "."
            )
          )),
          column(12, conditionalPanel(
            condition = sprintf("input.tipo_col_%s == 'Date' || input.tipo_col_%s == 'DateTime'", cid, cid),
            textInput(
              inputId = paste0("fmt_col_", cid),
              label = paste0("Formato de fecha/hora (", cols[i], "):"),
              value = "%Y-%m-%d"
            )
          ))
        )
      })
    )
  })

  # Aplica la transformación de tipos y separador decimal
  observeEvent(input$apply_types, {
    req(values$dat)
    dat <- values$dat
    cols <- names(dat)
    for (col in cols) {
      cid <- clean_id(col)
      tipo <- input[[paste0("tipo_col_", cid)]]
      if (is.null(tipo)) next
      if (tipo == "Factor") {
        dat[[col]] <- as.factor(dat[[col]])
      } else if (tipo == "Numeric") {
        sep <- input[[paste0("sep_col_", cid)]]
        if (is.null(sep)) sep <- "."
        # If separator is comma, replace ',' with '.' before converting to numeric
        if (sep == ",") {
          dat[[col]] <- as.numeric(gsub(",", ".", as.character(dat[[col]])))
        } else {
          dat[[col]] <- as.numeric(as.character(dat[[col]]))
        }
      } else if (tipo == "Date") {
        fmt <- input[[paste0("fmt_col_", cid)]]
        if (is.null(fmt) || fmt == "") fmt <- "%Y-%m-%d"
        dat[[col]] <- as.Date(as.character(dat[[col]]), format = fmt)
      } else if (tipo == "DateTime") {
        fmt <- input[[paste0("fmt_col_", cid)]]
        if (is.null(fmt) || fmt == "") fmt <- "%Y-%m-%d %H:%M:%S"
        dat[[col]] <- as.POSIXct(as.character(dat[[col]]), format = fmt)
      } else if (tipo == "String") {
        dat[[col]] <- as.character(dat[[col]])
      }
    }
    values$dat <- dat
    showNotification("Column types updated", type = "message")
  })

  output$table1 <- renderTable(
    {
      req(values$dat)

      # Create a clean data summary table
      data_summary <- data.frame(
        Column = names(values$dat),
        Type = sapply(values$dat, function(x) {
          if (is.factor(x)) {
            return("Factor")
          } else if (is.numeric(x)) {
            return("Numeric")
          } else if (is.character(x)) {
            return("Character")
          } else if (is.Date(x)) {
            return("Date")
          } else if (inherits(x, "POSIXt")) {
            return("DateTime")
          } else {
            return(class(x)[1])
          }
        }),
        Missing = sapply(values$dat, function(x) sum(is.na(x))),
        Unique_Values = sapply(values$dat, function(x) length(unique(x))),
        Min = sapply(values$dat, function(x) {
          if (is.numeric(x)) {
            round(min(x, na.rm = TRUE), 3)
          } else if (is.factor(x) || is.character(x)) {
            paste0("'", as.character(x[!is.na(x)][1]), "'")
          } else {
            "N/A"
          }
        }),
        Max = sapply(values$dat, function(x) {
          if (is.numeric(x)) {
            round(max(x, na.rm = TRUE), 3)
          } else if (is.factor(x) || is.character(x)) {
            paste0("'", as.character(x[!is.na(x)][length(x[!is.na(x)])]), "'")
          } else {
            "N/A"
          }
        }),
        stringsAsFactors = FALSE
      )

      # Rename columns for better display
      names(data_summary) <- c("Column", "Data Type", "Missing", "Unique", "Min/First", "Max/Last")

      return(data_summary)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%",
    align = "c",
    rownames = FALSE,
    digits = 3,
    sanitize.text.function = function(x) {
      # Force black color on all text
      gsub("<", "&lt;", gsub(">", "&gt;", x))
    }
  )



  ########################## Fin load data#######################################
  ############## Descriptivos univariantes ######################################
  ############## Descriptivos ######################################
  observe({
    req(values$dat)
    updateSelectizeInput(
      session,
      "selvarcuanti",
      "Variables Quantitative",
      choices = values$dat %>% dplyr::select_if(is.numeric) %>% names(),
    )
    updateSelectizeInput(
      session,
      "selgrupo",
      "group by",
      choices = values$dat %>% dplyr::select_if(is.factor) %>% names(),
      # multiple = TRUE,
      options = list(maxItems = 2),

    )
  })
  #--------- descriptivos númericos
  output$central <- DT::renderDT({
    req(input$selvarcuanti)
    if (input$Grupo == 0) {
      if (!is.null(input$selvarcuanti)) {
        df <- data.frame(values$dat)
        dtvos <- df %>%
          stats::na.omit() %>%
          dplyr::select(input$selvarcuanti) %>%
          summarise(
            across(
              everything(),
              list(
                mean = mean,
                mean.trimmed5p = ~ mean(., trim = 0.05),
                # Winsor.mean =~ DescTools::Winsorize(.,probs = c(0.05, 0.95)),
                geometric.mean = geometric.mean,
                median = median,
                mode = ~ DescTools::Mode(., na.rm = TRUE)
              )
            )
          )
      }

      DT::datatable(
        data = t(dtvos), extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          columnDefs = list(list(targets = 1, title = "Value")),
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c( #' pageLength',
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 14
        )
      ) %>%
        DT::formatRound(columns = c(1:1), digits = 4)
    } else if (input$Grupo == 1) {
      dtvos <- values$dat %>%
        stats::na.omit() %>%
        dplyr::group_by(!!!rlang::syms(input$selgrupo)) %>%
        summarise(across(input$selvarcuanti, list(
          mean = mean,
          mean.trimmed5p = ~ mean(., trim = 0.05),
          # Winsor.mean =~ DescTools::Winsorize(.,probs = c(0.05, 0.95)),
          geometric.mean = geometric.mean,
          median = median, mode = ~ DescTools::Mode(., na.rm = TRUE)[1]
        )), .groups = "drop")

      DT::datatable(
        data = dtvos, extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c(
            "pageLength",
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      ) %>%
        DT::formatRound(columns = c(3:5), digits = 4)
    }
  })

  output$variability <- DT::renderDT({
    req(input$selvarcuanti)
    if (input$Grupo == 0) {
      if (!is.null(input$selvarcuanti)) {
        df <- data.frame(values$dat)
        dtvos <- df %>%
          stats::na.omit() %>%
          dplyr::select(input$selvarcuanti) %>%
          summarise(
            across(
              everything(),
              list(
                min = min, max = max, sd = sd, var = var, mad = mad, IQR = IQR,
                cv = ~ sd(.) / mean(.) * 100,
                Std.error = ~ sd(.) / sqrt(length(.))
              )
            )
          )
      }

      DT::datatable(
        data = t(dtvos), extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          columnDefs = list(list(targets = 1, title = "Value")),
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c( #' pageLength',
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      ) %>%
        DT::formatRound(columns = c(1:1), digits = 4)
    } else if (input$Grupo == 1) {
      dtvos <- values$dat %>%
        stats::na.omit() %>%
        dplyr::group_by(!!!rlang::syms(input$selgrupo)) %>%
        summarise(
          across(
            input$selvarcuanti,
            list(
              min = min, max = max, sd = sd, var = var, mad = mad, IQR = IQR,
              cv = ~ sd(.) / mean(.) * 100,
              Std.error = ~ sd(.) / sqrt(length(.))
            )
          )
        )

      DT::datatable(
        data = dtvos,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c(
            "pageLength",
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      ) %>%
        DT::formatRound(columns = c(3:9), digits = 4)
    }
  })

  output$quart <- DT::renderDT({
    req(input$selvarcuanti, input$nquartiles)
    if (input$Grupo == 0) {
      if (!is.null(input$selvarcuanti)) {
        df <- data.frame(values$dat)

        # Generate dynamic quantile probabilities based on user input
        n_quartiles <- input$nquartiles
        probs <- seq(1, n_quartiles, 1) / (n_quartiles + 1)

        # Create dynamic list of quantile functions using local() to fix closure
        quartile_functions <- list()
        for (i in 1:n_quartiles) {
          quartile_functions[[paste0("Q", i)]] <- local({
            prob_val <- probs[i]
            function(x) stats::quantile(x, prob_val)
          })
        }

        dtvos <- df %>%
          stats::na.omit() %>%
          dplyr::select(input$selvarcuanti) %>%
          summarise(
            across(
              everything(),
              quartile_functions
            )
          )

        DT::datatable(
          data = t(dtvos), extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            columnDefs = list(list(targets = 1, title = "Value")),
            scrollX = TRUE,
            fixedColumns = TRUE,
            buttons = c( #' pageLength',
              "copy",
              "csv",
              "excel",
              "pdf",
              "print"
            ),
            pagelength = 14
          )
        ) %>%
          DT::formatRound(columns = c(1:1), digits = 4)
      }
    } else if (input$Grupo == 1) {
      # Generate dynamic quantile probabilities based on user input
      n_quartiles <- input$nquartiles
      probs <- seq(1, n_quartiles, 1) / (n_quartiles + 1)

      # Create dynamic list of quantile functions for grouped data using local()
      quartile_functions <- list()
      for (i in 1:n_quartiles) {
        quartile_functions[[paste0("Q", i)]] <- local({
          prob_val <- probs[i]
          function(x) stats::quantile(x, prob_val) %>% round(4)
        })
      }

      Tabla <- values$dat %>%
        stats::na.omit() %>%
        dplyr::group_by(!!!rlang::syms(input$selgrupo)) %>%
        summarise(across(input$selvarcuanti, quartile_functions))

      DT::datatable(
        data = Tabla,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c(
            "pageLength",
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      )
    }
  })

  output$perc <- renderPrint({
    req(input$selvarcuanti)
    if (input$Grupo == 0 && input$relpos == "Percentiles") {
      df <- data.frame(values$dat)
      dtvos <- df %>%
        stats::na.omit() %>%
        dplyr::select(input$selvarcuanti) %>%
        summarise(
          across(
            everything(),
            list(
              percentile = ~ stats::quantile(., probs = input$npercent / 100)
            )
          )
        )

      paste("The", input$npercent, "percentile is:", dtvos, collapse = " ")
    }
  })

  output$perct <- DT::renderDT({
    req(input$npercent)
    if (input$Grupo == 1 && input$relpos == "Percentiles") {
      Tabla <- values$dat %>%
        stats::na.omit() %>%
        dplyr::group_by(!!!rlang::syms(input$selgrupo)) %>%
        summarise(across(
          input$selvarcuanti,
          list(
            percentile = ~ stats::quantile(., probs = input$npercent / 100)
          )
        ))

      DT::datatable(
        data = Tabla,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c(
            "pageLength",
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      )
    }
  })

  output$shape <- DT::renderDT({
    req(input$selvarcuanti)
    if (input$Grupo == 0) {
      if (!is.null(input$selvarcuanti)) {
        df <- data.frame(values$dat)
        dtvos <- df %>%
          stats::na.omit() %>%
          dplyr::select(input$selvarcuanti) %>%
          summarise(
            across(
              everything(),
              list(
                Skewness = ~ DescTools::Skew(., na.rm = TRUE),
                Pearson_Skewness = ~ 3 * (mean(., na.rm = TRUE) - median(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                Kurtosis = ~ DescTools::Kurt(., method = "fisher", na.rm = TRUE)
              )
            )
          )

        DT::datatable(
          data = t(dtvos), extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            columnDefs = list(list(targets = 1, title = "Value")),
            scrollX = TRUE,
            fixedColumns = TRUE,
            buttons = c( #' pageLength',
              "copy",
              "csv",
              "excel",
              "pdf",
              "print"
            ),
            pagelength = 14
          )
        ) %>%
          DT::formatRound(columns = c(1:1), digits = 4)
      }
    } else if (input$Grupo == 1) {
      Tabla <- values$dat %>%
        stats::na.omit() %>%
        dplyr::group_by(!!!rlang::syms(input$selgrupo)) %>%
        summarise(across(
          input$selvarcuanti,
          list(
            Skewness = ~ DescTools::Skew(., na.rm = TRUE),
            Pearson_Skewness = ~ 3 * (mean(., na.rm = TRUE) - median(., na.rm = TRUE)) / sd(., na.rm = TRUE),
            Kurtosis = ~ DescTools::Kurt(., method = "fisher", na.rm = TRUE)
          )
        ))

      DT::datatable(
        data = Tabla,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c(
            "pageLength",
            "copy",
            "csv",
            "excel",
            "pdf",
            "print"
          ),
          pagelength = 8,
          lengthMenu = list(
            c(8, 25, 100, -1),
            c("8", "25", "100", "All")
          )
        )
      )
    }
  })


  #----------- tablas descriptivas

  observeEvent(input$getTable, {
    req(input$selvarcuanti)
    if (input$Grupo == 1) {
      return(NULL)
    }
    dataT <- subset(values$dat,
      select = input$selvarcuanti,
      drop = TRUE
    )
    dataT <- dataT %>% stats::na.omit()

    if (input$Selectrule == "Sturges") {
      bins <- ceiling(log2(length(dataT))) + 1
    } else if (input$Selectrule == "Sqrt") {
      bins <- sqrt(length(dataT))
    } else if (input$Selectrule == "Freedman–Diaconis") {
      bins <- diff(range(dataT)) / (2 * IQR(dataT) / length(dataT)^(1 / 3))
    } else if (input$Selectrule == "Scott") {
      bw <- 3.5 * sd(dataT) / length(dataT)^(1 / 3)
      bins <- diff(range(dataT)) / bw
    } else if (input$Selectrule == "Number of bin") {
      bins <- input$intervalnum
    }

    values$TF <- ST_freq_numeric(values$dat, input$selvarcuanti, bins = bins)
  })

  output$tablefreq <- DT::renderDT({
    req(values$TF)
    if (input$Grupo == 1) {
      return(NULL)
    }
    DT::datatable(
      data = values$TF,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        pagelength = 20,
        lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        )
      )
    ) %>%
      DT::formatRound(columns = c(1:3, 6, 7), digits = 2)
  })

  #-------------------graficas
 output$plotdescript <- renderPlot({
  req(values$dat)
  if (input$SelecTipografico == "Histogram" & input$Grupo == FALSE) {
    p <- ggplot(values$dat, aes(x = get(input$selvarcuanti))) +
      geom_histogram(
        bins = input$binsHist,
        color = input$linecolorHist,
        fill = input$fillHist,
        alpha = input$alphaHist,
        lwd = input$lwdHist
      ) +
      themesb[[input$themeHist]] +
      ylab(input$ylabHist) +
      xlab(input$xlabHist) +
      theme(text = element_text(size = input$fontHist, family = input$fontH)) +
      geom_text(
        stat = "bin",
        aes(label = stat(count)),
        bins = input$binsHist,
        vjust = if (isTRUE(input$horizontal_plot)) 0.5 else -0.7,
        hjust = if (isTRUE(input$horizontal_plot)) -0.2 else 0.5
      )

    if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabHist, y = input$ylabHist)
    }

    p
  }
})
output$plotdescript2 <- renderPlot({
  req(values$dat)
  if (input$SelecTipografico == "Histogram" & input$Grupo == TRUE) {
    p <- ggplot(values$dat, aes(
      x = (!!rlang::sym(input$selvarcuanti)),
      fill = (!!rlang::sym(input$selgrupo))
    )) +
      geom_histogram(
        color = input$linecolorHist2,
        bins = input$binsHist2,
        alpha = input$alphaHist2,
        lwd = input$lwdHist2,
        position = if (isTRUE(input$horizontal_plot)) "dodge2" else "stack"
      ) +
      themesb[[input$themeHist2]] +
      ylab(input$ylabHist2) +
      xlab(input$xlabHist2) +
      theme(text = element_text(size = input$fontHist2, family = input$fontH2)) +
      scale_fill_brewer(palette = input$paletas)

    if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabHist2, y = input$ylabHist2)
    }

    p
  }
})

  output$plotdescript3 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Density" & input$Grupo == FALSE) {
      p <- ggplot(values$dat, aes(x = get(input$selvarcuanti))) +
        geom_density(
          color = input$linecolorDensy, fill = input$fillDensy,
          alpha = input$alphaDensy, lwd = input$lwdDensy
        ) +
        themesb[[input$themeDensy]] +
        ylab(input$ylabDensy) +
        xlab(input$xlabDensy) +
        theme(text = element_text(size = input$fontDensy, family = input$fontdensy))
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabDensy, y = input$ylabDensy)
    }
    p
    }
  })
  output$plotdescript4 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Density" & input$Grupo == TRUE) {
      p <-ggplot(values$dat, aes(x = (!!rlang::sym(input$selvarcuanti)), fill = (!!rlang::sym(input$selgrupo)))) +
        geom_density(
          color = input$linecolorDensy2,
          alpha = input$alphaDensy2, lwd = input$lwdDensy2
        ) +
        themesb[[input$themeDensy2]] +
        ylab(input$ylabDensy2) +
        xlab(input$xlabDensy2) +
        theme(text = element_text(size = input$fontDensy2, family = input$fontdensy2)) +
        scale_fill_brewer(palette = input$paletasDensy2)
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabDensy2, y = input$ylabDensy2)
    }
    p
    }
  })

  output$plotdescript5 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Boxplot" & input$Grupo == FALSE) {
      p <-ggplot(values$dat, aes(y = get(input$selvarcuanti))) +
        geom_boxplot(
          notch = input$nocht, outlier.colour = input$coloroutlier, color = input$linecolorboxplot,
          fill = input$fillboxplot,
          alpha = input$alphaboxplot,
          lwd = input$lwdboxplot,
          outlier.shape = input$pchboxplot,
          outlier.size = input$pointboxplot
        ) +
        themesb[[input$themeboxplot]] +
        ylab(input$ylabboxplot) +
        xlab(input$xlabboxplot) +
        theme(text = element_text(size = input$fontboxplot, family = input$fontbox))
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabboxplot, y = input$ylabboxplot)
    }
    p
    }
  })


  output$graph <- renderPlot({
    if (input$SelecTipografico == "Boxplot" & input$Grupo == 1) {
      p <-ggplot(values$dat, aes(
        y = (!!rlang::sym(input$selvarcuanti)),
        x = (!!rlang::sym(input$selgrupo)),
        fill = (!!rlang::sym(input$selgrupo))
      )) +
        geom_boxplot(
          notch = input$nocht2, outlier.colour = input$coloroutlier2, color = input$linecolorboxplot2,
          alpha = input$alphaboxplot2,
          lwd = input$lwdboxplot2,
          outlier.shape = input$pchboxplot2,
          outlier.size = input$pointboxplot2
        ) +
        themesb[[input$themeboxplot2]] +
        ylab(input$ylabboxplot2) +
        xlab(input$xlabboxplot2) +
        theme(text = element_text(size = input$fontboxplot2, family = input$fontbox2)) +
        scale_fill_brewer(palette = input$paletasboxplot2)
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabboxplot2, y = input$ylabboxplot2)
    }
    p
    }
  })


# --- Helper de límites IQR reutilizable ---
get_limits <- function(x) {
  qs  <- unname(stats::quantile(x, c(0.25, 0.75), na.rm = TRUE))
  iqr <- qs[2] - qs[1]
  c(low = qs[1] - 1.5 * iqr, up = qs[2] + 1.5 * iqr)
}

# --- 1) Función: outliernogroup(data, var) ---
find_outliers_no_group <- function(data, var) {
  validate(need(var %in% names(data), "Variable no existe en los datos."))
  validate(need(is.numeric(data[[var]]), "La variable debe ser numérica."))

  lims <- get_limits(data[[var]])
  low  <- unname(lims["low"])
  up   <- unname(lims["up"])

  Dt <- data %>%
    dplyr::mutate(
      .val  = .data[[var]],
      .low  = low,
      .up   = up,
      .is_o = !is.na(.val) & (.val < .low | .val > .up)
    ) %>%
    dplyr::filter(.is_o) %>%
    dplyr::select(.val, lowlim = .low, uplim = .up)

  names(Dt)[names(Dt) == ".val"] <- var
  Dt
}

# ---- Server ----
output$outlierNoGroup <- DT::renderDT({
  req(values$dat, input$selvarcuanti, input$SelecTipografico)
  validate(need(input$SelecTipografico == "Boxplot", "Selecciona 'Boxplot'."))

  df  <- as.data.frame(values$dat)
  var <- input$selvarcuanti[[1]]



  Dt <- find_outliers_no_group(df, var)

  validate(need(nrow(Dt) > 0, "No se encontraron atípicos con el criterio IQR."))

  DT::datatable(
    Dt,
    options = list(
      scrollX    = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ),
    rownames = FALSE
  )
})

# --- 2) Output: output$outlier (DT::renderDT) ---
output$outlier <- DT::renderDT({
  req(values$dat, input$selvarcuanti, input$SelecTipografico)
  validate(need(input$SelecTipografico == "Boxplot", "Selecciona 'Boxplot'."))

  df  <- as.data.frame(values$dat)
  var <- input$selvarcuanti[[1]]
  validate(need(var %in% names(df), "Variable no existe en los datos."))
  validate(need(is.numeric(df[[var]]), "La variable debe ser numérica."))

  # Detectar si hay grupo (robusto a tipos)
  grp_on <- {
    v <- input$Grupo
    if (is.null(v)) FALSE
    else if (is.logical(v)) isTRUE(v)
    else if (is.numeric(v)) v != 0
    else tolower(as.character(v)) %in% c("1","true","si","sí","on")
  }

  if (grp_on) {
    grp_id <- if (!is.null(input$selgrupocat)) input$selgrupocat else input$selgrupo
    validate(need(length(grp_id) > 0, "Selecciona al menos una columna de grupo."))
    validate(need(all(grp_id %in% names(df)), "Alguna columna de grupo no existe en los datos."))



    Dt <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_id))) %>%
      dplyr::mutate(
        .val  = .data[[var]],
        .low  = get_limits(.val)["low"],
        .up   = get_limits(.val)["up"],
        .is_o = !is.na(.val) & (.val < .low | .val > .up)
      ) %>%
      dplyr::filter(.is_o) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(grp_id), .val, lowlim = .low, uplim = .up)

    names(Dt)[names(Dt) == ".val"] <- var
  } else {
    Dt <- outliernogroup(df, var)
  }

  # Si prefieres tabla vacía sin mensaje, elimina este validate
  validate(need(nrow(Dt) > 0, "No se encontraron atípicos con el criterio IQR."))

  DT::datatable(
    Dt,
    options = list(
      scrollX    = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ),
    rownames = FALSE
  )
})


  output$data_brush <- DT::renderDT({
    n <- nrow(brushedPoints(values$dat, brush = input$plot_brush)) # row count will be 0 when no selection made by the brush
    if (n == 0) {
      return()
    } else {
      df <- brushedPoints(values$dat, brush = input$plot_brush)
    }
    DT::datatable(
      data = df, extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        pagelength = 20,
        lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        )
      )
    )
  })

  output$plotdescript6 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Violin" & input$Grupo == FALSE) {
      p <-ggplot(values$dat, aes(y = 1, x = get(input$selvarcuanti))) +
        geom_violin(
          lwd = input$lwdviolin, alpha = input$alphaviolin,
          trim = input$trim, color = input$linecolorviolin,
          fill = input$fillviolin
        ) +
        themesb[[input$themeviolin]] +
        ylab(input$ylabviolin) +
        xlab(input$xlabviolin) +
        theme(text = element_text(size = input$fontSviolin, family = input$fontviolin))
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabviolin, y = input$ylabviolin)
    }
    p
    }
  })

  output$plotdescript7 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Violin" & input$Grupo == TRUE) {
      p <-ggplot(values$dat, aes(
        y = (!!rlang::sym(input$selvarcuanti)),
        x = (!!rlang::sym(input$selgrupo)),
        fill = (!!rlang::sym(input$selgrupo))
      )) +
        geom_violin(
          lwd = input$lwdviolin2, alpha = input$alphaviolin2,
          trim = input$trim2, color = input$linecolorviolin2
        ) +
        themesb[[input$themeviolin2]] +
        ylab(input$ylabviolin2) +
        xlab(input$xlabviolin2) +
        theme(text = element_text(size = input$fontSviolin2, family = input$fontviolin2)) +
        scale_fill_brewer(palette = input$paletasviolin)
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabviolin2, y = input$ylabviolin2)
    }
    p
    }
  })

  # =====================================================================
  # POLYGON PLOTS
  # =====================================================================

  output$plotdescript8 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Polygon" & input$Grupo == FALSE) {
      # Validate inputs exist
      req(
        input$binsPolygon, input$linecolorPolygon, input$lwdPolygon,
        input$pointcolorPolygon, input$pointsizePolygon, input$themePolygon,
        input$ylabPolygon, input$xlabPolygon, input$fontPolygon
      )

      # Create histogram data for polygon
      hist_data <- hist(values$dat[[input$selvarcuanti]],
        breaks = input$binsPolygon,
        plot = FALSE
      )

      # Create data frame for polygon
      polygon_data <- data.frame(
        x = hist_data$mids,
        y = hist_data$counts
      )

      p <-ggplot(polygon_data, aes(x = x, y = y)) +
        geom_line(
          color = input$linecolorPolygon,
          size = input$lwdPolygon
        ) +
        geom_point(
          color = input$pointcolorPolygon,
          size = input$pointsizePolygon
        ) +
        themesb[[input$themePolygon]] +
        ylab(input$ylabPolygon) +
        xlab(input$xlabPolygon) +
        theme(text = element_text(
          size = input$fontSizePolygon,
          family = input$fontPolygon
        ))
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabPolygon, y = input$ylabPolygon)
    }
    p
    }
  })

  output$plotdescript9 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Polygon" & input$Grupo == TRUE) {
      # Validate inputs exist
      req(
        input$binsPolygon2, input$lwdPolygon2, input$pointsizePolygon2,
        input$themePolygon2, input$ylabPolygon2, input$xlabPolygon2,
        input$fontPolygon2, input$paletasPolygon
      )

      # Create histogram data for each group
      group_data <- split(
        values$dat[[input$selvarcuanti]],
        values$dat[[input$selgrupo]]
      )

      # Create polygon data for each group
      polygon_list <- list()
      for (group_name in names(group_data)) {
        hist_data <- hist(group_data[[group_name]],
          breaks = input$binsPolygon2,
          plot = FALSE
        )
        polygon_list[[group_name]] <- data.frame(
          x = hist_data$mids,
          y = hist_data$counts,
          group = group_name
        )
      }

      polygon_data <- do.call(rbind, polygon_list)

      p <-ggplot(polygon_data, aes(x = x, y = y, color = group)) +
        geom_line(size = input$lwdPolygon2) +
        geom_point(size = input$pointsizePolygon2) +
        themesb[[input$themePolygon2]] +
        ylab(input$ylabPolygon2) +
        xlab(input$xlabPolygon2) +
        theme(text = element_text(
          size = input$fontSizePolygon2,
          family = input$fontPolygon2
        )) +
        scale_color_brewer(palette = input$paletasPolygon)
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabPolygon2, y = input$ylabPolygon2)
    }
    p
    }
  })

  # =====================================================================
  # OGIVE PLOTS
  # =====================================================================

  output$plotdescript10 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Ogive" & input$Grupo == FALSE) {
      # Validate inputs exist
      req(
        input$binsOgive, input$linecolorOgive, input$lwdOgive,
        input$pointcolorOgive, input$pointsizeOgive, input$themeOgive,
        input$ylabOgive, input$xlabOgive, input$fontOgive
      )

      # Create histogram data for ogive
      hist_data <- hist(values$dat[[input$selvarcuanti]],
        breaks = input$binsOgive,
        plot = FALSE
      )

      # Calculate cumulative frequencies
      cumulative_freq <- cumsum(hist_data$counts)

      # Create data frame for ogive
      ogive_data <- data.frame(
        x = hist_data$breaks[-1], # Use upper class boundaries
        y = cumulative_freq
      )

      p <-ggplot(ogive_data, aes(x = x, y = y)) +
        geom_line(
          color = input$linecolorOgive,
          size = input$lwdOgive
        ) +
        geom_point(
          color = input$pointcolorOgive,
          size = input$pointsizeOgive
        ) +
        themesb[[input$themeOgive]] +
        ylab(input$ylabOgive) +
        xlab(input$xlabOgive) +
        theme(text = element_text(
          size = input$fontSizeOgive,
          family = input$fontOgive
        ))
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabOgive, y = input$ylabOgive)
    }
    p
    }
  })

  output$plotdescript11 <- renderPlot({
    req(values$dat)
    if (input$SelecTipografico == "Ogive" & input$Grupo == TRUE) {
      # Validate inputs exist
      req(
        input$binsOgive2, input$lwdOgive2, input$pointsizeOgive2,
        input$themeOgive2, input$ylabOgive2, input$xlabOgive2,
        input$fontOgive2, input$paletasOgive
      )

      # Create histogram data for each group
      group_data <- split(
        values$dat[[input$selvarcuanti]],
        values$dat[[input$selgrupo]]
      )

      # Create ogive data for each group
      ogive_list <- list()
      for (group_name in names(group_data)) {
        hist_data <- hist(group_data[[group_name]],
          breaks = input$binsOgive2,
          plot = FALSE
        )

        # Calculate cumulative frequencies
        cumulative_freq <- cumsum(hist_data$counts)

        ogive_list[[group_name]] <- data.frame(
          x = hist_data$breaks[-1], # Use upper class boundaries
          y = cumulative_freq,
          group = group_name
        )
      }

      ogive_data <- do.call(rbind, ogive_list)

      p <-ggplot(ogive_data, aes(x = x, y = y, color = group)) +
        geom_line(size = input$lwdOgive2) +
        geom_point(size = input$pointsizeOgive2) +
        themesb[[input$themeOgive2]] +
        ylab(input$ylabOgive2) +
        xlab(input$xlabOgive2) +
        theme(text = element_text(
          size = input$fontSizeOgive2,
          family = input$fontOgive2
        )) +
        scale_color_brewer(palette = input$paletasOgive)
        if (isTRUE(input$horizontal_plot)) {
      p <- p +
        coord_flip() +
        labs(x = input$xlabOgive2, y = input$ylabOgive2)
    }
    p
    }
  })

  ### -----------Variables cualitativas

  observe({
    req(values$dat)

    updateSelectizeInput(
      session,
      "selvarcuali",
      "Qualitative variables",
      choices = values$dat %>% dplyr::select_if(is.factor) %>% names(),

    )
    updateSelectizeInput(
      session,
      "selgrupoQuali",
      "Group by",
      choices = values$dat %>% dplyr::select_if(is.factor) %>% names(),

    )
  })





status_text <- reactiveVal("Selecciona dos variables.")


  # Helper para clasificar tipo (enfocado en numérica vs factor)
  tipo_var <- function(x){
    if (is.numeric(x)) return("numérica")
    if (is.factor(x))  return("factor")
    # Si quieres tratar character como categórica, descomenta la siguiente línea:
    if (is.character(x)) return("categórica (character)")
    # Otros tipos (fecha, lógico, etc.)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) return("fecha/tiempo")
    if (is.logical(x)) return("lógica")
    if (is.character(x)) return("caracter")
    return(paste(class(x), collapse = "/"))
  }

  # Observa cambios en selvar1, selvar2 o en los datos y actualiza el texto
  observeEvent(list(input$selvar1, input$selvar2, values$dat), {
    req(values$dat, input$selvar1, input$selvar2)

    df <- as.data.frame(values$dat)
    v1 <- input$selvar1
    v2 <- input$selvar2

    # Verifica que existan las columnas
    if (!(v1 %in% names(df)) || !(v2 %in% names(df))) {
      status_text("Selecciona variables válidas.")
      return()
    }

    t1 <- tipo_var(df[[v1]])
    t2 <- tipo_var(df[[v2]])

    # Mensajes compactos priorizando numérica vs factor
    msg <- if (t1 == "numérica" && t2 == "numérica") {
      sprintf("Ambas son numéricas (%s y %s).", v1, v2)
    } else if (t1 == "factor" && t2 == "factor") {
      sprintf("Ambas son factores/categóricas (%s y %s).", v1, v2)
    } else if (t1 == "numérica" && t2 == "factor") {
      sprintf("%s es numérica y %s es factor.", v1, v2)
    } else if (t1 == "factor" && t2 == "numérica") {
      sprintf("%s es factor y %s es numérica.", v1, v2)
    } else {
      # Caso general si caen en otros tipos (character, fecha, lógico, etc.)
      sprintf("%s es %s y %s es %s.", v1, t1, v2, t2)
    }

    status_text(msg)
  }, ignoreInit = FALSE)

  # Renderiza el texto en la UI
  output$tipo_vars <- renderText(status_text())



  output$TfreqQuali <- DT::renderDT({
    req(values$dat)
    TabQuali <- ST_freq_factor(values$dat, input$selvarcuali)
    DT::datatable(
      data = TabQuali, extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        pagelength = 20,
        lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        )
      )
    )
  })


output$TwofreqQuali <- DT::renderDT({
  req(values$dat, input$selvarcuali, input$selgrupoQuali)

  df <- as.data.frame(values$dat)
  x <- input$selvarcuali
  g <- input$selgrupoQuali

  tab <- table(df[[x]], df[[g]], useNA = "no")
  if (length(tab) == 0) {
    return(DT::datatable(data.frame(Mensaje = "Sin datos"),
                         options = list(dom = "t"), rownames = FALSE))
  }

  # % por fila (row %) y por columna (col %)
  rp <- suppressWarnings(prop.table(tab, 1)); rp[is.nan(rp)] <- 0
  cp <- suppressWarnings(prop.table(tab, 2)); cp[is.nan(cp)] <- 0

  # Celda: n + row% + col%
  to_cell <- function(i, j) {
    sprintf("%d\nrow: %.1f%%\ncol: %.1f%%", tab[i, j], 100*rp[i, j], 100*cp[i, j])
  }

  disp <- matrix("", nrow = nrow(tab), ncol = ncol(tab),
                 dimnames = dimnames(tab))
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      disp[i, j] <- to_cell(i, j)
    }
  }

  out <- as.data.frame.matrix(disp, stringsAsFactors = FALSE)

  # Columna Total (conteo + row:100%)
  row_tot <- rowSums(tab)
  out$Total <- sprintf("%d\nrow: 100%%", row_tot)

  # Primera columna: niveles de x
  out <- tibble::rownames_to_column(out, var = x)

  # Fila de Totales (por columna): conteo + col:100% + % del total global
  col_tot <- colSums(tab)
  grand   <- sum(tab)
  bottom <- as.list(c(
    setNames("Total", x),
    setNames(
      sprintf("%d\ncol: 100%%\nof total: %.1f%%", col_tot, 100*col_tot/grand),
      names(col_tot)
    ),
    Total = sprintf("%d\nof total: 100%%", grand)
  ))
  out <- dplyr::bind_rows(out, bottom)

  # Render bonito: saltos de línea y centrado
  out[] <- lapply(out, function(z) gsub("\n", "<br>", z, fixed = TRUE))

  DT::datatable(
    out,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      dom = "Bfrtip", paging = FALSE, ordering = FALSE, scrollX = TRUE,   buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),   pagelength = 20,   lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        ),
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )






  )



})



output$plotQu <- renderPlot({
  req(values$dat)
  df <- data.frame(values$dat)
  DF <- df %>% stats::na.omit()
  if (input$SelecQu == "Barplot" & input$GrupoQuali == FALSE) {
    p <- ggplot(DF, aes(x = (!!!rlang::sym(input$selvarcuali)))) +
      geom_bar(
        fill = input$fillQu,
        alpha = input$alphaQu,
        lwd = input$lwdQu,
        color = input$linecolorQu
      ) +
      geom_text(
        aes(
          label = paste0(
            after_stat(count),
            "\n(",
            round(after_stat(count)/sum(after_stat(count))*100, 1),
            "%)"
          )
        ),
        stat = "count",
        vjust = -0.3,
        size = input$fontqu/3,
        lineheight = 0.8
      ) +
      themesb[[input$themeQu]] +
      ylab(input$ylabQu) +
      xlab(input$xlabQu) +
      theme(text = element_text(size = input$fontqu, family = input$fontQu)) +
      expand_limits(y = max(table(DF[[input$selvarcuali]])) * 1.2)

if (input$horizontal_plot_qualitative) {
       p <- p + coord_flip() + labs(x = input$xlabQu, y = input$ylabQu)




      }
      p
  }
})

output$plotQu2 <- renderPlot({
  req(values$dat)
  df <- data.frame(values$dat)
  DF <- df %>% stats::na.omit()
  if (input$SelecQu == "Barplot" & input$GrupoQuali == TRUE) {
    p <-ggplot(DF, aes(
      x = (!!!rlang::sym(input$selvarcuali)),
      fill = (!!!rlang::sym(input$selgrupoQuali))
    )) +
      geom_bar(
        alpha = input$alphaQu2,
        lwd = input$lwdQu2,
        color = input$linecolorQu2,
        position = position_dodge()
      ) +
      geom_text(
        aes(
          label = paste0(
            after_stat(count),
            "\n(",
            round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[as.character(after_stat(x))] * 100, 1),
            "%)"
          ),
          group = (!!!rlang::sym(input$selgrupoQuali))
        ),
        stat = "count",
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = input$fontqu2/3,
        lineheight = 0.8
      ) +
      themesb[[input$themeQu2]] +
      ylab(input$ylabQu2) +
      xlab(input$xlabQu2) +
      theme(
        text = element_text(size = input$fontqu2, family = input$fontQu2),
        axis.text.x = element_text(angle = input$angleQu, vjust = 0.6)
      ) +
      scale_fill_brewer(palette = input$paletasQu2) +
      expand_limits(y = max(table(DF[[input$selvarcuali]], DF[[input$selgrupoQuali]])) * 1.3)

      if (input$horizontal_plot_qualitative) {
        p <- p + coord_flip() + labs(x = input$xlabQu2, y = input$ylabQu2)
      }
      p
  }
})

output$pieplot <- renderPlot({
  req(values$dat)
  TabQuali <- ST_freq_factor(values$dat, input$selvarcuali)

  DF <- TabQuali %>%
    dplyr::mutate(
      percent = round(Frequency / sum(Frequency) * 100, 1),
      label   = paste0(Frequency, " (", percent, "%)")
    )

  ggplot(DF, aes(x = 1, y = Frequency, fill = Levels)) +
    geom_bar(stat = "identity", width = 1, alpha = input$alphapie) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    scale_fill_brewer(palette = input$paletaspie) +
    theme(text = element_text(size = input$fontpie, family = input$fontPie)) +
    guides(fill = guide_legend(title = input$labelfill)) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = input$fontpie / 3,
      fontface = "bold"
    )
})

observeEvent(input$resetBarplotQualiNoGrupo, {
  updateSelectInput(session, "themeQu", selected = names(themesb)[1])
  updateSelectInput(session, "fontQu", selected = fuentes[1])
  updateTextInput(session, "ylabQu", value = "Write ylab...")
  updateTextInput(session, "xlabQu", value = "Write xlab...")
  updateColourInput(session, "linecolorQu", value = "red")
  updateTextInput(session, "fillQu", value = "#0095ff")
  updateSliderInput(session, "lwdQu", value = 1.5)
  updateSliderInput(session, "fontqu", value = 12)
  updateSliderInput(session, "alphaQu", value = 0.5)
})

observeEvent(input$resetBarplotQualiGrupo, {
  updateSelectInput(session, "themeQu2", selected = names(themesb)[1])
  updateSelectInput(session, "fontQu2", selected = fuentes[1])
  updateTextInput(session, "ylabQu2", value = "Write ylab...")
  updateTextInput(session, "xlabQu2", value = "Write xlab...")
  updateColourInput(session, "linecolorQu2", value = "red")
  updateSelectInput(session, "paletasQu2", selected = paletas[1])
  updateSliderInput(session, "lwdQu2", value = 1.5)
  updateSliderInput(session, "fontqu2", value = 12)
  updateSliderInput(session, "alphaQu2", value = 0.5)
  updateSliderInput(session, "angleQu", value = 1)
})


observeEvent(input$resetPieplot, {
  updateSelectInput(session, "paletaspie", selected = paletas[1])
  updateSelectInput(session, "fontPie", selected = fuentes[1])
  updateTextInput(session, "labelfill", value = "Write...")
  updateSliderInput(session, "fontpie", value = 12)
  updateSliderInput(session, "alphapie", value = 0.7)
})


  observe({
    req(values$dat)
    updateSelectizeInput(
      session,
      "selvar1",
      "Variable 1 (x)",
      choices = values$dat %>% dplyr::select_if(is.numeric) %>% names(),
      # multiple = TRUE,

    )
    updateSelectizeInput(
      session,
      "selvar2",
      "Variable 2 (y)",
      choices = values$dat %>% dplyr::select_if(is.numeric) %>% names(),
      # multiple = TRUE,
      options = list(maxItems = 1),

    )

    updateSelectizeInput(
      session,
      "selgruposcatter",
      "Variable 2",
      choices = values$dat %>% dplyr::select_if(is.factor) %>% names(),
      options = list(maxItems = 1)

    )
  })

  output$corr <- renderPrint({
    req(values$dat)
    if (input$correlation == "Pearson" & input$Gruposcatter == FALSE) {
      dplyr::summarize(
        values$dat,
        cor((!!!rlang::sym(input$selvar1)),
          (!!!rlang::sym(input$selvar2)),
          use = "pairwise.complete.obs",
          method = "pearson"
        )
      )
    }
    # else if (input$correlation == "Kendall") {
    # dplyr::summarize(values$dat,
    # cor((!!!rlang::sym(input$selvar1)),
    #  (!!!rlang::sym(input$selvar2)),
    # use = "pairwise.complete.obs",
    # method = "kendall"))
    # }
    # else if (input$correlation == "Spearman") {
    # dplyr::summarize(values$dat,
    # cor((!!!rlang::sym(input$selvar1)),
    # (!!!rlang::sym(input$selvar2)),
    # use = "pairwise.complete.obs",
    # method = "spearman"))
    # }
  })



  output$corr2 <- renderPrint({
    req(values$dat)
    if (input$correlation == "Pearson" & input$Gruposcatter == TRUE) {
      df <- values$dat
      df %>%
        na.omit() %>%
        group_by((!!!rlang::sym(input$selgruposcatter))) %>%
        summarize(cor = cor(
          (!!!rlang::sym(input$selvar1)), (!!!rlang::sym(input$selvar2)),
          use = "pairwise.complete.obs",
          method = "pearson"
        ))
    }
  })

  output$scat <- renderPlot({
    req(values$dat)
    if (input$Gruposcatter == FALSE) {
      ggplot(values$dat, aes(
        x = (!!!rlang::sym(input$selvar1)),
        y = (!!!rlang::sym(input$selvar2))
      )) +
        geom_point(
          color = input$colorscatter,
          shape = input$shapescatter,
          alpha = input$alphascatter,
          size = input$sizescatter
        ) +
        themesb[[input$themescatter]] +
        ylab(input$ylabscatter) +
        xlab(input$xlabscatter) +
        theme(
          text = element_text(
            size = input$fontscatter,
            family = input$fontScatter
          )
        )
    }
  })

  output$scat2 <- renderPlot({
    req(values$dat)
    if (input$Gruposcatter == TRUE) {
      ggplot(values$dat, aes(
        x = (!!!rlang::sym(input$selvar1)),
        y = (!!!rlang::sym(input$selvar2)),
        color = (!!!rlang::sym(input$selgruposcatter))
      )) +
        geom_point(
          size = input$sizescatter2,
          alpha = input$alphascatter2,
          shape = input$shapescatter2
        ) +
        scale_colour_brewer(palette = input$paletascatter) +
        themesb[[input$themescatter2]] +
        ylab(input$ylabscatter2) +
        xlab(input$xlabscatter2) +
        theme(
          text = element_text(
            size = input$fontscatter2,
            family = input$fontScatter2
          )
        )
    }
  })


  observe({
    req(values$dat)
    updateSelectizeInput(
      session,
      "selmulti",
      "Variables (select minimum 2)",
      choices = values$dat %>% dplyr::select_if(is.numeric) %>% names(),
      # multiple = TRUE

    )
  })

  output$cormat <- DT::renderDT({
    req(values$dat)
    df <- values$dat %>% dplyr::select(!!!rlang::syms(input$selmulti))

    DT::datatable(
      data = cor(df),
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        pagelength = 20,
        lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        )
      )
    )
  })
  output$covmat <- DT::renderDT({
    req(values$dat)
    df <- values$dat %>% dplyr::select(!!!rlang::syms(input$selmulti))

    DT::datatable(
      data = cov(df),
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE,
        fixedColumns = TRUE,
        buttons = c(
          "pageLength",
          "copy",
          "csv",
          "excel",
          "pdf",
          "print"
        ),
        pagelength = 20,
        lengthMenu = list(
          c(20, 25, 100, -1),
          c("20", "25", "100", "All")
        )
      )
    )
  })

  output$multiv <- renderPrint({
    req(input$selmulti)
    df <- values$dat %>% dplyr::select(!!!rlang::syms(input$selmulti))
    Multiva(as.matrix(df))
  })



  output$ContinuousBivariateScatterDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("scatterplot_grouped_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousBivariateScatterDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("scatterplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )



  output$ContinuousUnivariateHistogramDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("histogram_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariateHistogramDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("histogram_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariateDensityDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("density_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariateDensityDownloadNoGrupo <- downloadHandler(

    # Repite el patrón para los otros botones...
    filename = function() {
      paste("density_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariateBoxplotDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("boxplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )
  output$ContinuousUnivariateBoxplotDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("boxplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )
  output$ContinuousUnivariateViolinDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("violin_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )
  output$ContinuousUnivariateViolinDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("violin_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  # =====================================================================
  # POLYGON DOWNLOAD HANDLERS
  # =====================================================================

  output$ContinuousUnivariatePolygonDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("polygon_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Polygon not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariatePolygonDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("polygon_grouped_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Polygon not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )


  # =====================================================================
  # OGIVE DOWNLOAD HANDLERS
  # =====================================================================

  output$ContinuousUnivariateOgiveDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("ogive_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Ogive not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$ContinuousUnivariateOgiveDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("ogive_grouped_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Ogive not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )

  output$QualitativeBarplotDownloadGrupo <- downloadHandler(
    filename = function() {
      paste("barplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )
  output$QualitativeBarplotDownloadNoGrupo <- downloadHandler(
    filename = function() {
      paste("barplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )
  output$QualitativePieDownload <- downloadHandler(
    filename = function() {
      paste("pie_chart_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot(1:10, main = "Placeholder - Histogram not found")
      ggsave(file, last_plot(), width = 8, height = 6, dpi = 300)
    }
  )


  observeEvent(input$resetHistogram, {
    updateSliderInput(session, "binsHist", value = 10)
    updateSelectInput(session, "themeHist", selected = names(themesb)[1])
    updateSelectInput(session, "fontH", selected = fuentes[1])
    updateTextInput(session, "ylabHist", value = "Write ylab...")
    updateTextInput(session, "xlabHist", value = "Write xlab...")
    updateColourInput(session, "linecolorHist", value = "red")
    updateColourInput(session, "fillHist", value = "blue")
    updateSliderInput(session, "lwdHist", value = 1.5)
    updateSliderInput(session, "fontHist", value = 12)
    updateSliderInput(session, "alphaHist", value = 0.3)
  })

  observeEvent(input$resetHistogram2, {
    updateSliderInput(session, "binsHist2", value = 10)
    updateSelectInput(session, "themeHist2", selected = names(themesb)[1])
    updateSelectInput(session, "fontH2", selected = fuentes[1])
    updateTextInput(session, "ylabHist2", value = "Write ylab...")
    updateTextInput(session, "xlabHist2", value = "Write xlab...")
    updateColourInput(session, "linecolorHist2", value = "red")
    updateColourInput(session, "fillHist2", value = "blue")
    updateSliderInput(session, "lwdHist2", value = 1.5)
    updateSliderInput(session, "fontHist2", value = 12)
    updateSliderInput(session, "alphaHist2", value = 0.3)
    updateSelectInput(session, "paletas", selected = paletas[1])
  })

  observeEvent(input$resetDensity, {
    updateSelectInput(session, "themeDensy", selected = names(themesb)[1])
    updateSelectInput(session, "fontdensy", selected = fuentes[1])
    updateTextInput(session, "ylabDensy", value = "Write ylab...")
    updateTextInput(session, "xlabDensy", value = "Write xlab...")
    updateColourInput(session, "linecolorDensy", value = "blue")
    updateColourInput(session, "fillDensy", value = "blue")
    updateSliderInput(session, "lwdDensy", value = 1.5)
    updateSliderInput(session, "fontDensy", value = 12)
    updateSliderInput(session, "alphaDensy", value = 0.3)
  })

  observeEvent(input$resetDensity2, {
    updateSelectInput(session, "paletasDensy2", selected = paletas[1])
    updateSelectInput(session, "paletas", selected = paletas[2])
    updateSelectInput(session, "themeDensy2", selected = names(themesb)[1])
    updateSelectInput(session, "fontdensy2", selected = fuentes[1])
    updateTextInput(session, "ylabDensy2", value = "Write ylab...")
    updateTextInput(session, "xlabDensy2", value = "Write xlab...")
    updateColourInput(session, "linecolorDensy2", value = "black")
    updateSliderInput(session, "lwdDensy2", value = 1.5)
    updateSliderInput(session, "fontDensy2", value = 12)
    updateSliderInput(session, "alphaDensy2", value = 0.3)
  })

  observeEvent(input$resetBoxplot, {
    updateSelectInput(session, "themeboxplot", selected = names(themesb)[1])
    updateSelectInput(session, "fontbox", selected = fuentes[1])
    updateTextInput(session, "ylabboxplot", value = "Write ylab...")
    updateTextInput(session, "xlabboxplot", value = "Write xlab...")
    updateColourInput(session, "linecolorboxplot", value = "black")
    updateColourInput(session, "fillboxplot", value = "black")
    updateSliderInput(session, "lwdboxplot", value = 1.5)
    updateSliderInput(session, "fontboxplot", value = 12)
    updateSliderInput(session, "alphaboxplot", value = 0.5)
    updateSelectInput(session, "coloroutlier", selected = "red")
    updateSliderInput(session, "pointboxplot", value = 2)
    updateSliderInput(session, "pchboxplot", value = 16)
    shinyWidgets::updateSwitchInput(session, "nocht", value = FALSE)
  })

  observeEvent(input$resetBoxplot2, {
    updateSelectInput(session, "paletasboxplot2", selected = paletas[1])

    updateSelectInput(session, "themeboxplot2", selected = names(themesb)[1])
    updateSelectInput(session, "fontbox2", selected = fuentes[1])
    updateTextInput(session, "ylabboxplot2", value = "Write ylab...")
    updateTextInput(session, "xlabboxplot2", value = "Write xlab...")
    updateColourInput(session, "linecolorboxplot2", value = "black")
    updateSliderInput(session, "lwdboxplot2", value = 1.5)
    updateSliderInput(session, "fontboxplot2", value = 12)
    updateSliderInput(session, "alphaboxplot2", value = 0.5)
    updateColourInput(session, "coloroutlier2", value = "red")
    updateSliderInput(session, "pointboxplot2", value = 2)
    updateSliderInput(session, "pchboxplot2", value = 16)
    shinyWidgets::updateSwitchInput(session, "nocht2", value = FALSE)
  })

  observeEvent(input$resetViolin, {
    updateSelectInput(session, "themeviolin", selected = names(themesb)[1])
    updateSelectInput(session, "fontviolin", selected = fuentes[1])
    updateTextInput(session, "ylabviolin", value = "Write ylab...")
    updateTextInput(session, "xlabviolin", value = "Write xlab...")
    updateColourInput(session, "linecolorviolin", value = "black")
    updateColourInput(session, "fillviolin", value = "blue")
    updateSliderInput(session, "lwdviolin", value = 1.5)
    updateSliderInput(session, "fontSviolin", value = 12)
    updateSliderInput(session, "alphaviolin", value = 0.5)
    shinyWidgets::updateSwitchInput(session, "trim", value = FALSE)
  })

  observeEvent(input$resetViolin2, {
    updateSelectInput(session, "themeviolin2", selected = names(themesb)[1])
    updateSelectInput(session, "paletasviolin", selected = paletas[1])
    updateSelectInput(session, "fontviolin2", selected = fuentes[1])
    updateTextInput(session, "ylabviolin2", value = "Write ylab...")
    updateTextInput(session, "xlabviolin2", value = "Write xlab...")
    updateColourInput(session, "linecolorviolin2", value = "red")
    updateSliderInput(session, "lwdviolin2", value = 1.5)
    updateSliderInput(session, "fontSviolin2", value = 12)
    updateSliderInput(session, "alphaviolin2", value = 0.5)
    shinyWidgets::updateSwitchInput(session, "trim2", value = FALSE)
  })

  # =====================================================================
  # POLYGON RESET EVENTS
  # =====================================================================

  observeEvent(input$resetPolygon, {
    updateSliderInput(session, "binsPolygon", value = 10)
    updateSelectInput(session, "themePolygon", selected = names(themesb)[1])
    updateSelectInput(session, "fontPolygon", selected = fuentes[1])
    updateTextInput(session, "ylabPolygon", value = "Frequency")
    updateTextInput(session, "xlabPolygon", value = "Values")
    updateColourInput(session, "linecolorPolygon", value = "blue")
    updateColourInput(session, "pointcolorPolygon", value = "red")
    updateSliderInput(session, "lwdPolygon", value = 2)
    updateSliderInput(session, "fontSizePolygon", value = 12)
    updateSliderInput(session, "pointsizePolygon", value = 3)
  })

  observeEvent(input$resetPolygon2, {
    updateSliderInput(session, "binsPolygon2", value = 10)
    updateSelectInput(session, "themePolygon2", selected = names(themesb)[1])
    updateSelectInput(session, "paletasPolygon", selected = paletas[1])
    updateSelectInput(session, "fontPolygon2", selected = fuentes[1])
    updateTextInput(session, "ylabPolygon2", value = "Frequency")
    updateTextInput(session, "xlabPolygon2", value = "Values")
    updateSliderInput(session, "lwdPolygon2", value = 2)
    updateSliderInput(session, "fonSizePolygon2", value = 12)
    updateSliderInput(session, "pointsizePolygon2", value = 3)
  })

  # =====================================================================
  # OGIVE RESET EVENTS
  # =====================================================================

  observeEvent(input$resetOgive, {
    updateSliderInput(session, "binsOgive", value = 10)
    updateSelectInput(session, "themeOgive", selected = names(themesb)[1])
    updateSelectInput(session, "fontOgive", selected = fuentes[1])
    updateTextInput(session, "ylabOgive", value = "Cumulative Frequency")
    updateTextInput(session, "xlabOgive", value = "Values")
    updateColourInput(session, "linecolorOgive", value = "darkgreen")
    updateColourInput(session, "pointcolorOgive", value = "orange")
    updateSliderInput(session, "lwdOgive", value = 2)
    updateSliderInput(session, "fontSizeOgive", value = 12)
    updateSliderInput(session, "pointsizeOgive", value = 3)
  })

  observeEvent(input$resetOgive2, {
    updateSliderInput(session, "binsOgive2", value = 10)
    updateSelectInput(session, "themeOgive2", selected = names(themesb)[1])
    updateSelectInput(session, "paletasOgive", selected = paletas[1])
    updateSelectInput(session, "fontOgive2", selected = fuentes[1])
    updateTextInput(session, "ylabOgive2", value = "Cumulative Frequency")
    updateTextInput(session, "xlabOgive2", value = "Values")
    updateSliderInput(session, "lwdOgive2", value = 2)
    updateSliderInput(session, "fonSizetOgive2", value = 12)
    updateSliderInput(session, "pointsizeOgive2", value = 3)
  })



    observeEvent(input$resetScatterBivariate, {
    updateSelectInput(session, "themescatter", selected = names(themesb)[1])
    updateSelectInput(session, "fontScatter", selected = fuentes[1])
    updateTextInput(session, "ylabscatter", value = "Cumulative Frequency")
    updateTextInput(session, "xlabscatter", value = "Values")
    updateColourInput(session, "colorscatter", value = "darkgreen")
    updateSliderInput(session, "fontSizeScatter", value = 12)
    updateSliderInput(session, "sizescatter", value = 3)
    updateSliderInput(session, "shapescatter", value = 16)
    updateSliderInput(session, "alphascatter", value = 0.5)
  })

  observeEvent(input$resetScatterBivariate2, {

    updateSelectInput(session, "themescatter2", selected = names(themesb)[1])
    updateSelectInput(session, "fontScatter2", selected = fuentes[1])
    updateTextInput(session, "ylabscatter2", value = "Cumulative Frequency")
    updateTextInput(session, "xlabscatter2", value = "Values")
    updateSelectInput(session, "paletascatter", selected = paletas[1])
    updateSliderInput(session, "fontSizeScatter2", value = 12)
    updateSliderInput(session, "sizescatter2", value = 3)
    updateSliderInput(session, "shapescatter2", value = 16)
    updateSliderInput(session, "alphascatter2", value = 0.5)

  })


  # Suponiendo que tus datos están en values$dat y que
# input$selvar1 e input$selvar2 tienen los nombres de columna elegidos

output$tipo_vars <- renderText({
  req(values$dat, input$selvar1, input$selvar2)
  df <- as.data.frame(values$dat)
  x_ok <- input$selvar1 %in% names(df) && is.numeric(df[[input$selvar1]])
  y_ok <- input$selvar2 %in% names(df) && is.numeric(df[[input$selvar2]])
  if (x_ok && y_ok) {
    sprintf("Variable X: %s (numérica) | Variable Y: %s (numérica)",
            input$selvar1, input$selvar2)
  } else {
    "Ambas variables deben ser NUMÉRICAS para el estudio de dependencia y correlación."
  }
})

bivar_tbl <- reactive({
  req(values$dat, input$selvar1, input$selvar2)
  df <- as.data.frame(values$dat)
  validate(need(input$selvar1 %in% names(df), "selvar1 no existe en los datos"))
  validate(need(input$selvar2 %in% names(df), "selvar2 no existe en los datos"))
  validate(need(is.numeric(df[[input$selvar1]]), "selvar1 debe ser numérica"))
  validate(need(is.numeric(df[[input$selvar2]]), "selvar2 debe ser numérica"))

  x <- df[[input$selvar1]]
  y <- df[[input$selvar2]]

  # Filtrar casos completos para evitar NAs en métricas
  ok <- stats::complete.cases(x, y)
  x <- x[ok]; y <- y[ok]

  n  <- length(x)
  validate(need(n >= 3, "Se requieren al menos 3 pares completos."))

  # Covarianza y correlaciones
  cov_xy <- stats::cov(x, y)

  cor_pear <- suppressWarnings(stats::cor(x, y, method = "pearson"))
  cor_spea <- suppressWarnings(stats::cor(x, y, method = "spearman"))
  cor_kend <- suppressWarnings(stats::cor(x, y, method = "kendall"))

  # Regresión lineal Y ~ X
  fit <- stats::lm(y ~ x)
  s   <- summary(fit)

  slope      <- unname(coef(fit)[2])
  intercept  <- unname(coef(fit)[1])
  r2         <- unname(s$r.squared)
  r2_adj     <- unname(s$adj.r.squared)
  p_slope    <- unname(coef(s)[2, "Pr(>|t|)"])
  rmse       <- sqrt(mean(residuals(fit)^2))

  # Selección de qué correlaciones mostrar
  show_all <- identical(input$correlation, "All")
  rows <- list(
    c("N (complete cases)", n),
    c("Covariance", cov_xy),


    if (show_all || input$correlation == "Pearson") c("Pearson r",  cor_pear),
    if (show_all || input$correlation == "Spearman") c("Spearman ρ", cor_spea),
    if (show_all || input$correlation == "Kendall") c("Kendall τ", cor_kend),

    c("Slope (β1)", slope),
    c("Intercept (β0)", intercept),
    c("R²", r2),
    c("Adj. R²", r2_adj),
    c("p-value (slope)", p_slope),
    c("RMSE", rmse)
  )

  out <- do.call(rbind, rows)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- c("Metric", "Value")

  # Convertir numéricos donde aplique y formatear
  out$Value <- suppressWarnings(as.numeric(out$Value))
  is_num <- !is.na(out$Value)
  out$Value[is_num] <- signif(out$Value[is_num], 6)

  out
})

output$bivar_metrics <- DT::renderDT({
  df <- bivar_tbl()
  DT::datatable(
    df,
    rownames = FALSE,
    extensions = c("Buttons"),
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "print"),
      pageLength = 12
    )
  )
})



  ########################## Fin descriptivos#####################
}) # FIN
