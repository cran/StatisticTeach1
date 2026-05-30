multivariate <- shinydashboard::tabItem( tabName = "Multivariate",
     shinydashboard::box( height =  "100%", width = 12,title = "Multivariate",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       sidebarPanel(
          shinyWidgets::actionBttn(
            "helpmulti",
            "Help",
            icon = icon("question-circle"),
            style = "stretch",
            block = FALSE,
            color = "primary"
          ),
          shinyBS::bsModal(
            id = "modalHelpMulti",
            title = "Help: Multivariate",
            trigger = "helpmulti",
            size = "large",
            tags$iframe(src = "multivariate.html", width = "100%", height = "600px", frameborder = 0)
          ),
         selectizeInput('selmulti',
                        'Variables',
                        choices = NULL,
                        multiple = TRUE),
         shinyWidgets::prettyCheckbox(
           inputId = "Grupomulti",
           label = "Statistics by group?",
           value = FALSE,
           status = "success"
         )

         ),
       mainPanel(
         tabsetPanel(
           tabPanel("Summaries",
                    br(), strong(h2("Correlation matrix")),
                    DT::DTOutput("cormat"),
                    br(), strong(h2("Variances and covariances matrix")),
                    DT::DTOutput("covmat"),br()
                     ),
       
                    
         )
       )
     )
   )
