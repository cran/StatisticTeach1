source(file.path(appDir, "pages/descriptive_statistics/qual_vars/tabular_summary.r"), local = TRUE)
source(file.path(appDir, "pages/descriptive_statistics/qual_vars/graphic_summaries.r"), local = TRUE)
qualitative_variables <- shinydashboard::tabItem( tabName = "Qualitative_variables",
     shinydashboard::box(   height =  "100%",   width = 12,  title = "Qualitative variables",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       sidebarPanel(
         shinyWidgets::actionBttn(
           "helpquali",
           "Help",
           icon = icon("question-circle"),
           style = "stretch",
           block = FALSE,
           color = "primary"
         ),
         shinyBS::bsModal(
           id = "modalHelpQuali",
           title = "Help: Qualitative Variables",
           trigger = "helpquali",
           size = "large",
           tags$iframe(src = "qualitative_variables.html", width = "100%", height = "600px", frameborder = 0)
         ),
         selectizeInput('selvarcuali',
                        'Qualitative variable',
                        choices = NULL,
                        multiple = FALSE),
         shinyWidgets::prettyCheckbox(
           inputId = "GrupoQuali",
           label = "Statistics by group?",
           value = FALSE,
           status = "success"
         ),
         conditionalPanel(
           condition = "input.GrupoQuali == 1",
           selectizeInput('selgrupoQuali',
                          'Group by',
                          choices = NULL,
                          multiple = TRUE,
                      
                          )
           )
       ),
       mainPanel(
         tabsetPanel(
           tabular_summary,
           graphic_summaries))
       )
     )
