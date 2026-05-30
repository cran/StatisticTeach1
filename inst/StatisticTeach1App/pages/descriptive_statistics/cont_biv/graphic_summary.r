 Graphic_summary_ContinuousBivariate <- tabPanel(
          "Graphic summary",
          strong(h2("Scatterplot")),
          br(),
          conditionalPanel(
            condition = "input.Gruposcatter == 0",
            plotOutput("scat"),
            br(),
            downloadButton("ContinuousBivariateScatterDownloadNoGrupo", "Download Graph", class = "btn-primary"),
            br(), br(),
            shinyWidgets::dropdown(
              label = "Customize graph",
              tags$h3("Customize"),
              div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetScatterBivariate",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
              div(
                style = "display: inline-block;vertical-align:top;
                               width: 150px;",
                selectInput("themescatter",
                  " Theme",
                  choices = names(themesb)
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                selectInput("fontScatter",
                  " Font",
                  choices = fuentes
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;width: 150px;",
                textInput(
                  "ylabscatter",
                  label = "ylab",
                  value = "Write ylab..."
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;width: 150px;",
                textInput(
                  "xlabscatter",
                  label = "xlab",
                  value = "Write xlab..."
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;width: 150px;",
                colourInput(
                  "colorscatter",
                  label = "Color",
                  value = "blue"
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "fontSizeScatter",
                  label = "Size font",
                  min = 1,
                  max = 50,
                  value = 12
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "sizescatter",
                  label = "Size",
                  min = 1,
                  max = 50,
                  value = 3
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "shapescatter",
                  label = "Shape",
                  min = 1,
                  max = 25,
                  value = 16
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "alphascatter",
                  label = "alpha",
                  min = 0.001,
                  max = 1,
                  value = 0.5
                )
              ),
              style = "unite",
              icon = icon("fish"),
              status = "primary",
              width = "500px",
              animate = shinyWidgets::animateOptions(
                enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
              )
            )
          ),
          conditionalPanel(
            condition = "input.Gruposcatter == 1",
            plotOutput("scat2"),
            br(),
            downloadButton("ContinuousBivariateScatterDownloadGrupo", "Download Graph", class = "btn-primary"),
            br(), br(),
            shinyWidgets::dropdown(
              label = "Customize graph",
              tags$h3("Customize"),
               div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetScatterBivariate2",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
              div(
                style = "display: inline-block;vertical-align:top;
                               width: 150px;",
                selectInput("themescatter2",
                  " Theme",
                  choices = names(themesb)
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                selectInput("fontScatter2",
                  " Font",
                  choices = fuentes
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;width: 150px;",
                textInput(
                  "ylabscatter2",
                  label = "ylab",
                  value = "Write ylab..."
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;width: 150px;",
                textInput(
                  "xlabscatter2",
                  label = "xlab",
                  value = "Write xlab..."
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                selectInput("paletascatter",
                  "Palletes",
                  choices = paletas
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "fontSizeScatter2",
                  label = "Size font",
                  min = 1,
                  max = 50,
                  value = 12
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "sizescatter2",
                  label = "Size",
                  min = 1,
                  max = 50,
                  value = 3
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "shapescatter2",
                  label = "Shape",
                  min = 1,
                  max = 25,
                  value = 16
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                sliderInput(
                  inputId = "alphascatter2",
                  label = "alpha",
                  min = 0.001,
                  max = 1,
                  value = 0.5
                )
              ),
              style = "unite",
              icon = icon("fish"),
              status = "primary",
              width = "500px",
              animate = shinyWidgets::animateOptions(
                enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
                exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
              )
            )
          )
        )
