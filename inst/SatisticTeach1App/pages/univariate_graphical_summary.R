graphical_summary <- tabPanel(
  "Graphic summary",
  div(style = "display: inline-block;vertical-align:top;
                        width: 150px;", shinyWidgets::pickerInput(
    inputId = "SelecTipografico",
    label = "Select :",
    choices = c(
      "",
      "Histogram",
      "Density",
      "Boxplot",
      "Violin",
      "Polygon",
      "Ogive"
    ),
    options = list(
      style = "btn-primary"
    )
  )), div(style = "display: inline-block; vertical-align:top; margin-left: 10px; padding-top: 25px;",
      prettySwitch(
        inputId = "horizontal_plot",
        label = "Horizontal", 
        status = "primary",
        slim = TRUE
      )
  ),
  conditionalPanel(
    condition = "input.SelecTipografico == 'Histogram'& input.Grupo == 0",
    plotOutput("plotdescript"),
    br(),
    downloadButton("ContinuousUnivariateHistogramDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetHistogram",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsHist",
        "Bins",
        min = 4,
        max = 500,
        value = 10,
        step = 1
      ),
      div(
        style = "display: inline-block;vertical-align:top;
                               width: 150px;",
        selectInput("themeHist",
          " Theme",
          choices = names(themesb)
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("fontH",
          " Font",
          choices = fuentes
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "ylabHist",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "xlabHist",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        colourInput(
          label = "Line_color",
          "linecolorHist", "red", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;", colourInput(
          label = "Fill_color",
          "fillHist", "blue", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "lwdHist",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 1.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "fontHist",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaHist",
          label = "alpha",
          min = 0.001,
          max = 1,
          value = 0.3
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
    condition = "input.SelecTipografico == 'Histogram'& input.Grupo == 1",
    plotOutput("plotdescript2"),
    br(),
    downloadButton("ContinuousUnivariateHistogramDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetHistogram2",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsHist2",
        "Bins",
        min = 4,
        max = 500,
        value = 10,
        step = 1
      ),
      div(
        style = "display: inline-block;vertical-align:top;
                               width: 150px;",
        selectInput("themeHist2",
          " Theme",
          choices = names(themesb)
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("fontH2",
          " Font",
          choices = fuentes
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        selectInput("paletas",
          "Palletes",
          choices = paletas
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "linecolorHist2",
          label = "Line color",
          value = "black"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput(
          "ylabHist2",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "xlabHist2",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "lwdHist2",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 2
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "fontHist2",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaHist2",
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
    condition = "input.SelecTipografico == 'Density'& input.Grupo == 0",
    plotOutput("plotdescript3"),
    br(),
    downloadButton("ContinuousUnivariateDensityDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetDensity",
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
        selectInput("themeDensy",
          " Theme",
          choices = names(themesb)
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("fontdensy",
          " Font",
          choices = fuentes
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        colourInput(
          label = "Line_color",
          "linecolorDensy", "blue", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        colourInput(
          label = "Fill_color",
          "fillDensy", "blue", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "ylabDensy",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "xlabDensy",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "lwdDensy",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 2
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "fontDensy",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaDensy",
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
    condition = "input.SelecTipografico == 'Density'& input.Grupo == 1",
    plotOutput("plotdescript4"),
    br(),
    downloadButton("ContinuousUnivariateDensityDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetDensity2",
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
        selectInput("themeDensy2",
          " Theme",
          choices = names(themesb)
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("paletasDensy2",
          "Palletes",
          choices = paletas
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("fontdensy2",
          " Font",
          choices = fuentes
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("paletas",
          "Palletes",
          choices = paletas
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput(
          "linecolorDensy2",
          label = "Line color",
          value = "black"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "ylabDensy2",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput(
          "xlabDensy2",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "lwdDensy2",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 2
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "fontDensy2",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaDensy2",
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
    condition = "input.SelecTipografico == 'Boxplot'& input.Grupo == 0",
    plotOutput("plotdescript5"),
    br(),
    downloadButton("ContinuousUnivariateBoxplotDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph", tags$h3("Customize"),
      div(
        style = "display: inline-block;vertical-align:top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetBoxplot",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("themeboxplot", " Theme",
        choices = names(themesb)
      )),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("fontbox", " Font", choices = fuentes)),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput("ylabboxplot",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput("xlabboxplot",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;
        vertical-align:top; width: 150px;",
        colourInput(
          label = "Line_color",
          "linecolorboxplot", "black",
          palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        colourInput(
          label = "Fill_color",
          "fillboxplot", "blue",
          palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        colourInput(
          label = "Outlier_color",
          "coloroutlier", "red",
          palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "lwdboxplot",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 1.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "fontboxplot",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "pointboxplot",
          label = "Size point",
          min = 1,
          max = 10,
          value = 2
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaboxplot",
          label = "alpha",
          min = 0.001,
          max = 1,
          value = 0.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "pchboxplot",
          label = " Point shape",
          min = 0,
          max = 25,
          value = 16
        )
      ),
      shinyWidgets::switchInput(
        inputId = "nocht",
        label = "Nocht",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"
      ),
      style = "unite",
      icon = icon("fish"),
      status = "primary",
      width = "500px",
      animate = shinyWidgets::animateOptions(
        enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
        exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
      )
    ),
    h2("Outlier - No Group"),
    DT::DTOutput("outlierNoGroup")
  ),
  conditionalPanel(
    condition = "input.SelecTipografico == 'Boxplot'& input.Grupo == 1",
    plotOutput(outputId = "graph", brush = "plot_brush"),
    br(),
    downloadButton("ContinuousUnivariateBoxplotDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph", tags$h3("Customize"),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", shinyWidgets::actionBttn(
        inputId = "resetBoxplot2",
        label = "Reset to Initial State",
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("refresh")
      )),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("themeboxplot2", " Theme",
        choices = names(themesb)
      )),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("paletasboxplot2",
          "Palletes",
          choices = paletas
        )
      ),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("fontbox2", " Font", choices = fuentes)),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput("ylabboxplot2",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput("xlabboxplot2",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;
        vertical-align:top; width: 150px;",
        textInput("linecolorboxplot2",
          label = "Color box",
          value = "black"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        colourInput("coloroutlier2", "red",
          label = "Color outlier"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "lwdboxplot2",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 1.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "fontboxplot2",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "pointboxplot2",
          label = "Size point",
          min = 1,
          max = 10,
          value = 2
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaboxplot2",
          label = "alpha",
          min = 0.001,
          max = 1,
          value = 0.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "pchboxplot2",
          label = " Point shape",
          min = 0,
          max = 25,
          value = 16
        )
      ),
      shinyWidgets::switchInput(
        inputId = "nocht2",
        label = "Nocht",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"
      ),
      style = "unite",
      icon = icon("fish"),
      status = "primary",
      width = "500px",
      animate = shinyWidgets::animateOptions(
        enter = shinyWidgets::animations$fading_entrances$fadeInLeftBig,
        exit = shinyWidgets::animations$fading_exits$fadeOutRightBig
      )
    ),
    h2("Outlier - Group"),
    DT::DTOutput("outlier")
  ),
  conditionalPanel(
    condition = "input.SelecTipografico == 'Violin'& input.Grupo == 0",
    plotOutput("plotdescript6"),
    br(),
    downloadButton("ContinuousUnivariateViolinDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph", tags$h3("Customize"),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", shinyWidgets::actionBttn(
        inputId = "resetViolin",
        label = "Reset to Initial State",
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("refresh")
      )),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("themeviolin", " Theme",
        choices = names(themesb)
      )),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("fontviolin", " Font", choices = fuentes)),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput("ylabviolin",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput("xlabviolin",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;
        vertical-align:top; width: 150px;",
        textInput("linecolorviolin",
          label = "Color line",
          value = "red"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput("fillviolin",
          label = "Color fill",
          value = "#0095ff"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "lwdviolin",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 1.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "fontSviolin",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaviolin",
          label = "alpha",
          min = 0.001,
          max = 1,
          value = 0.5
        )
      ),
      shinyWidgets::switchInput(
        inputId = "trim",
        label = "Trim",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"
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
    condition = "input.SelecTipografico == 'Violin'& input.Grupo == 1",
    plotOutput("plotdescript7"),
    br(),
    downloadButton("ContinuousUnivariateViolinDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph", tags$h3("Customize"),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", shinyWidgets::actionBttn(
        inputId = "resetViolin2",
        label = "Reset to Initial State",
        style = "bordered",
        color = "warning",
        size = "sm",
        icon = icon("refresh")
      )),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("themeviolin2", " Theme",
        choices = names(themesb)
      )),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput("paletasviolin",
          "Palletes",
          choices = paletas
        )
      ),
      div(style = "display: inline-block;vertical-align:top;
        width: 150px;", selectInput("fontviolin2", " Font", choices = fuentes)),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        textInput("ylabviolin2",
          label = "ylab",
          value = "Write ylab..."
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        textInput("xlabviolin2",
          label = "xlab",
          value = "Write xlab..."
        )
      ),
      div(
        style = "display: inline-block;
        vertical-align:top; width: 150px;",
        textInput("linecolorviolin2",
          label = "Color line",
          value = "red"
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "lwdviolin2",
          label = "Line width",
          min = 0.1,
          max = 10,
          value = 1.5
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top;width: 150px;",
        sliderInput(
          inputId = "fontSviolin2",
          label = "Size font",
          min = 1,
          max = 50,
          value = 12
        )
      ),
      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        sliderInput(
          inputId = "alphaviolin2",
          label = "alpha",
          min = 0.001,
          max = 1,
          value = 0.5
        )
      ),
      shinyWidgets::switchInput(
        inputId = "trim2",
        label = "Trim",
        onLabel = "TRUE",
        offLabel = "FALSE",
        size = "mini"
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

  # =====================================================================
  # POLYGON PLOTS
  # =====================================================================

  # Polygon without groups
  conditionalPanel(
    condition = "input.SelecTipografico == 'Polygon'& input.Grupo == 0",
    plotOutput("plotdescript8"),
    br(),
    downloadButton("ContinuousUnivariatePolygonDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block; vertical-align: top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetPolygon",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsPolygon", "Bins", min = 4, max = 500, value = 10, step = 1),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("themePolygon", " Theme", choices = names(themesb))
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("fontPolygon", "Font", choices = fuentes)
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("ylabPolygon", label = "ylab", value = "Frequency")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("xlabPolygon", label = "xlab", value = "Values")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        colourInput(
          label = "Line_color",
          "linecolorPolygon", "blue", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        colourInput(
          label = "Point_color",
          "pointcolorPolygon", "red", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "lwdPolygon",
          label = "Line width",
          min = 0.1, max = 10, value = 2
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "fontSizePolygon",
          label = "Size font",
          min = 1, max = 50, value = 12
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "pointsizePolygon",
          label = "Point size",
          min = 1, max = 10, value = 3
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

  # Polygon with groups
  conditionalPanel(
    condition = "input.SelecTipografico == 'Polygon'& input.Grupo == 1",
    plotOutput("plotdescript9"),
    br(),
    downloadButton("ContinuousUnivariatePolygonDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block; vertical-align: top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetPolygon2",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsPolygon2", "Bins", min = 4, max = 500, value = 10, step = 1),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("themePolygon2", " Theme", choices = names(themesb))
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("paletasPolygon", "Palletes", choices = paletas)
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("fontPolygon2", " Font", choices = fuentes)
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("ylabPolygon2", label = "ylab", value = "Frequency")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("xlabPolygon2", label = "xlab", value = "Values")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "lwdPolygon2",
          label = "Line width",
          min = 0.1, max = 10, value = 2
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "fontSizePolygon2",
          label = "Size font",
          min = 1, max = 50, value = 12
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "pointsizePolygon2",
          label = "Point size",
          min = 1, max = 10, value = 3
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

  # =====================================================================
  # OGIVE PLOTS
  # =====================================================================

  # Ogive without groups
  conditionalPanel(
    condition = "input.SelecTipografico == 'Ogive'& input.Grupo == 0",
    plotOutput("plotdescript10"),
    br(),
    downloadButton("ContinuousUnivariateOgiveDownloadGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block; vertical-align: top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetOgive",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsOgive", "Bins", min = 4, max = 500, value = 10, step = 1),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("themeOgive", " Theme", choices = names(themesb))
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput(
          "fontOgive", " Font",
          choices = fuentes
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("ylabOgive", label = "ylab", value = "Cumulative Frequency")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("xlabOgive", label = "xlab", value = "Values")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        colourInput(
          label = "Line_color",
          "linecolorOgive", "darkgreen", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        colourInput(
          label = "Point_color",
          "pointcolorOgive", "orange", palette = "square",
          showColour = "background"
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "lwdOgive",
          label = "Line width",
          min = 0.1, max = 10, value = 2
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "fontSizeOgive",
          label = "Size font",
          min = 1, max = 50, value = 12
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "pointsizeOgive",
          label = "Point size",
          min = 1, max = 10, value = 3
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

  # Ogive with groups
  conditionalPanel(
    condition = "input.SelecTipografico == 'Ogive'& input.Grupo == 1",
    plotOutput("plotdescript11"),
    br(),
    downloadButton("ContinuousUnivariateOgiveDownloadNoGrupo", "Download Graph", class = "btn-primary"),
    br(), br(),
    shinyWidgets::dropdown(
      label = "Customize graph",
      tags$h3("Customize"),
      div(
        style = "display: inline-block; vertical-align: top; width: 100%; text-align: center; margin-bottom: 10px;",
        shinyWidgets::actionBttn(
          inputId = "resetOgive2",
          label = "Reset to Initial State",
          style = "bordered",
          color = "warning",
          size = "sm",
          icon = icon("refresh")
        )
      ),
      sliderInput("binsOgive2", "Bins", min = 4, max = 500, value = 10, step = 1),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("themeOgive2", " Theme", choices = names(themesb))
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("paletasOgive", "Palletes", choices = paletas)
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        selectInput("fontOgive2", " Font", choices = fuentes)
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("ylabOgive2", label = "ylab", value = "Cumulative Frequency")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        textInput("xlabOgive2", label = "xlab", value = "Values")
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "lwdOgive2",
          label = "Line width",
          min = 0.1, max = 10, value = 2
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "fontSizeOgive2",
          label = "Size font",
          min = 1, max = 50, value = 12
        )
      ),
      div(
        style = "display: inline-block; vertical-align: top; width: 150px;",
        sliderInput(
          inputId = "pointsizeOgive2",
          label = "Point size",
          min = 1, max = 10, value = 3
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
