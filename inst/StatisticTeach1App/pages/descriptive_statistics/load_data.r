load_dataPage <- shinydashboard::tabItem( tabName = "Load_data",
      tags$head(
        tags$style(HTML("
          /* Global styles to ensure all text is black */
          .tab-pane label, .tab-pane .control-label { color: #000000 !important; }
          .tab-pane .form-control { color: #000000 !important; }
          .tab-pane strong { color: #000000 !important; }
          .tab-pane .checkbox label { color: #000000 !important; }
          .tab-pane .shiny-input-container label { color: #000000 !important; }
          .tab-pane .form-group label { color: #000000 !important; }
          .tab-pane .file-input-container label { color: #000000 !important; }
          .tab-pane .selectize-input { color: #000000 !important; }
          .tab-pane .selectize-dropdown { color: #000000 !important; }
          .tab-pane .btn-file { color: #000000 !important; }
          .tab-pane .input-group-addon { color: #000000 !important; }
          .tab-pane .help-block { color: #000000 !important; }
          .tab-pane .text-muted { color: #000000 !important; }
          .tab-pane .text-info { color: #000000 !important; }
          .tab-pane .text-success { color: #000000 !important; }
          .tab-pane .text-warning { color: #000000 !important; }
          .tab-pane .text-danger { color: #000000 !important; }
          .tab-pane .pretty { color: #000000 !important; }
          .tab-pane .pretty .state label { color: #000000 !important; }
          .tab-pane .pretty .state label:before { color: #000000 !important; }
          .tab-pane .pretty .state label:after { color: #000000 !important; }
          
          /* Improved styling for column type selector */
          .column-type-row { 
            background: #f8f9fa; 
            border: 1px solid #e9ecef; 
            border-radius: 6px; 
            padding: 12px; 
            margin-bottom: 8px; 
            transition: all 0.3s ease;
          }
          .column-type-row:hover { 
            background: #e9ecef; 
            border-color: #007bff; 
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
          .column-type-row label { 
            font-weight: bold; 
            color: #495057 !important; 
            margin-bottom: 8px; 
          }
          .column-type-row .form-control { 
            border-radius: 4px; 
            border: 1px solid #ced4da; 
          }
          .column-type-row .form-control:focus { 
            border-color: #007bff; 
            box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25); 
          }
          .decimal-separator-section, .date-format-section { 
            background: #e7f3ff; 
            border: 1px solid #b3d9ff; 
            border-radius: 4px; 
            padding: 8px; 
            margin-top: 5px; 
          }
          .decimal-separator-section label, .date-format-section label { 
            font-size: 12px; 
            color: #0056b3 !important; 
            font-weight: normal; 
          }
          
          /* Improved table styling */
          .shiny-table-output table {
            width: 100% !important;
            border-collapse: collapse !important;
            margin: 0 !important;
            font-size: 13px !important;
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
          }
          .shiny-table-output th {
            background: linear-gradient(135deg, #e9ecef, #f8f9fa) !important;
            color: #000000 !important;
            font-weight: 600 !important;
            padding: 12px 8px !important;
            text-align: center !important;
            border: 1px solid #dee2e6 !important;
            font-size: 14px !important;
            text-transform: uppercase !important;
            letter-spacing: 0.5px !important;
          }
          .shiny-table-output td {
            padding: 10px 8px !important;
            border: 1px solid #e9ecef !important;
            text-align: center !important;
            vertical-align: middle !important;
            color: #000000 !important;
            font-weight: 500 !important;
          }
          .shiny-table-output tr:nth-child(even) {
            background-color: #f8f9fa !important;
          }
          .shiny-table-output tr:nth-child(odd) {
            background-color: white !important;
          }
          .shiny-table-output tr:hover {
            background-color: #e3f2fd !important;
            transform: scale(1.01) !important;
            transition: all 0.2s ease !important;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
          }
          .shiny-table-output .table {
            box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
            border-radius: 8px !important;
            overflow: hidden !important;
            border: none !important;
          }
          .shiny-table-output .table-striped > tbody > tr:nth-of-type(odd) {
            background-color: white !important;
          }
          .shiny-table-output .table-striped > tbody > tr:nth-of-type(even) {
            background-color: #f8f9fa !important;
          }
          .shiny-table-output .table-hover > tbody > tr:hover {
            background-color: #e3f2fd !important;
          }
          .shiny-table-output .table-bordered {
            border: 1px solid #dee2e6 !important;
          }
          .shiny-table-output .table-bordered > thead > tr > th,
          .shiny-table-output .table-bordered > tbody > tr > td {
            border: 1px solid #dee2e6 !important;
          }
          
          /* Force all table text to be black with maximum specificity */
          .shiny-table-output,
          .shiny-table-output *,
          .shiny-table-output table,
          .shiny-table-output table *,
          .shiny-table-output thead,
          .shiny-table-output thead *,
          .shiny-table-output tbody,
          .shiny-table-output tbody *,
          .shiny-table-output tr,
          .shiny-table-output tr *,
          .shiny-table-output th,
          .shiny-table-output th *,
          .shiny-table-output td,
          .shiny-table-output td *,
          .shiny-table-output .table,
          .shiny-table-output .table *,
          .shiny-table-output .table-striped,
          .shiny-table-output .table-striped *,
          .shiny-table-output .table-hover,
          .shiny-table-output .table-hover *,
          .shiny-table-output .table-bordered,
          .shiny-table-output .table-bordered * {
            color: #000000 !important;
          }
          
          /* Override any Bootstrap or other framework styles */
          .shiny-table-output .text-white,
          .shiny-table-output .text-light,
          .shiny-table-output .text-muted {
            color: #000000 !important;
          }
        "))
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            width = 12,
            title = tags$h3(icon("database"), " Load Data", style = "margin: 0; color: #000000;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            background = "light-blue",
            fluidRow(
              column(
                width = 4,
                div(class = "well-panel",
                  style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border: 1px solid #dee2e6; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                  tags$style(HTML("
                    .well-panel label, .well-panel .control-label { color: #000000 !important; }
                    .well-panel .form-control { color: #000000 !important; }
                    .well-panel strong { color: #000000 !important; }
                    .well-panel .checkbox label { color: #000000 !important; }
                    .well-panel .pretty-checkbox label { color: #000000 !important; }
                    .well-panel .shiny-input-container label { color: #000000 !important; }
                    .well-panel .form-group label { color: #000000 !important; }
                    .well-panel .file-input-container label { color: #000000 !important; }
                    .well-panel .selectize-input { color: #000000 !important; }
                    .well-panel .selectize-dropdown { color: #000000 !important; }
                    .well-panel .btn-file { color: #000000 !important; }
                    .well-panel .input-group-addon { color: #000000 !important; }
                    .well-panel .help-block { color: #000000 !important; }
                    .well-panel .text-muted { color: #000000 !important; }
                    .well-panel .text-info { color: #000000 !important; }
                    .well-panel .text-success { color: #000000 !important; }
                    .well-panel .text-warning { color: #000000 !important; }
                    .well-panel .text-danger { color: #000000 !important; }
                    .well-panel .pretty { color: #000000 !important; }
                    .well-panel .pretty .state label { color: #000000 !important; }
                    .well-panel .pretty .state label:before { color: #000000 !important; }
                    .well-panel .pretty .state label:after { color: #000000 !important; }
                    
                    /* Column type selector improvements */
                    .well-panel .column-type-row { 
                      background: #f8f9fa; 
                      border: 1px solid #e9ecef; 
                      border-radius: 6px; 
                      padding: 12px; 
                      margin-bottom: 8px; 
                      transition: all 0.3s ease;
                    }
                    .well-panel .column-type-row:hover { 
                      background: #e9ecef; 
                      border-color: #007bff; 
                      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                    }
                    .well-panel .column-type-row label { 
                      font-weight: bold; 
                      color: #495057 !important; 
                      margin-bottom: 8px; 
                    }
                    .well-panel .decimal-separator-section, .well-panel .date-format-section { 
                      background: #e7f3ff; 
                      border: 1px solid #b3d9ff; 
                      border-radius: 4px; 
                      padding: 8px; 
                      margin-top: 5px; 
                    }
                    .well-panel .decimal-separator-section label, .well-panel .date-format-section label { 
                      font-size: 12px; 
                      color: #0056b3 !important; 
                      font-weight: normal; 
                    }
                    
                    /* Table styling for the panel */
                    .well-panel .shiny-table-output table {
                      width: 100% !important;
                      border-collapse: collapse !important;
                      margin: 0 !important;
                      font-size: 13px !important;
                      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
                      color: #000000 !important;
                    }
                    .well-panel .shiny-table-output th {
                      background: linear-gradient(135deg, #e9ecef, #f8f9fa) !important;
                      color: #000000 !important;
                      font-weight: 600 !important;
                      padding: 12px 8px !important;
                      text-align: center !important;
                      border: 1px solid #dee2e6 !important;
                      font-size: 14px !important;
                      text-transform: uppercase !important;
                      letter-spacing: 0.5px !important;
                    }
                    .well-panel .shiny-table-output td {
                      padding: 10px 8px !important;
                      border: 1px solid #e9ecef !important;
                      text-align: center !important;
                      vertical-align: middle !important;
                      color: #495057 !important;
                      font-weight: 500 !important;
                    }
                    .well-panel .shiny-table-output tr:nth-child(even) {
                      background-color: #f8f9fa !important;
                    }
                    .well-panel .shiny-table-output tr:nth-child(odd) {
                      background-color: white !important;
                    }
                    .well-panel .shiny-table-output tr:hover {
                      background-color: #e3f2fd !important;
                      transform: scale(1.01) !important;
                      transition: all 0.2s ease !important;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
                    }
                    
                    /* Force all table text to be black in panel with maximum specificity */
                    .well-panel .shiny-table-output,
                    .well-panel .shiny-table-output *,
                    .well-panel .shiny-table-output table,
                    .well-panel .shiny-table-output table *,
                    .well-panel .shiny-table-output thead,
                    .well-panel .shiny-table-output thead *,
                    .well-panel .shiny-table-output tbody,
                    .well-panel .shiny-table-output tbody *,
                    .well-panel .shiny-table-output tr,
                    .well-panel .shiny-table-output tr *,
                    .well-panel .shiny-table-output th,
                    .well-panel .shiny-table-output th *,
                    .well-panel .shiny-table-output td,
                    .well-panel .shiny-table-output td *,
                    .well-panel .shiny-table-output .table,
                    .well-panel .shiny-table-output .table *,
                    .well-panel .shiny-table-output .table-striped,
                    .well-panel .shiny-table-output .table-striped *,
                    .well-panel .shiny-table-output .table-hover,
                    .well-panel .shiny-table-output .table-hover *,
                    .well-panel .shiny-table-output .table-bordered,
                    .well-panel .shiny-table-output .table-bordered * {
                      color: #000000 !important;
                    }
                    
                    /* Override any Bootstrap or other framework styles in panel */
                    .well-panel .shiny-table-output .text-white,
                    .well-panel .shiny-table-output .text-light,
                    .well-panel .shiny-table-output .text-muted {
                      color: #000000 !important;
                    }
                  ")),
                  tags$h4(icon("cog"), " Data Source Configuration", style = "color: #000000; margin-bottom: 20px; border-bottom: 2px solid #007bff; padding-bottom: 10px;"),
                  
                  # Help Button with improved styling
                  div(
                    style = "margin-bottom: 20px;",
                    shinyWidgets::actionBttn(
                      "helpload",
                      "Help Guide",
                      icon = icon("question-circle"),
                      style = "gradient",
                      block = TRUE,
                      color = "primary",
                      size = "sm"
                    )
                  ),
                  
                  # Help Modal
                  shinyBS::bsModal(
                    id="helploaddata",
                    title = tags$h3(icon("info-circle"), " Data Loading Guide"),
                    trigger = "helpload",
                    size="large",
                    tags$iframe(
                      src = "load_data.html",
                      width = "100%",
                      height = "1000px",
                      frameborder = 0,
                      scrolling = "auto"
                    )
                  ),
                  
                  # Example Data Section
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 20px; border-left: 4px solid #28a745;",
                    tags$h5(icon("flask"), " Example Data", style = "color: #000000; margin-bottom: 15px;"),
                    shinyWidgets::prettyCheckbox(
                      inputId = "example",
                      label = "Use example data set?",
                      value = FALSE,
                      status = "success",
                      shape = "round",
                      outline = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.example == 1",
                      div(
                        style = "margin-top: 10px;",
                        selectInput('selexample',
                                   'Select Example Dataset:',
                                   choices = c("iris","fishes"),
                                   multiple = FALSE,
                                   selectize = TRUE)
                      )
                    )
                  ),
                  
                  # File Upload Section
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 6px; margin-bottom: 20px; border-left: 4px solid #007bff;",
                    tags$h5(icon("upload"), " File Upload", style = "color: #000000; margin-bottom: 15px;"),
                    fileInput(
                      "file1",
                      "Choose File:",
                      accept = c(".csv", ".xlsx", ".xls"),
                      multiple = FALSE,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"
                    ),
                    selectInput(
                      inputId = "worksheet",
                      label = "Worksheet Name:",
                      choices = NULL,
                      selectize = TRUE
                    ),
                    div(
                      style = "margin-top: 15px; color: #000000;",
                      actionButton(
                        inputId = "getData",
                        label = "Load Data",
                        icon = icon("download"),
                        class = "btn-primary btn-block ",
                        style = "background: linear-gradient(45deg, #007bff, #0056b3); border: none; border-radius: 6px; padding: 10px;"
                      )
                    )
                  ),
                  
                  # Column Types Section
                  div(
                    style = "background: #f8f9fa; padding: 15px; border-radius: 6px; border-left: 4px solid #ffc107;",
                    tags$h5(icon("columns"), " Column Types Configuration", style = "color: #000000; margin-bottom: 15px; border-bottom: 2px solid #ffc107; padding-bottom: 8px;"),
                    
                    # Info panel
                    div(
                      style = "background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 6px; padding: 10px; margin-bottom: 15px;",
                      tags$p(
                        icon("info-circle"), 
                        " Configure the data types for each column. This helps ensure proper data analysis.",
                        style = "margin: 0; color: #856404; font-size: 12px;"
                      )
                    ),
                    
                    # Column types selector with improved styling
                    div(
                      style = "max-height: 300px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 6px; padding: 10px; background: white; margin-bottom: 15px;",
                      uiOutput("type_selector")
                    ),
                    
                    # Apply button with improved styling
                    div(
                      style = "text-align: center;",
                      shinyWidgets::actionBttn(
                        "apply_types",
                        "Apply Column Types",
                        icon = icon("check-circle"),
                        style = "gradient",
                        block = TRUE,
                        color = "success",
                        size = "md"
                      )
                    ),
                    
                    # Status indicator
                    div(
                      id = "type_status",
                      style = "margin-top: 10px; text-align: center; display: none;",
                      tags$span(
                        icon("check-circle"),
                        " Column types applied successfully!",
                        style = "color: #28a745; font-weight: bold;"
                      )
                    )
                  )
                )
              ),
              
              # Main Panel with improved styling
              column(
                width = 8,
                div(
                  style = "background: white; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); min-height: 400px;",
                  tags$h4(icon("table"), " Data Preview", style = "color: #000000; margin-bottom: 20px; border-bottom: 2px solid #dee2e6; padding-bottom: 10px;"),
                  div(
                    style = "background: #f8f9fa; border-radius: 6px; padding: 15px; border-left: 4px solid #007bff;",
                    tags$style(HTML("
                      #table1, #table1 * { color: #000000 !important; }
                      #table1 table, #table1 table * { color: #000000 !important; }
                      #table1 th, #table1 th * { color: #000000 !important; }
                      #table1 td, #table1 td * { color: #000000 !important; }
                    ")),
                    tableOutput("table1")
                  )
                )
              )
            )
          )
        )
      )
    )
