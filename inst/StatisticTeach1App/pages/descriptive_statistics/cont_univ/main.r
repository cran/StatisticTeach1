# =============================================================================
# Continuous Variables Univariate Analysis - Main Module
# =============================================================================
# This file serves as the main entry point for the continuous variables 
# univariate analysis module. It sources the individual tab components
# and assembles them into the complete UI.
# =============================================================================

# =============================================================================
# SOURCE INDIVIDUAL TAB COMPONENTS
# =============================================================================

# Load numerical summary tab component
source(file.path(appDir, "pages/descriptive_statistics/cont_univ/numerical_summary.r"), local = TRUE)

# Load tabular summary tab component  
source(file.path(appDir, "pages/descriptive_statistics/cont_univ/tabular_summary.r"), local = TRUE)

# Load graphical summary tab component
source(file.path(appDir, "pages/descriptive_statistics/cont_univ/graphical_summary.r"), local = TRUE)

# =============================================================================
# MAIN UI COMPONENT
# =============================================================================

continuous_variables_univariate <- shinydashboard::tabItem(
  tabName = "Continuous_variables_univariate",
  
  # Main container box
  shinydashboard::box(
    height = "100%",
    width = 12,
    title = "Descriptive Statistics for Continuous Variables",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    
    # =====================================================================
    # SIDEBAR PANEL - Controls and Options
    # =====================================================================
    sidebarPanel(
      
      # Help button and modal
      shinyWidgets::actionBttn(
        "helpuni",
        "Help",
        icon = icon("question-circle"),
        style = "stretch",
        block = FALSE,
        color = "primary"
      ),
      
      # Help modal with documentation
      shinyBS::bsModal(
        id = "modalHelpUni",
        title = "Help: Continuous Variables (Univariate)",
        trigger = "helpuni",
        size = "large",
        tags$iframe(
          src = "continuous_variables_univariate.html", 
          width = "100%", 
          height = "600px", 
          frameborder = 0
        )
      ),
      
      # Variable selection
      selectizeInput(
        'selvarcuanti',
        'Select Quantitative Variable',
        choices = NULL,
        multiple = FALSE
      ),
      
      # Group analysis option
      shinyWidgets::prettyCheckbox(
        inputId = "Grupo",
        label = "Statistics by group?",
        value = FALSE,
        status = "success"
      ),
      
      # Group variable selection (conditional)
      conditionalPanel(
        condition = "input.Grupo == 1",
        selectizeInput(
          'selgrupo',
          'Group by (max 2 variables)',
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 2)
        )
      )
    ),
    
    # =====================================================================
    # MAIN PANEL - Content Tabs
    # =====================================================================
    mainPanel(
      
      # Tab navigation
      tabsetPanel(
        type = "tabs",
        
        # Numerical summary tab
        numerical_summary,
        
        # Tabular summary tab
        tabular_summary,
        
        # Graphical summary tab
        graphical_summary
      )
    )
  )
)
