appDir <- system.file("StatisticTeach1App", package = "StatisticTeach1")

################################# Header #######################################
source(file.path(appDir, "R/header.R"))
#####################################Header Fin ################################
#######################################Sidebar #################################
source(file.path(appDir, "R/sidebar.R"))
source(file.path(appDir, "pages/tab0_about.R"), local = TRUE)
source(file.path(appDir, "pages/prob_dist/discrete_probability_distributions.R"), local = TRUE)
source(file.path(appDir, "pages/prob_dist/continuous_probability_distributions.r"), local = TRUE)
source(file.path(appDir, "pages/descriptive_statistics/load_data.r"), local=TRUE)
source(file.path(appDir, "pages/descriptive_statistics/cont_univ/main.r"),local=TRUE)
source(file.path(appDir, "pages/descriptive_statistics/qual_vars/main.r"), local = TRUE)
source(file.path(appDir, "pages/descriptive_statistics/cont_biv/main.r"), local = TRUE)
source(file.path(appDir, "pages/descriptive_statistics/multivariate.r"), local = TRUE)
##############<#######################Sidebar Fin ###############################
####################################Body #######################################
body <- shinydashboard::dashboardBody(
  height = "auto",
  width = "100p%",
 tags$head(
  tags$style(HTML(
  '/* logo when hovered */
  .skin-blue .main-header .logo:hover {
  background-color: #3991e3;}
  /* navbar (rest of the header) */
  .skin-blue .main-header .navbar {
  background-color: #3991e3;}
  /* main sidebar */
  .skin-blue .main-sidebar {
  background-color: #3991e3;}
  /* active selected tab in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  background-color: #0091ff;}
  /* other links in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
  background-color: #ccdceb;
  color: #000000;}
  /* other links in the sidebarmenu when hovered */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
  background-color: #69c3ff;}
  /* toggle button when hovered  */
  .skin-blue .main-header .navbar .sidebar-toggle:hover{
  background-color: #9bc2e8;}
  /* body */
  .content-wrapper, .right-side {
  background-color: #7da2d1;}')
  )
  ),
 shinydashboard::tabItems(
  tab0_about,
  discrete_probability_distributions,
  continuous_probability_distribution,
   load_dataPage,
  continuous_variables_univariate,
  qualitative_variables,
  continuous_variables_bivariate,
  multivariate
  )#fintabItems
)
####################################Body Fin #################################
shinydashboard::dashboardPage(header, sidebar, body)
