
################################# Header #######################################
source("header.R")
#####################################Header Fin ################################
#######################################Sidebar #################################
source("sidebar.R")
source("pages/tab0_about.R", local = TRUE)
source("pages/discrete_prob_distributions.R", local = TRUE)
source("pages/continuous_prob_distributions.R", local = TRUE)
source("pages/load_data.R", local=TRUE)
source("pages/univariate_main.R",local=TRUE)
source("pages/qualitative_main.R", local = TRUE)
source("pages/bivariate_main.R", local = TRUE)
source("pages/multivariate.R", local = TRUE)
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
