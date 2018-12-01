library(shinydashboard)
source("ui/qsi.r")
source("ui/polyreg.r")
source("ui/simplex.r")
source("ui/help.r")

ui = dashboardPage(
  dashboardHeader(title = "CS150 Solver"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quadratic Spline Interpolation",tabName = "qsi"),
      menuItem("Polynomial Regression",tabName = "polyreg"),
      menuItem("Simplex",tabName = "simplex"),
      menuItem("Help",tabName = "help")
    )
  ),
  dashboardBody(
    tabItems(
      qsi(),
      polyreg(),
      simplex(),
      help()
    )
  )
)

