library(shinydashboard)
source("ui/qsi.r")
source("ui/polyreg.r")
source("ui/simplex.r")

ui = dashboardPage(
  dashboardHeader(title = "CMSC150 Solver"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quadratic Spline Interpolation",tabName = "qsi"),
      menuItem("Polynomial Regression",tabName = "polyreg"),
      menuItem("Simplex",tabName = "simplex")
    )
  ),
  dashboardBody(
    tabItems(
      qsi(),
      polyreg(),
      simplex()
    )
  )
)

