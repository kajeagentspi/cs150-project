library(shiny)
library(shinydashboard)

qsi = function(){
  tabItem(tabName = "qsi",
          fluidPage(
            titlePanel("Quadratic Spline Interpolation"),
            sidebarLayout(
              sidebarPanel(
                fileInput("qsiFile", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                )
              ),
              mainPanel(
                tableOutput("qsiInput"),
                tableOutput("qsiOutput")
              )
              
            )
          )
  )
}
  