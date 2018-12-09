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
                ),
                numericInput("qsiEstimate", "Value to Estimate", 1, min = 1)
              ),
              mainPanel(
                fluidRow(
                  column(2, tableOutput("qsiInput")),
                  column(8, tableOutput("qsiOutput"))
                ),
                fluidRow(
                  plotOutput("qsiPlot")
                ),
                fluidRow(
                  textOutput("qsiEstimateOutput")
                )
              )
            )
          )
  )
}
  