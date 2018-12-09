library(shiny)
library(shinydashboard)

polyreg = function(){
  tabItem(tabName = "polyreg",
          fluidPage(
            titlePanel("Polynomial Regression"),
            sidebarLayout(
              sidebarPanel(
                fileInput("polyregFile", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
                numericInput("polyregDegree", "Degree", 1, min = 1),
                numericInput("polyregEstimate", "Value to Estimate", 1, min = 1)
              ),
              mainPanel(
                fluidRow(
                  column(2, tableOutput("polyregInput")),
                  column(8, textOutput("polyregOutput"))
                ),
                fluidRow(
                  plotOutput("polyregPlot")
                ),
                fluidRow(
                  textOutput("polyregEstimateOutput")
                )
              )
              
            )
          )
  )
}
