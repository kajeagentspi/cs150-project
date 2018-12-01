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
                numericInput("polyregDegree", "Degree", 0)
              ),
              mainPanel(
                tableOutput("polyregInput"),
                textOutput("polyregOutput")
              )
              
            )
          )
  )
}
