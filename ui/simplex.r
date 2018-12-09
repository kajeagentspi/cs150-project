library(shinydashboard)
library(rhandsontable)
simplex <- function(){
  tabItem(tabName = "simplex",
          fluidPage(
            titlePanel("Simplex"),
            sidebarLayout(
              sidebarPanel = NULL,
              mainPanel = mainPanel(
                fluidRow(
                  rHandsontableOutput("computation")  
                ),
                fluidRow(
                  rHandsontableOutput("shippingCost")
                ),
                fluidRow(
                  selectInput("tableau", "Select Iteration", c("HIDE"))
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.tableau != 'HIDE'",
                    verbatimTextOutput("iterationTable")  
                  )
                  
                )
              )
            )
          )
  )
}
