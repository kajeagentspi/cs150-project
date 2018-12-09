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
                )
              )
            )
          )
  )
}
