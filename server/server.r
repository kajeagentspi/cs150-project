library(shiny)
library(shinydashboard)
source("server/scripts/qsi.r")
source("server/scripts/polyreg.r")
server = function(input, output) {
  output$qsiInput = renderTable({
    req(input$qsiFile)
    tryCatch(
      {
        xyFrame = read.csv(input$qsiFile$datapath,
                       header = FALSE,
                       sep = ",")
        colnames(xyFrame) = c("x", "y")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    output$qsiOutput = renderTable({ return(qsi(xyFrame)) })
    return(xyFrame)
  })
  output$polyregInput = renderTable({
    req(input$polyregFile)
    tryCatch(
      {
        xyFrame = read.csv(input$polyregFile$datapath,
                           header = FALSE,
                           sep = ",")
        print("Fdfd")
        colnames(xyFrame) = c("x", "y")
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    output$polyregOutput = renderText({ 
      req(input$polyregDegree)
      return(polyreg(xyFrame, input$polyregDegree)) 
      })
    return(xyFrame)
  })
}