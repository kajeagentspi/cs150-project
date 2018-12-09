library(shiny)
library(shinydashboard)
library(rhandsontable)
source("server/scripts/qsi.r")
source("server/scripts/polyreg.r")
source("server/scripts/simplex.r")

plants = c("Denver", "Phoenix", "Dalas", "" ,"Shipping")
supply = c(310, 260, 280, "--", 0)
sacramento = c(10, 6, 3, NA, 0)
saltLakeCity = c(8, 5, 4, NA, 0)
albuquerque = c(6, 4, 5, NA, 0)
chicago = c(5, 3, 5, NA, 0)
newYorkCity = c(4, 6, 9, NA, 0)
shippingCost = data.frame(
                "Supply" = supply,
                "Sacramento" = sacramento,
                "SaltLake" = saltLakeCity,
                "Albuquerque" = albuquerque,
                "Chicago" = chicago,
                "NewYork" = newYorkCity,
                stringsAsFactors = FALSE)

rownames(shippingCost) = plants

plants = c("Denver", "Phoenix", "Dalas", "", "Total", "--", "Demand")
sacramento = c(0, 0, 0, NA, 0, NA, 180)
saltLakeCity = c(0, 0, 0, NA, 0, NA, 80)
albuquerque = c(0, 0, 0, NA, 0, NA, 200)
chicago = c(0, 0, 0, NA, 0, NA, 160)
newYorkCity = c(0, 0, 0, NA, 0, NA, 220)
total = c(0, 0, 0, NA, NA, "--", NA)

computation = data.frame(
  "TotalShipped" = total,
  "Sacramento" = sacramento,
  "SaltLake" = saltLakeCity,
  "Albuquerque" = albuquerque,
  "Chicago" = chicago,
  "NewYork" = newYorkCity,
  stringsAsFactors = FALSE
)

rownames(computation) = plants

server = function(input, output) {
  values = reactiveValues(computation = computation, shippingCost = shippingCost)

  output$qsiInput = renderTable({
    req(input$qsiFile)
    tryCatch(
      {
        xyFrame = read.csv(input$qsiFile$datapath,
                       header = FALSE,
                       sep = ",")
        colnames(xyFrame) = c("x", "y")

        xyFrame = xyFrame[order(xyFrame["x"]),]
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    result = qsi(xyFrame)
    output$qsiOutput = renderTable({ return(result) })
    sorting = order(xyFrame[,"x"])
    sortedX = xyFrame[,"x"][sorting]
    minX = sortedX[1]
    maxX = sortedX[length(sortedX)]
    output$qsiPlot = renderPlot({
      plot(xyFrame[,"x"], xyFrame[,"y"], xlab = "x", ylab = "y", xlim = c(minX, maxX))
      for (row in 1:nrow(result)) {
        fx = eval(parse(text = result[row, "fx"]))
        lines(result[row, "lowerBound"]:result[row, "upperBound"], fx(result[row, "lowerBound"]:result[row, "upperBound"]), type = "l", col = "red")
      }
    })
    output$qsiEstimateOutput = renderText({
      if(input$qsiEstimate > maxX || input$qsiEstimate < minX){
        return("Out of bounds")
      }else{
        for (row in 1:nrow(result)) {
          if(result[row, "lowerBound"] <= input$qsiEstimate  && input$qsiEstimate <= result[row, "upperBound"]){
            fx = eval(parse(text = result[row, "fx"]))
            return(toString(fx(input$qsiEstimate)))
          }
        }
      }
    })
    return(xyFrame)
  })
  output$polyregInput = renderTable({
    req(input$polyregFile, input$polyregDegree)
    tryCatch(
      {
        xyFrame = read.csv(input$polyregFile$datapath,
                           header = FALSE,
                           sep = ",")
        colnames(xyFrame) = c("x", "y")
        xyFrame = xyFrame[order(xyFrame["x"]),]
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    fxstring = polyreg(xyFrame, input$polyregDegree)
    output$polyregOutput = renderText({
      if(input$polyregDegree >= length(xyFrame[,"x"])){
        return(NA)
      }
      return(fxstring)
      })
    sorting = order(xyFrame[,"x"])
    sortedX = xyFrame[,"x"][sorting]
    minX = sortedX[1]
    maxX = sortedX[length(sortedX)]
    fx = eval(parse(text = fxstring))
    output$polyregPlot = renderPlot({
      if(input$polyregDegree >= length(xyFrame[,"x"])){
        return(NA)
      }
      plot(xyFrame[,"x"], xyFrame[,"y"], xlab = "x", ylab = "y", xlim = c(minX, maxX), ylim = c(fx(minX), fx(maxX)))
      lines(minX:maxX,fx(minX:maxX), type = "l", col = "red")
    })
    output$polyregEstimateOutput = renderText({
      if(input$polyregDegree >= length(xyFrame[,"x"])){
        return(NA)
      }
      if(input$polyregEstimate > maxX || input$polyregEstimate < minX){
        return("Out of bounds")
      }else{
        return(toString(fx(input$polyregEstimate)))
      }
    })
    return(xyFrame)
  })

  observe({
    if(!is.null(input$shippingCost))
      values$shippingCost = hot_to_r(input$shippingCost)
  })

  observe({
    if(!is.null(input$computation)){
      values$computation = hot_to_r(input$computation)
    }
  })

  output$computation = renderRHandsontable({
    values$computation[1, "TotalShipped"] = sum(values$computation[1, 2:6])
    values$computation[2, "TotalShipped"] = sum(values$computation[2, 2:6])
    values$computation[3, "TotalShipped"] = sum(values$computation[3, 2:6])
    values$computation["Total", "Sacramento"] = sum(values$computation[1:3, "Sacramento"])
    values$computation["Total", "SaltLake"] = sum(values$computation[1:3, "SaltLake"])
    values$computation["Total", "Albuquerque"] = sum(values$computation[1:3, "Albuquerque"])
    values$computation["Total", "Chicago"] = sum(values$computation[1:3, "Chicago"])
    values$computation["Total", "NewYork"] = sum(values$computation[1:3, "NewYork"])
    rhandsontable(values$computation, width = 600) %>%
    hot_row(1:6, readOnly = TRUE)
  })

  output$shippingCost = renderRHandsontable({
    vec = t(as.matrix(values$shippingCost[1:3, 2:6]))
    z = as.vector(vec)
    demand = unlist(values$computation["Demand", 2:6], use.names = FALSE)
    supply = as.numeric(unlist(values$shippingCost[1:3,"Supply"], use.names = FALSE))
    demand = demand * -1
    rhs = c(demand, supply)
    simplexOut = calculateSimplex(z, rhs)
    result = matrix(simplexOut$results, ncol = 5, nrow = 3, byrow = TRUE)
    values$computation[1:3, 2:6] = result[1:3,]
    values$shippingCost["Shipping", "Sacramento"] = values$shippingCost[1:3, "Sacramento"] %*% values$computation[1:3, "Sacramento"]
    values$shippingCost["Shipping", "SaltLake"] = values$shippingCost[1:3, "SaltLake"] %*% values$computation[1:3, "SaltLake"]
    values$shippingCost["Shipping", "Albuquerque"] = values$shippingCost[1:3, "Albuquerque"] %*% values$computation[1:3, "Albuquerque"]
    values$shippingCost["Shipping", "Chicago"] = values$shippingCost[1:3, "Chicago"] %*% values$computation[1:3, "Chicago"]
    values$shippingCost["Shipping", "NewYork"] = values$shippingCost[1:3, "NewYork"] %*% values$computation[1:3, "NewYork"]
    values$shippingCost["Shipping", "Supply"] = sum(values$shippingCost[5, 2:6])
    rhandsontable(values$shippingCost, width = 600) %>%
      hot_row(c(4, 5), readOnly = TRUE)
  })
}