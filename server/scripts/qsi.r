options(scipen = 999)
source("server/scripts/helper.r")
qsi = function(xyDataFrame){
  len = nrow(xyDataFrame)
  numberOfFunctions = len - 1
  lowerBounds = xyDataFrame[1:numberOfFunctions, 1]
  upperBounds = xyDataFrame[2:len, 1]
  bounds = data.frame("lowerBounds" = lowerBounds,"upperBounds" = upperBounds)
  rowNames = c()
  for (i in 1:(numberOfFunctions * 3)) {
    rowNames = append(rowNames, as.character(i))
  }
  colNames = c()
  for (iterator in 1:numberOfFunctions) {
    colNames = append(colNames, paste("a", as.character(iterator), sep = ""))
    colNames = append(colNames, paste("b", as.character(iterator), sep = ""))
    colNames = append(colNames, paste("c", as.character(iterator), sep = ""))
  }
  colNames = append(colNames, "RHS")
  equationMatrix = matrix(data = 0, nrow = numberOfFunctions * 3, ncol = (numberOfFunctions * 3) + 1, dimnames = list(rowNames, colNames))
  
  currentLine = 1
  for (iterator in 1:numberOfFunctions) {
    equationMatrix[currentLine, paste("a", as.character(iterator), sep = "")] = bounds[iterator, "lowerBounds"] ^ 2
    equationMatrix[currentLine, paste("b", as.character(iterator), sep = "")] = bounds[iterator, "lowerBounds"]
    equationMatrix[currentLine, paste("c", as.character(iterator), sep = "")] = 1
    equationMatrix[currentLine, "RHS"] = xyDataFrame[iterator, "y"]
    currentLine = currentLine + 1
    equationMatrix[currentLine, paste("a", as.character(iterator), sep = "")] = bounds[iterator, "upperBounds"] ^ 2
    equationMatrix[currentLine, paste("b", as.character(iterator), sep = "")] = bounds[iterator, "upperBounds"]
    equationMatrix[currentLine, paste("c", as.character(iterator), sep = "")] = 1
    equationMatrix[currentLine, "RHS"] = xyDataFrame[iterator + 1, "y"]
    currentLine = currentLine + 1
  }
  iterator = 1
  for (bound in bounds[2:numberOfFunctions, "lowerBounds"]) {
    equationMatrix[currentLine, paste("a", as.character(iterator), sep = "")] = bound * 2
    equationMatrix[currentLine, paste("b", as.character(iterator), sep = "")] = 1
    equationMatrix[currentLine, paste("a", as.character(iterator + 1), sep = "")] = bound * -2
    equationMatrix[currentLine, paste("b", as.character(iterator + 1), sep = "")] = -1
    iterator = iterator + 1
    currentLine = currentLine + 1
  }
  equationMatrix[currentLine, "a1"] = 1
  print(equationMatrix)
  gaussJordanResult = gaussJordan(equationMatrix, colNames[1:(numberOfFunctions * 3)])
  resultCoefficients = gaussJordanResult$matrix[,"RHS"]
  iterator = 1
  fxList = c()
  for (i in 1:numberOfFunctions) {
    fxstring = "function (x) "
    fxstring = paste(fxstring, resultCoefficients[iterator], " * (x ^ 2) + ", sep = "")
    fxstring = paste(fxstring, resultCoefficients[iterator + 1], " * x + ", sep = "")
    fxstring = paste(fxstring, resultCoefficients[iterator + 2], sep = "")
    fxList = append(fxList, fxstring)
    iterator = iterator + 3
  }
  resultDataFrame = data.frame("fx" = fxList, "lowerBound" = bounds[,"lowerBounds"], "upperBound" = bounds[,"upperBounds"], stringsAsFactors = FALSE)
  return(resultDataFrame)
}


# x = c(0, 10, 15, 20, 22.5, 30)
# y = c(0, 227.04, 362.78, 517.35, 602.97, 901.67)
# x = c(1,2,7)
# y = c(5,11,32)
# xyDataFrame = data.frame("x" = x, "y" = y)
# print(qsi(xyDataFrame))