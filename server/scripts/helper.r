gaussJordan = function(matrix, variables){
  len = length(matrix[1,])
  for(currentCol in 1:(len - 1)){
    sorting = order(abs(tail(matrix[,currentCol], len-currentCol)), decreasing = TRUE)
    sorting = sapply(sorting, function(x) x + (currentCol - 1))
    sorting = c(0:(currentCol - 1), c(sorting))[-1]
    matrix = matrix[sorting,]
    currentRow = currentCol
    divisor = matrix[currentRow, currentCol]
    if(divisor == 0){
      return (NA)
    }
    for(i in currentCol:len){
      matrix[currentRow, i] = matrix[currentRow, i] / divisor
    }
    for(currentRow in 1:(len - 1)){
      if(currentCol == currentRow){
        next
      }else{
        multiplier = matrix[currentRow, currentCol]
        matrix[currentRow, currentCol] = 0
        for(i in (currentCol + 1):len){
          matrix[currentRow, i] = matrix[currentRow, i] - matrix[currentCol, i] * multiplier
        }
      }
    }
  }
  solutionSet = gaussJordanSolutionSet(variables, matrix)
  return(list(solutionSet = solutionSet, variables = variables, matrix = matrix))
}

gaussJordanSolutionSet = function(variables, matrix){
  rowCount = length(matrix[,1])
  #placeholder
  solutionSet = c()
  for(i in 1:rowCount){
    #RHS
    solutionSet = append(solutionSet, matrix[i, rowCount + 1])
  }
  return(solutionSet)
}

# Polynomial Regression
generateAugCoeffMatrix=function(independentVar, dependentVar, degree){
  augcoeff = matrix(NaN, nrow = degree + 1, ncol = degree + 2)
  for (i in 1:(degree + 1)){
    if(i == 1){
      for (j in 1:(degree + 1)){
        augcoeff[i, j]=sum(dependentVar ^ (j - 1))
      }    
    }else{
      augcoeff[i,][1:degree] = augcoeff[i - 1,][2:(degree + 1)]
      augcoeff[i,][degree + 1] = sum(dependentVar ^ (degree + i - 1))
    }
    augcoeff[i, degree + 2]=sum((dependentVar ^ (i - 1)) * independentVar)
  }
  variables=paste0("x", as.character(0:(degree + 1)))
  return(list(matrix = augcoeff, variables = variables))
}

generateFxString = function(solutionset,degree){
  fxstring ="function (x) "
  for(i in (degree+1):1){
    fxstring=paste(fxstring,solutionset[i]," * x ^ ",i-1," + ",sep="")
  }
  fxstring=sub(" \\+ $", "", fxstring)
  return (fxstring)
}