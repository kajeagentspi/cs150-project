options(scipen=999)

calculateSimplex = function(z, rhs){
  rowNames=c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z")
  colNames=c("denverSacramento", "denverSaltLake", "denverAlbuquerque", "denverChicago", "denverNewYork", "phoneixSacramento", "phoneixSaltLake", "phoneixAlbuquerque", "phoneixChicago", "phoneixNewYork", "dallasSacramento", "dallasSaltLake", "dallasAlbuquerque", "dallasChicago", "dallasNewYork", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z", "RHS")
  tempVec = c(-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0, 0,	0,	0,	0,	0,	0,	0,	-190,
              0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0, -220,
              0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	-250,
              0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	-100,
              0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	-1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	-120,
              1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	310,
              0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	260,
              0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	280,
              10,	8,	6,	5,	4,	6,	5,	4,	3,	6,	3,	4,	4,	5,	9,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0)
  simplexMatrix = matrix(tempVec, nrow = 9, ncol=25,byrow = TRUE, dimnames = list(rowNames,colNames))
  simplexMatrix[9,] = c(z, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  simplexMatrix[,"RHS"] = c(rhs, 0)
  return(simplex(simplexMatrix))
}

lappend = function (lst, ...){
  return(c(lst, list(...)))
}

endCondition = function(vectorX){
  return(all(sapply(vectorX,function(x) x >= 0, simplify = TRUE)))
}

simplex=function(simplexMatrix){
  colCount = ncol(simplexMatrix)
  rowCount = nrow(simplexMatrix)
  iterations = list()
  iterations = lappend(iterations, simplexMatrix)
  while (!endCondition(simplexMatrix[1:(rowCount - 1), colCount])) {
    pivotRow = which.min(simplexMatrix[1:(rowCount - 1), colCount])
    pivotColumn = which.max(simplexMatrix[pivotRow,1:(colCount-1)] / simplexMatrix[pivotRow, colCount])
    simplexMatrix[pivotRow,] = simplexMatrix[pivotRow,] / simplexMatrix[pivotRow, pivotColumn]
    for (row in 1:rowCount){
      if(row==pivotRow) next
      temp=simplexMatrix[row,pivotColumn]*simplexMatrix[pivotRow,]
      simplexMatrix[row,]=simplexMatrix[row,] - temp
    }
    iterations = lappend(iterations, simplexMatrix)
  }
  
  while(!endCondition(simplexMatrix[nrow(simplexMatrix),1:(ncol(simplexMatrix)-1)])){
    lastrow=simplexMatrix[nrow(simplexMatrix),1:(ncol(simplexMatrix)-1)]
    # get smallest number or largest negative
    pivotColumn=colnames(simplexMatrix)[which(lastrow==min(lastrow))]
    
    temp=head(simplexMatrix[,"RHS"],-1)/head(simplexMatrix[,pivotColumn],-1)
    # get min positive
    pivotRow = which(temp==min(temp[temp>0]))
    
    pivotElement=simplexMatrix[pivotRow,pivotColumn]
    # gauss
    
    simplexMatrix[pivotRow,]=simplexMatrix[pivotRow,]/pivotElement
    for (row in 1:rowCount){
      if(row==pivotRow) next
      temp=simplexMatrix[row,pivotColumn]*simplexMatrix[pivotRow,]
      simplexMatrix[row,]=simplexMatrix[row,]-temp
      
    }
    iterations = lappend(iterations, simplexMatrix)
  }
  results = getResults(simplexMatrix)
  answer = simplexMatrix[rowCount, colCount] * -1
  return(list(results = results, answer = answer, iterations = iterations))
}

getResults = function(simplexMatrix){
  answers=c();
  for (col in 1:15) {
    indexes = which(simplexMatrix[1:8,col]==1)
    if(length(indexes)==1){
      answers = c(answers, simplexMatrix[indexes[1], "RHS"])
    }else{
      answers = c(answers, 0)
    }
  }
  return(answers)
}