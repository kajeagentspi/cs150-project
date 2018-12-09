options(scipen = 999)
source("server/scripts/helper.r")

polyreg = function(xyDataFrame, degree){
  augcoeff = generateAugCoeffMatrix(xyDataFrame["x"], xyDataFrame["y"], degree)
  result = gaussJordan(augcoeff$matrix, augcoeff$variables)
  fxstring = generateFxString(result$solutionSet, degree)
  if(length(xyDataFrame["x"]) != length(xyDataFrame["y"])) return(NA)
  if(degree > nrow(xyDataFrame)) return(NA)
  
  return(fxstring)
}

# x = c(0, 10, 15, 20, 22.5, 30)
# y = c(0, 227.04, 362.78, 517.35, 602.97, 901.67)
# xyDataFrame = data.frame("x" = x, "y" = y)
# print(polyreg(xyDataFrame, 3))
