 Performs two curve fits, quadratic and n lg n, with a plot of the data and the two curves
# First parameter: A data frame
# Second parameter: Name of the independent (x) variable
# Third parameter: Name of the dependent (y) variable
# Fourth parameter: The label for the x-axis
# Fifth parameter: The label for the y-axis
dp4dsFit <- function(dataFrame, indepVarName, depVarName, xLabel, yLabel) {
  library(ggplot2)
  library(labeling)
  dp4dsQuadraticFit <- lm(dataFrame[,depVarName] ~ poly(dataFrame[,indepVarName],2))
  write("=============\r",file="")
  write("Quadratic fit\r",file="")
  write("=============\r",file="")
  print(summary(dp4dsQuadraticFit))
  dp4dsNlogNFit <- lm(dataFrame[,depVarName] ~ dataFrame[,indepVarName]:log(dataFrame[,indepVarName]) + dataFrame[,indepVarName])
  write("==========\r",file="")
  write("n lg n fit\r",file="")
  write("==========\r",file="")
  print(summary(dp4dsNlogNFit))
  ggplot() +
    geom_point(data = dataFrame, aes_string(x = indepVarName, y = depVarName), size = 3) +
    geom_smooth(data = dataFrame, aes_string(x = indepVarName, y = depVarName),
                method = "lm", se = FALSE, colour = "RED", formula = y ~ poly(x,2)) +
    geom_smooth(data = dataFrame, aes_string(x = indepVarName, y = depVarName),
                method = "lm", se = FALSE, colour = "BLUE", formula = y ~ x:log(x) + x) +
    xlab(label = xLabel) +
    ylab(label = yLabel)
}

# Plots a family of raw data in a line plot, assuming the first column is the independent (x) variable,
# and the remaining columns are the dependent (y) variables.
# First parameter: A data frame.
# Second parameter: The label for the x-axis.
# Third parameter: The label for the y-axis.
# Fourth parameter: The label for the legend.
# Fifth parameter: A vector of labels of the dependent variables for the legend.
dp4dsRawPlot <- function(dataFrame, xLabel, yLabel, legendLabel, dependentLabels) {
  library(ggplot2)
  library(reshape2)
  library(labeling)
  longData <<- melt(dataFrame, id=colnames(dataFrame)[1])
  ggplot() +
    geom_point(data = longData, aes_string(x = colnames(longData)[1], y = "value", colour = "variable")) +
    geom_line(data = longData, aes_string(x = colnames(longData)[1], y = "value", colour = "variable")) +
    xlab(label = xLabel) +
    ylab(label = yLabel) +
    scale_colour_brewer(guide = guide_legend(),
                        name = legendLabel,
                        breaks = colnames(dataFrame[2:ncol(dataFrame)]),
                        labels = dependentLabels,
                        palette = 'Dark2')
}
