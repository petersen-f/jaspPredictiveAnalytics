#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Main function ----

multiVarControl <- function(jaspResults, dataset, options) {
  ready <- length(options[["variables"]]) > 0

  if(ready){
    dataset <- .mVarConReadData(dataset,options)
    #.mVarConCheckErrors(dataset,options)
  }
  .mVarContContainerHelper(jaspResults,ready,dataset,options)
  .mVarContSummaryTable(jaspResults,ready,dataset,options)
  .mVarContSummaryPlot(jaspResults,ready,dataset,options)

  .mVarContMultiBinomialHelper(jaspResults,ready,dataset,options)
}

.mVarConReadData <- function(dataset,options){
  #if(!is.null(dataset))
   # return(dataset)

  vars <- unlist(options$variables)
  dataset <- .readDataSetToEnd(columns.as.numeric = vars)

  dataset$time <- 1:nrow(dataset)

  dataset <- tail(dataset,options$previousDataPoints)
  return(dataset)
}

.mVarConCheckErrors <- function(dataset,options){
  if(!is.data.frame(dataset) || any(colnames(dataset) %in% c("X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25")))
    stop(gettext("Column input is wrong"))
  return()
}

.mVarContContainerHelper <- function(jaspResults,ready,dataset,options){
  if(!ready) return()

  if(is.null(jaspResults[["mVarContMainContainer"]])){
    mVarContMainContainer <- createJaspContainer(position = 1)
    jaspResults[["mVarContMainContainer"]] <- mVarContMainContainer
  }
 return()
}

##### Helper functions

.computeBoundsHelper <- function(data,options){

    controlBounds <- data.frame( mean = c(66,8,2.5,13.15,13.15,13.15,13.15,7,7),
                               bound= c(.43,.46,.46,.16,.16,.18,.18,.33,.33))


    rownames(controlBounds) <- encodeColNames(c("X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25"))

    cols <- intersect(rownames(controlBounds),colnames(data))

    dataCoded <- as.data.frame(sapply(X=cols, function(x) {
      as.numeric(data[x] < controlBounds[x,"mean"] - controlBounds[x,"bound"] |
                   data[x] > controlBounds[x,"mean"] + controlBounds[x,"bound"] )
    },USE.NAMES = T))
    colnames(dataCoded) <- cols


  return(dataCoded)
}




# aggregate binomial data by a window
.aggregateBinomialData <- function(data,dataWindow){
  data$all <- rowSums(data)
  cols <- ncol(data) - 1 # subtract all column
  nS <- round(nrow(data)/dataWindow,0)
  x = 1:nrow(data)
  s <- split(x, sort(x%%nS))
  dataCombined <- as.data.frame(t(sapply(X = 1:nS,function(x) colSums(data[s[[x]],]))))
  dataCombined$u <- lengths(s)*cols
  return(dataCombined)
}


.mVarContSummaryTable <- function(jaspResults,ready,dataset,options){
  if(!is.null(jaspResults[["mVarContMainContainer"]][["overallSummaryTable"]]) || !ready) return()


  overallSummaryTable <- createJaspTable(title = "Control Summary Table")

  overallSummaryTable$dependOn(c("variables","overallControlSummaryTable","transposeOverallTable","orderTableByOutBound"))

  overallSummaryTable$addColumnInfo(name = "variable", title = "Variable", type= "string")
  overallSummaryTable$addColumnInfo(name = "nData", title = "Valid", type = "integer")
  overallSummaryTable$addColumnInfo(name = "missing", title = "Missing", type = "integer")
  overallSummaryTable$addColumnInfo(name = "outBoundNum", title = "Out-of-bound - Number", type = "integer")
  overallSummaryTable$addColumnInfo(name = "outBoundPerc", title = "Out-of-bound - Percent", type = "number", format = "dp:3")

  if(options$transposeOverallTable)
    overallSummaryTable$transpose  <- T

  if(ready && options$overallControlSummaryTable)
    overallSummaryTable <- .mVarContSummaryTableFill(overallSummaryTable,dataset,options)

  jaspResults[["mVarContMainContainer"]][["overallSummaryTable"]] <- overallSummaryTable
  return()
}


.mVarContSummaryTableFill <- function(overallSummaryTable,dataset,options,ready){


  dataCoded <- .computeBoundsHelper(dataset,options)


  #View(dataCoded)
  dataRes <- data.frame()


  dataResAll <- data.frame(
    variable = "All",
    nData = sum(!is.na(dataCoded),na.rm = T),
    missing = sum(is.na(dataCoded),na.rm = T),
    outBoundNum = sum(dataCoded,na.rm = T),
    outBoundPerc = sum(dataCoded,na.rm = T)/sum(!is.na(dataCoded),na.rm = T)
  )


 # stop(gettext(paste0("Models didn't work. Instead of prediction matrix we got:",one_step_pred)))


  for(var in colnames(dataCoded)){
    dataRes <- rbind(dataRes,
      c(variable = var,
           nData = sum(!is.na(dataCoded[var]),na.rm = T),
           missing = sum(is.na(dataCoded[var]),na.rm = T),
           outBoundNum = sum(dataCoded[var],na.rm = T),
           outBoundPerc = sum(dataCoded[var],na.rm = T)/sum(!is.na(dataCoded[var]),na.rm = T)
      )
    )
  }
  colnames(dataRes) <- c("variable",
                         "nData",
                         "missing",
                         "outBoundNum",
                         "outBoundPerc")

  if(options$orderTableByOutBound)
    dataRes <- dataRes[order(dataRes$outBoundPerc,decreasing = T),]


  dataRes <- rbind(dataResAll,dataRes)

  dataRes[-1] <- apply(dataRes[-1],2,as.numeric)

  for(col in colnames(dataRes)){
    overallSummaryTable$addColumns( col = dataRes[col])
  }
  print("table created")

  return(overallSummaryTable)
}


.mVarContSummaryPlot <- function(jaspResults,ready,dataset,options){
  if(!is.null(jaspResults[["mVarContMainContainer"]][["overallSummaryPlotContainer"]]) || !ready) return()

  overallSummaryPlotContainer <- createJaspContainer(title = "Control Plots")

  overallSummaryPlotContainer$dependOn(c("outBoundOverallPlotCheck","outBoundOverallPlotMetricChoice","outBoundOverallPlotLineType"))

  if(options$outBoundOverallPlotCheck)
    .mVarContSummaryPlotFill(overallSummaryPlotContainer,dataset,options)

  #if(options$summaryPlotIndividualVars)
  #  .mVarContSummaryPlotFillSingle(overallSummaryPlotContainer,dataset,options)

  jaspResults[["mVarContMainContainer"]][["overallSummaryPlotContainer"]] <- overallSummaryPlotContainer

  return()

}


.mVarContSummaryPlotFill <- function(overallSummaryPlotContainer,dataset,options){

  overallPlot <- createJaspPlot("Overall Control Plot", height = 480, width = 720)


  dataCoded <- .computeBoundsHelper(dataset,options)
  #stop(gettext(dim(dataCoded)))
  dataCoded$all <- rowSums(dataCoded,na.rm = T)
  if(options$outBoundOverallPlotMetricChoice == "percent"){
    dataCoded$all <- dataCoded$all/length(options$variables)
    yTitle <- "Percent"
    yLim <- c(0,1)
  } else {
    yTitle <- "Number"
    yLim <- c(0,length(options$variables))
  }
  dataCoded$time <- dataset$time

  xBreaks <- pretty(dataCoded$time)
  yBreaks <- pretty(yLim)


  p <- ggplot2::ggplot(data = dataCoded, ggplot2::aes(y = all,x = time)) + #ggplot2::geom_point() +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = c(min(dataCoded$time),max(dataCoded$time))) +
    ggplot2::ylab(yTitle) +
    ggplot2::scale_y_continuous("Errors",breaks = yBreaks,limits = yLim) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

  if(options$outBoundOverallPlotLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=0.7)
  if(options$outBoundOverallPlotLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=0.5)

  overallPlot$plotObject <- p

  overallSummaryPlotContainer[["overallPlot"]] <- overallPlot

  return()
}

.mVarContSummaryPlotFillSingle <- function(overallSummaryPlotContainer,dataset,options){
  #placeholder for individual plots
  return()
}


.mVarContMultiBinomialHelper <- function(jaspResults,ready,dataset,options){
  if(!is.null(jaspResults[["mVarContMainContainer"]][["multiVarBinomialContainer"]]) || !ready) return()

  multiVarBinomialContainer <- createJaspContainer("Binomial Control")

  multiVarBinomialContainer$dependOn(c("multiBinWindow","multiBinaryCheckPlot","multiBinomDraws"))
  dataCoded <- .computeBoundsHelper(dataset,options)

  dataAggregated <- .aggregateBinomialData(dataCoded,options$multiBinWindow)


  .multiVarBinResultsHelper(jaspResults,dataAggregated,options)
  if(options$multiBinaryCheckPlot)
    .mVarContMultiBinomialPlot(jaspResults,dataAggregated,options,multiVarBinomialContainer)

  jaspResults[["mVarContMainContainer"]][["multiVarBinomialContainer"]] <- multiVarBinomialContainer

  return()
}

.multiVarBinResultsHelper <-function(jaspResults,dataAggregated,options){
  multiVarBinomialResults <- createJaspState()

  mod <- bssm::bsm_ng(y=dataAggregated$all,
                sd_level = bssm::halfnormal(0.5,2),
                sd_slope = bssm::halfnormal(0.5,2),
                u=dataAggregated$u,
                distribution = "binomial")

  startProgressbar(1,label = "Running binomial state space model")
  sample <- bssm::run_mcmc(mod, iter = options$multiBinomDraws,mcmc_type = "approx")
  progressbarTick()


  multiVarBinomialResults$object <- list(model = mod,sample=sample,dat = dataAggregated)
  jaspResults[["multiVarBinomialResults"]] <- multiVarBinomialResults
  return()
}




.mVarContMultiBinomialPlot <- function(jaspResults,dataAggregated,options,multiVarBinomialContainer){

  titleSub <- ifelse(options$multiBinWindow > 1,paste(" - summary window:",options$multiBinWindow),"")
  multiBinomialPlot <- createJaspPlot(title =paste0("Estimated mutivariate proportion",titleSub), height = 480, width = 720)


  predBssm <-  jaspResults[["multiVarBinomialResults"]]$object$sample


  predBssm <- subset(as.data.frame(predBssm,variable = "states"),variable == "level")
  predBssm$value <- plogis(predBssm$value)
  binomialSummary <- do.call(data.frame,
                             aggregate( value ~ time,
                                        data = predBssm,
                                        FUN = function(x) c(mean = mean(x),
                                                            lowerCI = quantile(x,probs= 0.025),
                                                            higherCI= quantile(x,probs= 0.975))
                             ))
  colnames(binomialSummary) <- c("time","mean","lowerCI","higherCI")

  binomialSummary <- binomialSummary[1:nrow(binomialSummary)-1,]
  binomialSummary$actual <- dataAggregated$all/dataAggregated$u


  xBreaks <- pretty(binomialSummary$time)
  yBreaks <- pretty(c(0,1))
  p <- ggplot2::ggplot(binomialSummary,ggplot2::aes(x = time, y = mean)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(y=actual),size=0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lowerCI, ymax = higherCI),
                fill = "blue", alpha = 0.25) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = c(min(binomialSummary$time),max(binomialSummary$time))) +
    #ggplot2::ylab(yTitle) +
    ggplot2::scale_y_continuous("Proportion",breaks = yBreaks,limits = c(0,1)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

  multiBinomialPlot$plotObject <- p

  multiVarBinomialContainer[["multiBinomialPlot"]] <- multiBinomialPlot

  return()
}






