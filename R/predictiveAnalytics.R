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

predictiveAnalytics <- function(jaspResults, dataset, options) {
    ready <- options$dependent != ""

    dataset <- .predanReadData(options,ready)

    .predanMainContainer(jaspResults, ready)
    .predanComputeResults(jaspResults, dataset, options, ready)
    .predanPlotsDescriptives(jaspResults,dataset,options,ready)
    .predanACFDescriptives(jaspResults,dataset,options,ready)
    .predanHistogramPlot(jaspResults,dataset,options,ready)
    .predanDiagnosticTables(jaspResults, dataset, options,ready)
    .predanComputeBinaryResults(jaspResults,dataset,options,ready)
    .predanBinaryControlChart(jaspResults,dataset,options,ready)
    return()
}

.predanReadData <- function(options,ready) {
  if(!ready) return()
  numericVariable <- c(options$dependent)
  numericVariable <- numericVariable[numericVariable != ""]
  #timeVar <- unlist(options$time)
  #timeVar <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVariable)

  options$dataset <- dataset

  return(dataset)

}

.predanMainContainer <- function(jaspResults, options) {

  if (is.null(jaspResults[["predanMainContainer"]])){
    predanMainContainer <- createJaspContainer()
    jaspResults[["predanMainContainer"]] <- predanMainContainer
    ##TODO: add depenencies
  }



  return()
}


###### Helper Functions
.extractState <- function(model,logit=F){
  state <- model$state.contributions
  burn <- bsts::SuggestBurn(0.1,model)
  state <- state[-(1:burn), , , drop = FALSE]
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)
  if(logit)
    state <- plogis(state)
  res <- data.frame(mean = colMeans(state),
                    lowerCI = apply(state,2,quantile,probs= 0.025),
                    higherCI= apply(state,2,quantile,probs= 0.975),
                    actualData = model$original.series,
                    time = 1:length(model$original.series))
  return(res)
}


##### dependencies
.boundDependencies <- function(){
  return(c("errorBoundMethodDrop",
           "manualBoundMethod",
           "manualBoundMean",
           "manualBoundErrorBound",
           "manualUpperBound",
           "manualUpperLower",
           "sigmaBound",
           "controlPeriodCheck",
           "controlPeriodStart",
           "controlPeriodEnd",
           "trimmedMeanCheck",
           "trimmedMeanPercent"))
}

.predanComputeResults <- function(jaspResults, dataset, options,ready) {

  if(!ready) return()

  if (is.null(jaspResults[["predanResults"]])) {
    predanResults <- createJaspContainer()

    jaspResults[["predanResults"]] <- predanResults
    #jaspResults[["predanResults"]]$dependsOn(c("dependent","time"))

  }
  if (is.null(jaspResults[["predanResults"]][["predanBounds"]])){
    predanBoundsState <- createJaspState()

    #TODO: insert depend on all boundary setting
    predanBounds <- .predanComputeBounds(dataset,options)
    predanBoundsState$object <- predanBounds
    predanBoundsState$dependOn(.boundDependencies())
    jaspResults[["predanResults"]][["predanBounds"]] <- predanBoundsState
  }

}

.predanComputeBounds <- function(dataset,options) {

  dataControl <- data.frame(y = dataset[,options[["dependent"]]])
  dataControl$time <- 1:nrow(dataControl)

  if(options$errorBoundMethodDrop == "manualBound" & options$manualBoundMethod == "manualBoundUniform") {

    upperLimit <- options$manualBoundMean + options$manualBoundErrorBound
    lowerLimit <- options$manualBoundMean - options$manualBoundErrorBound
    plotLimit <- options$manualBoundMean + c(2*options$manualBoundErrorBound,-2*options$manualBoundErrorBound)
  } else if(options$errorBoundMethodDrop == "manualBound" && options$manualBoundMethod == "manualBoundCustom"){

    upperLimit <- options$manualUpperBound
    lowerLimit <- options$manualLowerBound
    boundMean <- (upperLimit + lowerLimit)/2
    plotLimit <- boundMean + c(1,-1)*2*(upperLimit-boundMean)

  } else {

    if (options$controlPeriodCheck & options$controlPeriodEnd > 0){
      controlPeriod <- seq(options$controlPeriodStart,options$controlPeriodEnd,1)
    } else
      controlPeriod <- seq_len(nrow(dataControl))

    trimMean <- ifelse(options$trimmedMeanCheck,options$trimmedMeanPercent,0)
    upperLimit <- mean(dataControl$y[controlPeriod],trim=trimMean) + sd(dataControl$y[controlPeriod],)*options$sigmaBound
    lowerLimit <- mean(dataControl$y[controlPeriod],trim=trimMean) - sd(dataControl$y[controlPeriod])*options$sigmaBound
    plotLimit <- c(mean(dataControl$y[controlPeriod],trim=trimMean) + 2*sd(dataControl$y[controlPeriod])*options$sigmaBound,
                   mean(dataControl$y[controlPeriod],trim=trimMean) - 2*sd(dataControl$y[controlPeriod])*options$sigmaBound)
  }



  dataControl$outBound <- ifelse(dataControl$y > upperLimit | dataControl$y < lowerLimit,T,F)
  dataControl$outBoundNum <- as.numeric(dataControl$outBound)
  dataControl$outBoundArea <- "Inside"
  dataControl$outBoundArea[dataControl$outBound] <- ifelse(dataControl$y[dataControl$outBound] > upperLimit,"Above","Below")

  dataControl$distance[dataControl$outBound] <- ifelse(dataControl$y[dataControl$outBound] > upperLimit,
                                                       dataControl$y[dataControl$outBound] - upperLimit,
                                                       dataControl$y[dataControl$outBound] - lowerLimit)


  results <- list(dataControl = dataControl,
                  upperLimit = upperLimit,
                  lowerLimit = lowerLimit,
                  plotLimit = plotLimit)

  return(results)
}

.predanPlotsDescriptives <- function(jaspResults,dataset,options,ready) {
  if(!ready) return()

  predanDescriptivePlots <- createJaspContainer(title=gettext("Descriptives"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]][["object"]]



  if(options$controlPlotCheck)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivePlots,options,zoom=F)

  if(options$controlPlotZoomCheck && options$zoomPeriodEnd >0)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivePlots,options,zoom=T)


  jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlot <- function(jaspResults,predanResults,predanDescriptivePlots,options,zoom){

  controlData <- predanResults[["dataControl"]]

  upperLimit <- predanResults[["upperLimit"]]
  lowerLimit <- predanResults[["lowerLimit"]]
  plotLimit <- predanResults[["plotLimit"]]

  title <- "Basic Control Plot"
  if(zoom){
    start <- options$zoomPeriodStart
    end <- options$zoomPeriodEnd
    if(end > nrow(controlData))
      end <- nrow(controlData)

    controlData <- controlData[start:end,]
    title <- "Basic Control Plot - Focused"
  }



    predanControlPlot <- createJaspPlot(title= title, height = 320, width = 480)
    p <-ggplot2::ggplot(controlData,ggplot2::aes(time,y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
      ggplot2::geom_hline(yintercept = upperLimit,linetype="dashed",color="darkred")+
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred")+
      ggplot2::scale_color_manual(guide="none",values=c("#4E84C4","#D16103"))


  if(options$controlLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=1,ggplot2::aes(colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit)),
                                lineend = "round",linejoin ="round")
  if(options$controlLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=2)

  if(options$xAxisLimit == "controlBounds")
    p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                             plotLimit[[1]]))

  p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid)

  predanControlPlot$plotObject <- p
  #jaspResults[["testPlot"]] <- predanControlPlot
  if(!zoom)
    predanDescriptivePlots[["predanControlPlot"]] <- predanControlPlot
  else
    predanDescriptivePlots[["predanControlPlotZoom"]] <- predanControlPlot
  return()
}


.predanACFDescriptives <- function(jaspResults,dataset,options,ready){
  if((!options$acfPlotCheck || !ready) || !is.null(jaspResults[["predanMainContainer"]][["acfPlots"]])) return()

  acfPlots <- createJaspContainer(title= gettext("Autocorrelation Function Plots"))
  # TODO add dependencies
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanACFPlots(jaspResults,dataControl,options,acfPlots)

  jaspResults[["predanMainContainer"]][["acfPlots"]] <- acfPlots

  return()
}

.predanACFPlots <- function(jaspResults,dataControl,options,acfPlots){
  y <- na.omit(dataControl$y)

  ac <- acf(y, plot = F, lag.max = options$acfLagsMax)
  acP <- pacf(y, plot = F, lag.max = options$acfLagsMax)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(ac$lag)
  yRange <- ac$acf
  ci <- T
  ciValue = 0.95
  p <- ggplot2::ggplot()
  if (ci) {
    clim      <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
    dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))
    yRange    <- c(yRange, clim, -clim)

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = "dashed", color = "blue", data = dfSegment)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  dat <- data.frame(acf = ac$acf, lag = ac$lag, pacf = c(acP$acf,NA))


  acfPlot <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::labs(x = "Lag", y = "ACF") +
    ggplot2::geom_hline(yintercept = 0) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  pacfPlot <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = pacf), size = 1) +
    ggplot2::labs(x = "Lag", y = "Partial ACF") +
    ggplot2::geom_hline(yintercept = 0) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  predanACFPlot <- createJaspPlot(title= "Autocorrelation Function", height = 340, width = 480)

  predanPACFPlot <- createJaspPlot(title= "Partial Autocorrelation Function", height = 340, width = 480)

  predanACFPlot$plotObject <- acfPlot
  predanPACFPlot$plotObject <- pacfPlot

  acfPlots[["predanACFPlot"]] <- predanACFPlot
  acfPlots[["predanPACFPlot"]] <- predanPACFPlot
  return()
}


.predanHistogramPlot <- function(jaspResults,dataset,options,ready){
  if((!ready || !options$outlierHistogramCheck)|| !is.null(jaspResults[["predanMainContainer"]][["histogramPlot"]]) ) return()

  if(is.null(jaspResults[["predanMainContainer"]][["histogramPlot"]] ))
    histogramPlot <- createJaspPlot(title= gettext("Histogram Plot"), height = 360, width = 450)

  histogramPlot$dependOn(c(.boundDependencies(),"outlierHistogramCheck","outlierHistogramDensity"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanHistogramPlotFill(jaspResults,dataControl,options,histogramPlot)

  jaspResults[["predanMainContainer"]][["histogramPlot"]] <- histogramPlot

  return()


}


.predanHistogramPlotFill <- function(jaspResults,dataControl,options,histogramPlot){

  sigma.g1 <- sqrt((6*(length(dataControl$y) - 2)) / ((length(dataControl$y) + 1)*(length(dataControl$y) + 3)))
  g1 <- mean(abs(dataControl$y)^3)
  k <- 1 + log2(length(dataControl$y)) + log2(1 + (g1 / sigma.g1))
  binWidthType <- k

  h <- hist(dataControl$y, plot = T, breaks = binWidthType)
  xticks <- base::pretty(c(dataControl$y, h$breaks), min.n = 3)

  p <- ggplot2::ggplot(data= dataControl,mapping = ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..,
                                         fill=outBound),
                            bins=binWidthType,
                            breaks = h[["breaks"]],
                            colour = 1) +
    ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103")) +
    ggplot2::ylab("") +
    ggplot2::scale_x_continuous( breaks = xticks,limits = c(xticks[1],max(xticks))) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  if(options$outlierHistogramDensity)
    p <- p + ggplot2::geom_density(ggplot2::aes(group=outBound))

  histogramPlot$plotObject <- p

  return()
}

.predanDiagnosticTables <- function(jaspResults,dataset,options,ready){
  if(!ready) return()

  diagnosticTables <- createJaspContainer(title = "Diagnostic Tables")

  diagnosticTables$dependOn(c("summaryStatsTableCheck","outlierTableCheck"))
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  if(options$summaryStatsTableCheck)
    .predanSummaryTable(dataControl, options, diagnosticTables)

  if(options$outlierTableCheck)
    .predanOutlierTable(dataControl,options, diagnosticTables)

  jaspResults[["predanMainContainer"]][["diagnosticTables"]] <- diagnosticTables

  return()

}


.predanSummaryTable <- function(dataControl, options, diagnosticTables){

  summaryTable <- createJaspTable("Control Summary Table")


  summaryTable$dependOn("summaryStatsTableCheck")
  summaryTable$transpose<- TRUE
  summaryTable$addColumnInfo(name="variable",  title="Control Area", type="string")
  #summaryTable$addColumnInfo(name="Level",     title="", type="string")

  summaryTable$addColumnInfo(name="mean",     title=gettext("Mean"), type="number")
  summaryTable$addColumnInfo(name="sd",       title=gettext("SD"),   type="number")
  summaryTable$addColumnInfo(name="min",      title=gettext("Minimum"),   type="number")
  summaryTable$addColumnInfo(name="max",      title=gettext("Maximum"),   type="number")
  summaryTable$addColumnInfo(name="valid",    title=gettext("Valid"),   type="integer")
  summaryTable$addColumnInfo(name="missing",  title=gettext("Missing"),   type="integer")
  summaryTable$addColumnInfo(name="percent",  title=gettext("Percent"),   type="number", format= "dp:1;pc")
  summaryTable$addColumnInfo(name="deviation",title=gettext("Average Deviation"),   type="number")

  tableRes <- do.call(data.frame,
                      aggregate( y ~ outBoundArea,
                                 data = dataControl,
                                 FUN = function(x) c(mean = mean(x),
                                                     sd = sd(x),
                                                     min = min(x),
                                                     max = max(x),
                                                     valid = sum(!is.na(x)),
                                                     missing = sum(is.na(x)))
                                 ))

  tableRes$percent = tableRes[,6]/sum(tableRes[,6])

  tableDeviation <- do.call(data.frame,
                            aggregate( distance~ outBoundArea,
                                       data = dataControl,
                                       FUN = function(x) c(mean = mean(x)
                                       ) ))


  tableRes <- merge(tableRes,tableDeviation,all.x = T)


  for(i in 1:nrow(tableRes)){
    row <- list(
      variable = tableRes$outBoundArea[i],
      mean = tableRes[i,2],
      sd = tableRes[i,3],
      min = tableRes[i,4],
      max = tableRes[i,5],
      valid = tableRes[i,6],
      missing = tableRes[i,7],
      percent = tableRes[i,8],
      deviation = tableRes[i,9]
    )
    summaryTable$addRows(row)
  }


  diagnosticTables[["summaryTable"]] <- summaryTable

  return()
}



.predanOutlierTable <- function(dataControl,options, diagnosticTables){

  outlierTable <- createJaspTable("Outlier Table")

  outlierTable$dependOn("outlierTableTransposeCheck")

  if(options$outlierTableTransposeCheck)
    outlierTable$transpose <- TRUE


  outlierTable$addColumnInfo(name= "time",      title = "Time point",             type = "integer")

  outlierTable$addColumnInfo(name="variable",   title = "Control area",           type = "string")

  outlierTable$addColumnInfo(name= "value",     title = "Value",                  type = "number", format= "dp:2")

  outlierTable$addColumnInfo(name= "deviation", title = "Deviation",   type = "integer", format= "dp:2")

  for(i in dataControl$time[dataControl$outBound]){
    row <- list(
      time = dataControl$time[i],
      variable = dataControl$outBoundArea[i],
      value = dataControl$y[i],
      deviation = dataControl$distance[i])

    outlierTable$addRows(row)
  }


  diagnosticTables[["outlierTable"]] <- outlierTable

  return()

}









.predanComputeBinaryResults <- function(jaspResults,dataset,options,ready){
  if(!ready | !options$binaryControlChartCheck) return()

  if (is.null(jaspResults[["predanResults"]][["predanBinaryBounds"]])){
    predanBinaryBoundsState <- createJaspState()

    predanBinaryBoundsState$dependOn(c(.boundDependencies(),"binaryControlChartCheck"))

    predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
    controlData <- predanResults[[1]]

    #TODO: insert depend on all boundary setting
    #if(options$binaryControlMethod=="state space")
      predanBinaryBounds <- .predanBinaryStateSpaceResults(jaspResults,controlData,dataset,options)
    #else if (options$binaryControlMethod == "beta distribution")

    predanBinaryBoundsState$object <- predanBinaryBounds


    jaspResults[["predanResults"]][["predanBinaryBounds"]] <- predanBinaryBoundsState
  }
  return()
}


.predanBinaryStateSpaceResults <- function(jaspResults,controlData,dataset,options){

  y <- controlData$outBoundNum

  ss <- list()
  ss <- bsts::AddLocalLevel(ss,y,sigma.prior = Boom::SdPrior(sigma.guess = .1,
                                                       sample.size = 1,
                                                       upper.limit = 1),
                            initial.state.prior = Boom::NormalPrior(0, 5))

  ts.model <- bsts::bsts(y , ss, niter = options$binaryStateSpaceNiter,
                   family = "logit")

  return(ts.model)

}








.predanBinaryControlChart <- function(jaspResults,dataset,options,ready) {
  if(!ready | !options$binaryControlChartCheck) return()

  predanBinaryControlPlots <- createJaspContainer(title=gettext("Binary Control Plots"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object

  predanBinaryControlPlots$dependOn(c(.boundDependencies(),"binaryControlChartCheck"))

  #if(options$binaryControlMethod == "state space"){
    tsModel <- jaspResults[["predanResults"]][["predanBinaryBounds"]]$object
    .predanBinaryControlStateSpacePlot(jaspResults,predanBinaryControlPlots,tsModel,predanResults,dataset,options)
  #}


  jaspResults[["predanMainContainer"]][["predanBinaryControlPlots"]] <- predanBinaryControlPlots
  return()
}
#
.predanBinaryControlStateSpacePlot <- function(jaspResults,predanBinaryControlPlots,tsModel,predanResults,dataset,options) {

  predanBinaryControlStateSpacePlot <- createJaspPlot(title= "Binary state space plot", height = 320, width = 480)

  results <- .extractState(tsModel,T)
  p <- ggplot2::ggplot(results,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=lowerCI,ymax =higherCI),
                         fill ="blue",alpha=0.5) + ggplot2::xlab("Time") +
    ggplot2::ylab("Distribution") +
    ggplot2::geom_line(size=0.7)  + ggplot2::theme_classic() +
    ggplot2::geom_point(ggplot2::aes(y=actualData),size=0.5)

  if(options$binaryControlOutPropLimit > 0)
    p <- p + ggplot2::geom_hline(yintercept = options$binaryControlOutPropLimit,linetype="dashed",color="darkred")

  predanBinaryControlStateSpacePlot$plotObject <- p

  predanBinaryControlPlots[["predanBinaryControlStateSpacePlot"]] <- predanBinaryControlStateSpacePlot
  return()
}

