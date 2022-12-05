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
    ready <- options$dependent != "" & options$time != ""

    dataset <- .predanReadData(options,ready)

    .predanContainerSetup(jaspResults, ready)
    .predanComputeResults(jaspResults, dataset, options, ready)
    .predanPlotsDescriptives(jaspResults,dataset,options,ready)

    .predanACFDescriptives(jaspResults,dataset,options,ready)
    .predanHistogramPlot(jaspResults,dataset,options,ready)
    .predanDiagnosticTables(jaspResults, dataset, options,ready)


    .predanFeatureEngineeringHelper(jaspResults,dataset,options,ready)
    .predanForecastVerificationPlanner(jaspResults,dataset,options,ready)

    .predanMetricTable(jaspResults,options,ready)
    .predanForecastVerificationResultPlot(jaspResults,options,ready)
    #.predanComputeBinaryResults(jaspResults,dataset,options,ready)
    #.predanBinaryControlChart(jaspResults,dataset,options,ready)

    #.predanComputeControlPrediction(jaspResults,dataset,options,ready)
    #.predanControlPredictionChart(jaspResults,dataset,options,ready)
    #.predanModelSelectionFitHelper(jaspResults,dataset,options,ready)
    #.predanSelectedModelPlots(jaspResults,dataset,options,ready)
#
    #.predanForecastVerificationModelsHelper(jaspResults,dataset,options,ready)
    #.predanForecastVerificationModelsTable(jaspResults,dataset,options,ready)
#
    .predanBMAHelperResults(jaspResults,dataset,options,ready)
    .predanBMAWeightsTable(jaspResults,dataset,options,ready)
    #.predanBMAPlots(jaspResults,dataset,options,ready)
    #.predanFuturePredictionHelper(jaspResults,dataset,options,ready)
    .predanFuturePredictionResults(jaspResults,dataset,options,ready)
    .predanFuturePredictionPlot(jaspResults,dataset,options,ready)
    .predanFuturePredictionTable(jaspResults,dataset,options,ready)

    return()
}

.extractQuantiles <-function(state){
  data.frame(mean = colMeans(state,na.rm = T),

             lowerCI = apply(state,2,quantile,probs= 0.025,na.rm = T),
             higherCI= apply(state,2,quantile,probs= 0.975,na.rm = T),
             tt = 1:ncol(state)
  )
}
.predanReadData <- function(options,ready) {
  if(!ready) return()
  numericVariable <- c(options$dependent,unlist(options$covariates))
  numericVariable <- numericVariable[numericVariable != ""]
  nominalVars     <- unlist(options$factors)
  timeVar <- unlist(options$time)
  timeVar <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVariable,
                               columns.as.factor = nominalVars,
                               columns = timeVar)

  #options$dataset <- dataset

  return(dataset)

}

.predanContainerSetup <- function(jaspResults, options) {

  if (is.null(jaspResults[["predanMainContainer"]])){
    predanMainContainer <- createJaspContainer()
    jaspResults[["predanMainContainer"]] <- predanMainContainer

  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]])){
    predanDescriptivesContainer <- createJaspContainer("Descriptives", position = 1)
    jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]] <- predanDescriptivesContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]])){
    predanDiagnosticsContainer <- createJaspContainer("Diagnostics", position = 2)
    jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]] <- predanDiagnosticsContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["cvContainer"]])){
    cvContainer <- createJaspContainer("Forecast Verification", position = 3)
    jaspResults[["predanMainContainer"]][["cvContainer"]] <- cvContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]])){
    predanModelSelectionContainer <- createJaspContainer("Model Selection", position = 4)
    jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]] <- predanModelSelectionContainer
  }

  if (is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]])){
    predanBMAContainer <- createJaspContainer("Bayesian Model Averaging", position = 5)
    jaspResults[["predanMainContainer"]][["predanBMAContainer"]] <- predanBMAContainer
  }
  if (is.null(jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]])){
    predanBMAContainer <- createJaspContainer("Future Prediction", position = 6)
    jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]] <- predanBMAContainer
  }


  return()
}


###### Helper Functions


# finds intersection between lines
.findCross <- function(tt,y,time,limit){
  d <- y - limit
  signChanges <- (d[1:(length(d)-1)] * d[2:length(d)]) <= 0
  left <- c(signChanges, FALSE)
  right <- c(FALSE, signChanges)
  t <- (limit - y[left])/(y[right] - y[left])

  x2 <- (1 - t)*tt[left] + t*tt[right]

  time2 <- as.numeric(time)
  x2time <- (1 - t)*time2[left] + t*time2[right]
  time3 <- as.POSIXct(x2time,origin = "1970-01-01")

  if(length(x2 ) > 0)
    return(data.frame(y=limit, time = time3,tt  = x2, include=0,outBound=T))
  else
    return(data.frame(y=NULL,time = NULL, tt=NULL,  include=NULL,outBound=NULL))
}



##### model selection helper functions


#.modelSelection




.dssScore <- function(y,dat){
  m <- mean(dat,na.rm = T)
  v <- mean(dat^2,na.rm = T) - m^2

  return(sapply(y, function(s) (s - m)^2 / v + log(v)))
}

.brierScore <- function(pred,real,calc_mean = F){

  brier <- (real-pred)^2
  if(calc_mean)
    brier <- mean(brier,na.rm=T)
  return(brier)
}

.crpsScore <- function(y,dat){
  c_1n <- 1 / length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  f <- function(s) 2 * c_1n * sum(((s < x) - a) * (x - s),na.rm = T)
  return(  sapply(y, f))
}



# determines quantile rank of value against ecdf
quantInv <- function(distr, value){
  if(all(is.na(distr))) return(NA)
  if(sum(distr < value) == length(distr))
    return(1) # 1 if valuelarger than all MCMC draws
  else if (sum(distr > value) == length(distr))
    return(0) # 0 if value smaller than all MCMC draws
  else
    ecdf(distr)(value)
}




### helper function that applies BMA weights to predictions
.bmaFuturePrediction <- function(fitBMAObject, modelListFuturePredictions, weightWindow,limits) {
  modelWeights <- colMeans(tail(t(fitBMAObject$weights), weightWindow))
  aWeights <- colMeans(tail(t(fitBMAObject$biasCoefs[1, , ]), weightWindow))
  bWeights <- colMeans(tail(t(fitBMAObject$biasCoefs[2, , ]), weightWindow))

  biasCorrectedPredictionList <- lapply(X = 1:length(modelListFuturePredictions), FUN = function(x) aWeights[x] + bWeights[x] * modelListFuturePredictions[[x]])
  names(biasCorrectedPredictionList) <- names(modelListFuturePredictions)
  weightedPredictions <- lapply(X = 1:length(modelListFuturePredictions), FUN = function(x) biasCorrectedPredictionList[[x]] * modelWeights[x])
  # add BMA model
  biasCorrectedPredictionList$BMA <- Reduce("+", weightedPredictions)
  sumPred2 <- lapply(biasCorrectedPredictionList, .extractQuantiles)
  sumPredLimitsUpper <- lapply(biasCorrectedPredictionList,function(x) 1-quantInvVec(t(x),limits[1]))
  sumPredLimitsLower <- lapply(biasCorrectedPredictionList,function(x) quantInvVec(t(x),limits[2]))
  sumPred2 <- lapply(X = 1:length(sumPred2),function(x) cbind(sumPred2[[x]],upperProb = sumPredLimitsUpper[[x]]))
  sumPred2 <- lapply(X = 1:length(sumPred2),function(x) cbind(sumPred2[[x]],lowerProb = sumPredLimitsLower[[x]]))

  sumPred2 <- lapply(X = 1:length(sumPred2), function(x) cbind(sumPred2[[x]], model = names(biasCorrectedPredictionList)[x]))

  sumPred2 <- do.call("rbind", sumPred2)
  return(sumPred2)
}




##### dependencies
.boundDependencies <- function(){
  return(c("dependent","time","errorBoundMethodDrop",
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



.forecastVeriDependencies <- function(){
  return(c("resampleForecastHorizon",
           "resampleInitialTraining",
           "resampleSkip",
           "resampleMaxSlice",
           "resampleCumulativeCheck",
           "resampleSliceStart"))
}



.forecastMetricDependencies <- function(){
  return(c("forecastMetricsCRPS",
           "forecastMetricsDSS",
           "forecastMetricsAUC",
           "forecastMetricsPR",
           "forecastMetricsBrier"))
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

  dataControl <- data.frame(y =  dataset[,options[["dependent"]]], time = as.POSIXct( dataset[,options[["time"]]]))
  dataControl$tt <- 1:nrow(dataControl)

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
    upperLimit <- mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) + sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound
    lowerLimit <- mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) - sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound
    plotLimit <- c(mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) + 2*sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound,
                   mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) - 2*sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound)
  }



  dataControl$outBound <- ifelse(dataControl$y > upperLimit | dataControl$y < lowerLimit,T,F)
  dataControl$outBoundNum <- as.numeric(dataControl$outBound)
  dataControl$outBoundArea[!is.na(dataControl$y)] <- "Inside"
  dataControl$outBoundArea[!is.na(dataControl$outBound)] <- ifelse(dataControl$y[!is.na(dataControl$outBound)] > upperLimit,"Above",ifelse(dataControl$y[!is.na(dataControl$outBound)] < lowerLimit,"Below","Inside"))

  dataControl$distance[!is.na(dataControl$outBound)] <- ifelse(dataControl$y[!is.na(dataControl$outBound)] > upperLimit,
                                                               dataControl$y[!is.na(dataControl$outBound)] - upperLimit,
                                                               dataControl$y[!is.na(dataControl$outBound)] - lowerLimit)


  results <- list(dataControl = dataControl,
                  upperLimit = upperLimit,
                  lowerLimit = lowerLimit,
                  plotLimit = plotLimit)

  return(results)
}

.predanPlotsDescriptives <- function(jaspResults,dataset,options,ready) {
  if(!ready) return()

  predanDescriptivesContainer <- jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]]

  #predanDescriptivePlots <- createJaspContainer(title=gettext("Descriptives"),position =1)

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]][["object"]]



  if(options$controlPlotCheck)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivesContainer,options,zoom=F)

  if(options$controlPlotZoomCheck && options$zoomPeriodEnd >0)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivesContainer,options,zoom=T)


  #jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlot <- function(jaspResults,predanResults,predanDescriptivesContainer,options,zoom){

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


  plotData <- controlData[1:4]
  plotData$include = 1

  plotData <- rbind(plotData,
                    .findCross(plotData$tt,plotData$y,plotData$time,upperLimit),
                    .findCross(plotData$tt,plotData$y,plotData$time,lowerLimit))


  plotData <- plotData[order(plotData$tt),]

  plotData$includeLine <- NA
  plotData$includeLine[1] <- plotData$outBound[1]
  for (i in 2:nrow(plotData)) {
    plotData$includeLine[i] <- ifelse((plotData$include[i] == 0 & plotData$include[i+1] &plotData$outBound[i+1]==T)|
                                        plotData$outBound[i]&plotData$outBound[i+1]==T,T,F)

  }

  t_var <- ifelse(options$"controlSpreadPointsEqually","tt","time")

  xBreaks <- pretty(plotData[[t_var]])
  if(options$xAxisLimit =="controlBounds")
    yBreaks <- pretty(plotLimit)
  else
    yBreaks <- pretty(plotData$y)



  predanControlPlot <- createJaspPlot(title= title, height = 480, width = 720)
  p <-ggplot2::ggplot(plotData[plotData$include==1,],ggplot2::aes(.data[[t_var]],y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
    ggplot2::geom_hline(na.rm = T,yintercept = upperLimit,linetype="dashed",color="darkred") +
    ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred") +
    ggplot2::scale_color_manual(guide="none",values=c("#4E84C4","#D16103"))


  if(options$controlLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=1,data=plotData,ggplot2::aes(.data[[t_var]],y,colour=ggplot2::after_stat(plotData$includeLine)))
  if(options$controlLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=2)

  if(options$xAxisLimit == "controlBounds")
    p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                             plotLimit[[1]]))

  p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::scale_y_continuous(name = "y",breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = "time",breaks = xBreaks,limits = range(xBreaks))

  predanControlPlot$plotObject <- p
  #jaspResults[["testPlot"]] <- predanControlPlot
  if(!zoom)
    predanDescriptivesContainer[["predanControlPlot"]] <- predanControlPlot
  else
    predanDescriptivesContainer[["predanControlPlotZoom"]] <- predanControlPlot
  return()
}



.predanACFDescriptives <- function(jaspResults,dataset,options,ready){
  if((!options$acfPlotCheck || !ready) || !is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["acfPlots"]])) return()

  acfPlots <- createJaspContainer(title= gettext("Autocorrelation Function Plots"))
  # TODO add dependencies
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanACFPlots(jaspResults,dataControl,options,acfPlots)

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["acfPlots"]] <- acfPlots

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
    jaspGraphs::themeJaspRaw() + ggplot2::ylim(c(NA,1))

  predanACFPlot <- createJaspPlot(title= "Autocorrelation Function", height = 340, width = 480)

  predanPACFPlot <- createJaspPlot(title= "Partial Autocorrelation Function", height = 340, width = 480)

  predanACFPlot$plotObject <- acfPlot
  predanPACFPlot$plotObject <- pacfPlot

  acfPlots[["predanACFPlot"]] <- predanACFPlot
  acfPlots[["predanPACFPlot"]] <- predanPACFPlot
  return()
}


.predanHistogramPlot <- function(jaspResults,dataset,options,ready){
  if((!ready || !options$outlierHistogramCheck)|| !is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]]) ) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]] ))
    histogramPlot <- createJaspPlot(title= gettext("Histogram Plot"), height = 360, width = 450)

  histogramPlot$dependOn(c(.boundDependencies(),"outlierHistogramCheck","outlierHistogramDensity"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanHistogramPlotFill(jaspResults,dataControl,options,histogramPlot)

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]] <- histogramPlot

  return()


}


.predanHistogramPlotFill <- function(jaspResults,dataControl,options,histogramPlot){

  sigma.g1 <- sqrt((6*(length(dataControl$y) - 2)) / ((length(dataControl$y) + 1)*(length(dataControl$y) + 3)))
  g1 <- mean(abs(dataControl$y)^3)
  k <- 1 + log2(length(dataControl$y)) + log2(1 + (g1 / sigma.g1))
  binWidthType <- k

  h <- hist(dataControl$y, plot = F, breaks = binWidthType)
  xticks <- base::pretty(c(dataControl$y, h$breaks), min.n = 3)

  if (options$outlierHistogramDensity)
    yhigh <- max(h$counts)

  p <- ggplot2::ggplot(dataControl,
                       ggplot2::aes(x= y,
                                    binwidth = 0.5))

  if(!options$outlierHistogramDensity){
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
      y= ..count.. ,
      fill = outBound),
      colour="black",
      binwidth = binWidthType,
      breaks = h[["breaks"]],
      position = "stack") +
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
      ggplot2::guides(color = "none") +
      ggplot2::scale_y_continuous(name = "Count",breaks = base::pretty(c(0, h$counts))) +
      ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103"))

  } else {
    dftmp <- data.frame(x = range(xticks), y = range( c(0,  max(h$density))))
    p <- p + ggplot2::geom_line(data = dftmp,
                                mapping = ggplot2::aes(x = .data$x,
                                                       y = .data$y), color = "white", alpha = 0)
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
      y= ..count.. / (sum(..count..) * binwidth) ,
      fill = outBound),
      colour= "black",
      size     = .7,
      binwidth = binWidthType,
      breaks = h[["breaks"]],
      position = "stack") +
      ggplot2::geom_density() +
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
      ggplot2::scale_x_continuous( breaks = xticks,limits = c(xticks[1],max(xticks))) +
      ggplot2::guides(color = "none") +
      ggplot2::scale_y_continuous(name = "Density") +
      ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103")) +
      ggplot2::guides(color = FALSE)
  }

  p <- jaspGraphs::themeJasp(p)

  if(options$outlierHistogramDensity){
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_blank())
  }

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

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["diagnosticTables"]] <- diagnosticTables

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
  #summaryTable$addColumnInfo(name="missing",  title=gettext("Missing"),   type="integer")
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


  tableResAll <- do.call(data.frame,
                         aggregate( y ~ 1,
                                    data = dataControl,
                                    FUN = function(x) c(mean = mean(x),
                                                        sd = sd(x),
                                                        min = min(x),
                                                        max = max(x),
                                                        valid = sum(!is.na(x)),
                                                        missing = sum(is.na(x)))
                         ))

  tableResAll$outBoundArea = "All"

  tableRes <- rbind(tableRes,tableResAll)
  tableRes$percent = tableRes[,6]/nrow(dataControl)

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
      #missing = tableRes[i,7],
      percent = tableRes[i,8],
      deviation = tableRes[i,9]
    )
    summaryTable$addRows(row)
  }


  diagnosticTables[["summaryTable"]] <- summaryTable

  return()
}



.predanOutlierTable <- function(dataControl,options, diagnosticTables){


  if(options$outlierTableFocusCheck & options$outLierTableEnd >1){
    start <- options$outLierTableStart
    end <- options$outLierTableEnd
    if(end > nrow(dataControl))
      end <- nrow(dataControl)

    dataControl <- dataControl[start:end,]
    title <- "Control Summary Table - Focused"
  } else
    title <- "Control Summary Table"

  outlierTable <- createJaspTable(title = title)

  outlierTable$dependOn("outlierTableTransposeCheck")

  if(options$outlierTableTransposeCheck)
    outlierTable$transpose <- TRUE


  outlierTable$addColumnInfo(name= "time",      title = "tt point",             type = "integer")

  outlierTable$addColumnInfo(name="variable",   title = "Control area",           type = "string")

  outlierTable$addColumnInfo(name= "value",     title = "Value",                  type = "number", format= "dp:2")

  outlierTable$addColumnInfo(name= "deviation", title = "Deviation",   type = "integer", format= "dp:2")

  for(i in dataControl$tt[dataControl$outBound]){
    row <- list(
      tt = dataControl$tt[i],
      variable = dataControl$outBoundArea[i],
      value = dataControl$y[i],
      deviation = dataControl$distance[i])

    outlierTable$addRows(row)
  }


  diagnosticTables[["outlierTable"]] <- outlierTable

  return()

}

### featureEng Helper

lagit <- function(a,k) {
  tmp <- lapply(k,function(i) c(rep(NA,i),head(a,length(a)-i)))
  res <- as.data.frame(cbind(a,do.call(cbind,tmp))[,-1])
  colnames(res) <- paste0("y_lag",c(k))
  res
}




.predanFeatureEngineeringHelper <- function(jaspResults,dataset,options,ready){
  if(!ready || !is.null(jaspResults[["predanResults"]][["featureEngState"]])) return()

  featureEngState <- createJaspState()
  featureEngState$dependOn(c("featEngLags","featEngAutoTimeBased","covariates","factors","dependent"))
  #featEngData <- data.frame(y =  dataset[,options[["dependent"]]], time = as.POSIXct( dataset[,options[["time"]]]))

  featEngData <- dataset[,c(options[["dependent"]],jaspBase::encodeColNames(options$time))]
  if(length(options$covariates)>0)
    featEngData <- cbind(featEngData,dataset[,jaspBase::encodeColNames(options$covariates),drop=F])
  colnames(featEngData)[1:2] <- c("y","time")

  featEngData$time <- as.POSIXct(featEngData$time)
  print(colnames(featEngData))
    #convert factors to dummy vars via model.matrix

  if(length(options$factors) > 0){

    fctrs <- dataset[,jaspBase::encodeColNames(options$factors),drop=F]
    colnames(fctrs) <- decodeColNames(colnames(fctrs))
    featEngData <- as.data.frame(cbind(featEngData,model.matrix(~.-1,fctrs)))

  }

  ##TODO: add
  if(options$featEngLags > 0)
    featEngData <- cbind(featEngData,as.data.frame(lagit(featEngData$y,k = 1:options$featEngLags)))

  if(options$featEngAutoTimeBased)
    featEngData <- cbind(featEngData,get_timeseries_signature_date(featEngData$time))



  #stop(gettext(paste0(colnames(featEngData))))

  colnames(featEngData) <- decodeColNames(colnames(featEngData))
  featureEngState$object <- as.data.frame(na.omit(featEngData))
  jaspResults[["predanResults"]][["featureEngState"]] <- featureEngState
  return()
}

######  CV Helper Function ######


.predanForecastVerificationPlanner <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["featureEngState"]])) return()

  dataControl <- jaspResults[["predanResults"]][["predanBounds"]]$object[[1]]

  if(is.null(jaspResults[["predanResults"]][["cvPlanState"]])){
    cvPlanState <- createJaspState(dependencies = c(.boundDependencies(),.forecastVeriDependencies()))
    cvPlanState$object <- .crossValidationPlanHelper(data = dataControl,
                                                     initial = options$resampleInitialTraining,
                                                     assess = options$resampleForecastHorizon,
                                                     cumulative = options$resampleCumulativeCheck,
                                                     skip = options$resampleSkip,
                                                     max_slice = options$resampleMaxSlice,
                                                     from = options$"resampleSliceStart")
    jaspResults[["predanResults"]][["cvPlanState"]] <- cvPlanState
  }

  if(is.null(jaspResults[["predanMainContainer"]][["cvContainer"]][["cvPlanPlot"]]) && options$resampleCheckShowTrainingPlan){
    cvPlot <- createJaspPlot(width = 720,height = 180*min(c(
      length(jaspResults[["predanResults"]][["cvPlanState"]]$object), options$resamplePlanPlotMaxPlots)))
    cvPlot$dependOn(c(.boundDependencies(),
                      .forecastVeriDependencies(),
                      "resampleCheckShowTrainingPlan",
                      "resamplePlanPlotEqualDistance",
                      "resamplePlanPlotMaxPlots"))
    cvPlot$plotObject <- .cvPlanPlot(data = dataControl,
                                     cvPlan = jaspResults[["predanResults"]][["cvPlanState"]]$object,
                                     equal_distance = options$resamplePlanPlotEqualDistance,
                                     maxSlices = options$resamplePlanPlotMaxPlots,ncol=1)
    jaspResults[["predanMainContainer"]][["cvContainer"]][["cvPlanPlot"]] <- cvPlot
  }


  if(is.null(jaspResults[["predanResults"]][["cvResultsState"]])){
    if(length(options$selectedModels) == 0) return()
    cvResultsState <- createJaspState(dependencies = c(.boundDependencies(),
                                                       .forecastVeriDependencies(),
                                                       "selectedModels"))

    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object

    modelList <- .createModelListHelper(dataEng,unlist(options$selectedModels))

    cvResults <- list()
    print(paste0("models:",sapply(modelList,"[","model")))




    for (i in 1:length(modelList)) {


      startProgressbar(
        length(modelList),
        gettextf(
          paste0("Running model ", i, " / ",
                 length(modelList)," :",options$selectedModels[[i]]))
      )


      cvResults[[i]] <- .crossValidationHelperSlices(model = modelList[[i]]$model,
                                                     formula = modelList[[i]]$modelFormula,
                                                     data = dataEng,
                                                     cvPlan = jaspResults[["predanResults"]][["cvPlanState"]]$object,
                                                     parallel = options$parallelComputation,preProList = T,keepModels = "summary",keepMetrics = "summary")



      cvResults[[i]]$modelName <- options$selectedModels[[i]]
    }

    names(cvResults) <- paste0(sapply(modelList, function(x) x$model),seq_along(modelList))

    cvResultsState$object <- cvResults

    jaspResults[["predanResults"]][["cvResultsState"]] <- cvResultsState

   # if(options$checkPerformBma)
   #   selectedMod <- options$selectedModels
   # else
   #   selectedMod <- c(unlist(options$selectedModels),"BMA")
    jaspResults[["plottableModelsQml"]] <- createJaspQmlSource(sourceID="plottableModelsQml", value= options$selectedModels,dependencies = "checkPerformBma")
  }

  return()
}


.crossValidationPlanHelper <- function(data,initial = 20,assess = 10,cumulative = TRUE,skip = 10,lag = 0, max_slice = 5,from = c("head","tail")){
  from <- match.arg(from)
  n <- nrow(data)
  stops <- n - seq(assess, (n - initial), by = skip)
  if (!cumulative) {
    starts <- stops - initial + 1
  } else {
    starts <- rep(1, length(stops))
  }
  in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  out_ind <- mapply(seq, stops + 1 - lag, stops + assess, SIMPLIFY = FALSE)
  merge_lists <- function(a, b) list(analysis = a, assessment = b)
  indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
  names(indices) <- paste0("slice",length(indices):1)

  return(do.call(from,args = list(rev(indices),max_slice)))

}

.cvPlanPlot <- function(data,cvPlan,maxSlices=2,equal_distance = T,...){

  t_var <- ifelse(equal_distance,"tt","time")

  data$X <- 1:nrow(data)
  dataPlot <- dplyr::bind_rows(.id = "id",lapply(head(cvPlan,maxSlices), function(x) data.frame( tt = c(x$analysis,x$assessment),
                                                                                                 type = rep(c("Analysis","Assessment"),c(length(x$analysis),length(x$assessment)) ))))

  dataPlot <- dplyr::left_join(dataPlot,data[,c("tt","time","y")],by = c("tt"))
  dataPlot$id <- factor(dataPlot$id,levels = names(cvPlan),ordered = F)

  xBreaks <- pretty(dataPlot[[t_var]])
  yBreaks <- pretty(dataPlot$y)


  p <- ggplot2::ggplot(dataPlot,ggplot2::aes_string(t_var,"y",color="type")) + ggplot2::geom_line() +
    ggplot2::scale_color_manual(name = "Time period",values=c("#4E84C4","#D16103")) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::scale_y_continuous(name = "Y",breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::facet_wrap(facets  = "id",scales = "free",...) +jaspGraphs::geom_rangeframe() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid,
                   panel.background = ggplot2::element_rect(fill = "white"),
                   legend.position = "bottom")

  return(p)
}

.createModelListHelper <- function(dataEng, selectedModels){

  modNames <- c("linear regression - y ~ time"= "lmSpike",
              "linear regression - regression"= "lmSpikeReg",
              "linear regression - regression + lag"= "lmSpikeRegLag",
              "bsts - linear trend model"= "bstsLinear",
              "bsts - linear trend model - regression"= "bstsLinearReg",
              "bsts - linear trend model - regression + lag"= "bstsLinearLag",
              "bsts - autoregressive model"= "bstsAr",
              "bsts - autoregressive model - regression"= "bstsArReg",
              "bsts - autoregressive model - regression + lag"= "bstsArRegLag",
              "prophet"= "prophet",
              "prophet - regression"= "prophetReg",
              "prophet - regression + lag"= "prophetRegLag",
              "xgboost - regression"= "xgboostReg",
              "xgboost - regression + lag"= "xgboostRegLag",
              "bart - regression" = "bartReg",
              "bart - regression + lag" = "bartRegLag",
              "bart - stack" = "bartStackReg")



  shortNames <- modNames[selectedModels]

  res <- lapply(shortNames, function(x){
    names(selectedModels) <- selectedModels
    model <- gsub("Lag|Reg","",x)

    # check whether model is only y ~ time, regression or has lag component
    # create formula accordingly
    modelFormula <- switch(
      sum(unlist(sapply(c(model,"Reg", "Lag"),grep,x))),
      "1" = formula(y ~ time),
      "2" = formula(dataEng[,!grepl("lag",colnames(dataEng))]),
      "3" = formula(dataEng)
    )
    # currently unused but option to extend model
    #with custom parameters such as MCMC draws, exptected model size,...
    #model_args = NULL
    #stop(gettext(paste0(modelFormula)))

    return(list(model = model,modelFormula = modelFormula,modelName = names(modNames)[modNames==x]))
  })
  print(res)
}



# wrapper that performs preprocessing, trains model and does prediction
.predAnModelFit <- function(trainData, formula, method, fit,predictFuture = F, testData,model_args = list(),preProList = NULL,...){


  lags <- sum(grepl("y_lag",labels(terms(formula))))
  #if(lags>0){
  #  trainData <- na.omit(cbind(trainData,lagit(trainData$y,1:lags)[,-1]))
  #  print("lag")
#
  #}

  #if(method == "lmSpike"){
  #  preProSpec <- preProcess.default(trainData[,!grepl("y",colnames(trainData))],verbose = F)
  #  trainData <- predict.preProcess(preProSpec,trainData)
  #  if(!is.null(testData)) testData <- predict(preProSpec,testData)
#
  #}

  if(!is.null(preProList)){
    preProSpec <- preProcess.default(trainData[,!grepl("y",colnames(trainData))],verbose = F,method = c("center", "scale","zv"))

    #identify factors with too
    #fctr <- sapply(trainData,is.factor)
    #factorsToRemove <-names(sapply(trainData[,fctr], function(x) length(unique(x)) < 2))
    #preProSpec$method$remove <- append(preProSpec$method$remove,factorsToRemove)
    #print(paste0("to remove:",preProSpec$method$remove,collapse = " "))
    trainData <- predict.preProcess(preProSpec,trainData)

    # remove args if they have zero var from formula
    if(length(preProSpec$method$remove) >0 )
      formula <- update(formula, paste0(".~." , paste0("-",preProSpec$method$remove,collapse = " ")))
    if(!is.null(testData)) testData <- predict.preProcess(preProSpec,testData)


  }

  args <- c(model_args,list(...))
  pred <- NULL
  fit <- switch (method,
                 "lmSpike" = do.call(.lmSpikeFitHelper,c(list(trainData,formula,"lmSpike",...),model_args)),
                 "bstsAr" = do.call(.bstsFitHelper,c(list(trainData,formula,"bstsAr",...),model_args)),
                 "bstsLinear" = do.call(.bstsFitHelper,c(list(trainData,formula,"bstsLinear",...),model_args)),
                 "prophet" = do.call(.prophetFitHelper,c(list(trainData,formula,...),model_args)),
                 "xgboost" = do.call(.xgbFitHelper,c(list(trainData = trainData,formula = formula),model_args,...)),
                 "bart" = do.call(.bartFitHelper,c(list(trainData = trainData,formula = formula),model_args,...)),
                 "bartStack" = do.call(.bartStackFit,c(list(trainData = trainData,formula = formula,testData = testData),model_args,...))
  )
  if(predictFuture & lags == 0){

    pred <- switch (method,
                    "lmSpike" = do.call(.lmSpikePredictHelper,c(list(fit$model,testData,trainData,formula,...),model_args)),
                    "bstsAr" = .bstsPredictHelper(fit$model,testData, horizon,formula,...),
                    "bstsLinear" = .bstsPredictHelper(fit$model,testData, horizon,formula,...),
                    "prophet" = .prophetPredictHelper(fit$model,testData,horizon,...),
                    "xgboost" = do.call(.xgbPredHelper,list(fit$model,testData,formula = formula)),
                    "bart" = do.call(.bartPredictHelper,list(fit$model,testData,formula = formula,...)),
                    "bartStack" = do.call(.bartStackPredictHelper,list(fit = fit$model,testData = testData,formula = formula,trendPred = fit$trendPred,...))
    )

  } else if(lags > 0){

    pred <- .recursivePredict(testData,trainData,formula,method,model = fit$model,lags = lags)

  }
  if(is.null(pred))
    return(list(fit = fit))

  return(list(fit = fit, pred=pred))


}



# helper function that wraps around model wrapper and performs cv across slices
# option to discard models fully and only keep prediction of state MCMC draws
# option to calculate metrics directly and then discard full MCMC and only keep sample to save memory
.crossValidationHelperSlices <- function(model,
                                        formula,
                                        data,
                                        cvPlan,
                                        keepModels = c("fully","distr","summary","none"),
                                        keepMetrics = c("fully", "summary","none"),
                                        metrics = c("crps","dss","log","coverage","bias", "pit","mae","rmse","rsq"),
                                        model_args=list(),
                                        parallel = T,
                                        ...) {


  keepModels <- match.arg(keepModels)
  keepMetrics <- match.arg(keepMetrics)
  cvResList <- list()
  cvModelObject <- list()

  if(model == "xgboost" | !parallel){
    future::plan(future::sequential)
  } else{
    ifelse(Sys.info()["sysname"] == "Windows",
           future::plan(future::multisession,workers = 5),
           future::plan(future::multisession,workers = 5))
  }
  cvModelObject <- future.apply::future_lapply(X = seq_along(cvPlan),function(i){
    system(sprintf('echo "\n%s\n"', paste0("fitting slice " , i, " of ",model)))
    .predAnModelFit(trainData = data[cvPlan[[i]]$analysis,],
                   testData = data[cvPlan[[i]]$assessment,],
                   predictFuture = T,
                   method = model,
                   formula = formula,
                   model_args =model_args,...
    )
  })
  future::plan(future::sequential)

  l <- lapply(cvModelObject, function(x) x$pred$dist)
  # time,draw,slice array
  predArray <- array(unlist(l),dim=c(dim(l[[1]]),length(l)),dimnames = list(1:dim(l[[1]])[1],1:dim(l[[1]])[2],names(cvModelObject)))

  realMatrix <- matrix(unlist(lapply(cvModelObject, function(x) x$pred$trueVal)),ncol = length(cvModelObject),
                       dimnames = list(1:dim(l[[1]])[1],names(cvModelObject))
                       )


  predSummary <- aperm(apply(X = predArray,c(1,3) ,
                             function(x) c(mean = mean(x),
                                           upr = quantile(x,0.975),
                                           lwr = quantile(x,0.025)),
                             simplify = T),perm = c(2,1,3))

  if(keepMetrics %in% c("summary","fully")){
    scoringArray <- sapply(X = 1:length(l),simplify = "array",function(x) .scorePred(predMatrix = predArray[,,x],real = realMatrix[,x],metrics) )
    dimnames(scoringArray)[3] <- list(names(l))
    cvResList$scoringSummary <- apply(scoringArray, c(3,2), mean)
    if(keepMetrics == "fully")
      cvResList$scoringArray <- scoringArray
  }


  if(keepModels %in% c("fully","distr","summary")){
    cvResList$predSummary <- predSummary
    if(keepModels %in% c("fully","distr")){
      cvResList$predArray <- predArray
      if(keepModels == "fully")
        cvResList$slices <- cvModelObject
    }
  }
  cvResList$realMatrix <- realMatrix
  dimnames(cvResList$realMatrix)[2] <- list(names(cvPlan))
  return(cvResList)
}



.scorePred <- function(predMatrix,real,metrics = c("crps","dss","log","coverage","bias", "pit","mae","rmse","rsq"),SD=NULL){
  #metrics <- match.arg(metrics,several.ok = T)
  resScoringMatrix <- matrix(ncol = length(metrics),nrow = length(real),dimnames = list(NULL,metrics))
  if(is.matrix(predMatrix)){
    if("crps" %in% metrics)
      resScoringMatrix[,"crps"] <- scoringRules::crps_sample(real,predMatrix)
    if("dss" %in% metrics)
      resScoringMatrix[,"dss"] <- scoringRules::dss_sample(real,predMatrix)
    if("log" %in% metrics)
      resScoringMatrix[,"log"] <- scoringRules::logs_sample(real,predMatrix)
    if("coverage"  %in% metrics)
      resScoringMatrix[,"coverage"] <- apply(predMatrix, 1, quantile,0.25) < real & real < apply(predMatrix, 1, quantile,0.75)

    if("bias" %in% metrics)
      resScoringMatrix[,"bias"] <- scoringutils::bias_sample(true_values = real,predictions = predMatrix)

    if("pit" %in% metrics)
      resScoringMatrix[,"pit"] <- scoringutils::pit_sample(true_values = real,predictions = predMatrix)

    predMatrix <- rowMeans(predMatrix)

  } else if(hasArg(SD)){
    if("crps" %in% metrics)
      resScoringMatrix[,"crps"] <- scoringRules::crps_norm(real,predMatrix,sd = SD)
    if("dss" %in% metrics)
      resScoringMatrix[,"dss"] <- scoringRules::dss_norm(real,predMatrix,sd = SD)
    if("log" %in% metrics)
      resScoringMatrix[,"log"] <- scoringRules::logs_norm(real,predMatrix,sd = SD)
    if("coverage"  %in% metrics)
      resScoringMatrix[,"coverage"] <- qnorm(0.25,predMatrix,SD) < real & real < qnorm(0.25,predMatrix,SD,lower.tail = F)

    if("pit" %in% metrics)
      resScoringMatrix[,"pit"] <- NA
  }
  if("rmse" %in% metrics)
    resScoringMatrix[,"rmse"] <- sqrt(mean((real- predMatrix)^2))

  if("mae" %in% metrics)
    resScoringMatrix[,"mae"] <- mean(abs(real -predMatrix))

  if("rsq" %in% metrics)
    resScoringMatrix[,"rsq"] <- cor(predMatrix,real)^2

  return(resScoringMatrix)
}


.predanMetricTable <- function(jaspResults,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  metricSummaryTable <- createJaspTable("Forecast Verification Metric Table")
  metricSummaryTable$dependOn(c("checkPerformBma","bmaTestPeriod","bmaTestProp"))
  scoreSummary <- as.data.frame(t(sapply(jaspResults[["predanResults"]][["cvResultsState"]]$object, function(x) colMeans(x$scoringSummary))))

  metricSummaryTable$addColumnInfo(name= "model", title = "", type = "string")
  if(options$metricCrps)      metricSummaryTable$addColumnInfo(name = "crps"    , title = "CRPS"                    ,type = "number")
  if(options$metricDss)       metricSummaryTable$addColumnInfo(name = "dss"     , title = "DSS"                     ,type = "number")
  if(options$metricLog)       metricSummaryTable$addColumnInfo(name = "log"     , title = "Log score"               ,type = "number")
  if(options$metricCoverage)  metricSummaryTable$addColumnInfo(name = "coverage", title = "Coverage"                ,type = "number")
  if(options$metricBias)      metricSummaryTable$addColumnInfo(name = "bias"    , title = "Bias"                    ,type = "number")
  if(options$metricPit)       metricSummaryTable$addColumnInfo(name = "pit"     , title = "PIT"                     ,type = "number")
  if(options$metricMae)       metricSummaryTable$addColumnInfo(name = "mae"     , title = "MAE"                     ,type = "number")
  if(options$metricRmse)      metricSummaryTable$addColumnInfo(name = "rmse"    , title = "RMSE"                    ,type = "number")
  if(options$metricR2)        metricSummaryTable$addColumnInfo(name = "r2"      , title = gettextf("R%s", "\u00B2") ,type = "number")


  metricSummaryTable[["model"]]     <-  rownames(scoreSummary)
  metricSummaryTable[["crps"]]      <-  scoreSummary$"crps"
  metricSummaryTable[["dss"]]       <-  scoreSummary$"dss"
  metricSummaryTable[["log"]]       <-  scoreSummary$"log"
  metricSummaryTable[["coverage"]]  <-  scoreSummary$"coverage"
  metricSummaryTable[["bias"]]      <-  scoreSummary$"bias"
  metricSummaryTable[["pit"]]       <-  scoreSummary$"pit"
  metricSummaryTable[["mae"]]       <-  scoreSummary$"mae"
  metricSummaryTable[["rmse"]]      <-  scoreSummary$"rmse"
  metricSummaryTable[["r2"]]        <-  scoreSummary$"rsq"

  if(options$"checkPerformBma" && !is.null(jaspResults[["predanResults"]][["bmaResState"]])){

    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object

    scoreSum <- rowMeans(bmaRes$scores,na.rm = T)
    metricSummaryTable$addRows(list(
      "model" = "BMA",
      "crps"  = scoreSum["crps"],
      "dss" = scoreSum["dss"],
      "log" = scoreSum["log"],
      "coverage"  =scoreSum["coverage"],
      "bias"  =scoreSum["bias"],
      "pit" = scoreSum["pit"],
      "mae" =scoreSum["mae"],
      "rmse"  =scoreSum["rmse"],
      "r2"  =scoreSum["rsq"]
    ))

  }

  jaspResults[["predanMainContainer"]][["cvContainer"]][["metricTable"]] <- metricSummaryTable


  return()
}


.predanForecastVerificationResultPlot <- function(jaspResults,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(#is.null(jaspResults[["predanMainContainer"]][["cvContainer"]][["cvResPlot"]]) &&
     length(options$"modelsToPlot") >1){


    cvRes <- jaspResults[["predanResults"]][["cvResultsState"]]$object
    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object
    cvPlan <- jaspResults[["predanResults"]][["cvPlanState"]]$object

    mods <- names(cvRes)
    modsFull <- sapply(cvRes, "[","modelName")
    plotMods <- mods[which(modsFull %in% options$"modelsToPlot")]
    slices <- dimnames(cvRes[[1]]$realMatrix)[[2]]

    cvResPlot <- createJaspPlot(title = "CV Plots",width = 720,height = 180*length(slices))
    cvResPlot$dependOn(c("modelsToPlot","checkPerformBma"))

    mods <- names(cvRes)
    modsFull <- sapply(cvRes, "[","modelName")


    print(plotMods)
    print(slices)
    ##TODO choice for equal or unequal t diff
    spread_equal <- T


    t_var <- ifelse(spread_equal,"tt",time)
    sumPred <- lapply(cvRes,function(x) x$predSummary)
    dataPlot <- lapply(cvPlan, function(x) data.frame( tt = c(x$analysis,x$assessment),
                                                       type = rep(c("Analysis","Assessment"),
                                                                  c(length(x$analysis),length(x$assessment)))))


    realMatrix <- cvRes[[1]]$realMatrix
    dataPlot <- dplyr::bind_rows(.id = "slice",dataPlot)
    realData <- as.data.frame.table(realMatrix)

    dataPlot$real <- NA
    dataPlot$real[dataPlot$type == "Assessment"] <- realData$Freq
    dataPlot$real[dataPlot$type == "Analysis"] <- dataEng$y[dataPlot$tt[dataPlot$type == "Analysis"]]
    dataPlot$time <- dataEng$time[dataPlot$tt]


    predSummArray <-  sapply(cvRes,FUN =  function(x) x$predSummary,simplify = "array",USE.NAMES = T)
    dimnames(predSummArray)[3] <- list(dimnames(realMatrix)[[2]])


    pred <- cbind(as.data.frame.table(predSummArray[,1,,],responseName = "pred"),
                    upr = as.data.frame.table(predSummArray[,2,,])$Freq,
                    lwr = as.data.frame.table(predSummArray[,3,,])$Freq)

    #if(length(mods)==1)
    #  pred <- cbind(pred,model = mods)[]


    colnames(pred)[1:3] <- c("tt","slice","model")
    pred$tt <- dataPlot$tt[dataPlot$type == "Assessment"]


    dataPlot <- dplyr::left_join(dataPlot,pred)
    dataPlot$type <- "Actual"

    #BMA

    if(!is.null(jaspResults[["predanResults"]][["bmaResState"]]) && "BMA" %in% options$"modelsToPlot"){

      bma <- jaspResults[["predanResults"]][["bmaResState"]]$object

      bmaRes <- bma$res
      bmaDat <- sapply(X = 1:(length(bmaRes)-1), function(x) bmaRes[[x]]@predTest[,1,])

    bmaPred  <- as.data.frame.table(bmaDat)
    bmaSlices <- (unique(dataPlot$slice))
    if(all(is.na(bma$scores[,1])))
      bmaSlices <- bmaSlices[-1]

    ttBma <- unlist(lapply(cvPlan,function(x) tail(x$assessment,nrow(bmaPred)/length(bmaSlices))))

    bmaData <- subset(dataPlot,model == plotMods[1] & slice %in% bmaSlices &  tt %in% ttBma)
    bmaData$pred <- bmaPred$Freq
    bmaData$slice <-  rep(bmaSlices,each = nrow(bmaPred)/length(bmaSlices))
    bmaData[,c("upr","lwr")] <- NA
    bmaData$model <- "BMA"
    #View(bmaData)
    dataPlot <- rbind(dataPlot,bmaData)
    plotMods <- c(plotMods,"BMA")

    }
    #View(dataPlot)
    dataPlot <- subset(dataPlot,slice %in% slices & (model %in% plotMods | is.na(model)))
    xBreaks <- pretty(dataPlot[[t_var]])
    yBreaks <- pretty(dataPlot$real)

    p <- ggplot2::ggplot(dataPlot,ggplot2::aes_string(t_var,"real",color = "type")) + ggplot2::geom_line() +
      ggplot2::geom_line(ggplot2::aes(tt,pred,color=model)) + ggplot2::facet_wrap(facets  = "slice",ncol = 1) +
      ggplot2::coord_cartesian(ylim = range(yBreaks)) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
      ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid,
                     panel.background = ggplot2::element_rect(fill = "white"),
                     legend.position = "bottom",legend.title = ) +
      jaspGraphs::scale_JASPcolor_discrete("viridis") +
      ggplot2::labs(color = "Type")

    cvResPlot$plotObject <- p
    jaspResults[["predanMainContainer"]][["cvContainer"]][["cvResPlot"]] <- cvResPlot
  }

  return()
}

.ebmaHelper <- function(predSumArray,
                       realArray,
                       methodBMA = c("EM","gibbs"),
                       testMethod = c("next","in"),
                       inPercent = 0.3,retrain = T,parallel = T){

  testMethod <- match.arg(testMethod)
  if(testMethod == "in"){
    nTrain <- 1:round(nrow(realArray)*(1-inPercent),0)
    nTest <- tail(nTrain+1,1):nrow(realArray)
    nSlice <- 0
  } else{
    nTrain <- nTest <- 1:nrow(realArray)
    nSlice <- 1
  }

  #predBmaArray <-
  resList <- list()

  if(!parallel){
    future::plan(future::sequential)
  } else{
    ifelse(Sys.info()["sysname"] == "Windows",
           future::plan(future::multisession,workers = 5),
           future::plan(future::multicore,workers = 5))
  }

  resList <- future.apply::future_lapply(X = 1:dim(realArray)[2],FUN = function(i){

    iTest <- i + nSlice
    if(i != dim(realArray)[2] | testMethod == "in"){
      bmaData <- EBMAforecast::makeForecastData(.predCalibration = predSumArray[nTrain,1,i,],
                                                .outcomeCalibration  = realArray[nTrain,i,1],
                                                .predTest = predSumArray[nTest,1,iTest,],
                                                .outcomeTest = realArray[nTest,iTest,1],
                                                .modelNames = dimnames(realArray)[[3]])
    } else {
      bmaData <- EBMAforecast::makeForecastData(.predCalibration = predSumArray[nTrain,1,i,],
                                                .outcomeCalibration  = realArray[nTrain,i,1],
                                                .modelNames = dimnames(realArray)[[3]])
    }

    bmaRes <- EBMAforecast::calibrateEnsemble(bmaData,model = "normal",method = methodBMA,tol	= 0.05,useModelParams =F)
  })
  future::plan(future::sequential)

  dimsMetric <- ifelse(testMethod == "next", length(resList)-1,length(resList))

  if(methodBMA == "gibbs"){
    scores <- sapply(X = 1:dimsMetric,
                              function(x) colMeans(.scorePred(resList[[x]]@predTest[,1,],
                                                             resList[[x]]@outcomeTest,
                                                             SD = sqrt(resList[[x]]@variance))))
  } else{
    scores <- sapply(X = 1:dimsMetric, function(x) colMeans(.scorePred(resList[[x]]@predTest[,1,],
                                                                                          resList[[x]]@outcomeTest,
                                                                                          SD =sqrt(resList[[x]]@variance))))
  }

  if(testMethod == "next")
    scores <- cbind(NA,scores)

  weightMatrix <- sapply(resList,function(x) x@modelWeights)
  return(list(res = resList,scores = scores,weightMatrix = weightMatrix))
}

.predanBMAHelperResults <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(is.null(jaspResults[["predanResults"]][["bmaResState"]]) && options$checkPerformBma){

    bmaResState <- createJaspState()
    bmaResState$dependOn(c("checkPerformBma","bmaMethod","bmaTestPeriod","bmaSameSlice","selectedModels"))

    cvRes <- jaspResults[["predanResults"]][["cvResultsState"]]$object

    predSumArray <-   sapply(cvRes,FUN =  function(x) x$predSummary,simplify = "array")
    realArray <- sapply(cvRes, function(x) x$realMatrix,simplify = "array")


    bmaMethod <- switch (options$"bmaMethod",
       "bmaMethodEm" = "EM",
       "bmaMethodGibbs" = "gibbs"
    )

    bmaTestMethod <- switch (options$"bmaTestPeriod",
       "bmaTestNextSlice" = "next",
       "bmaSameSlice" = "in"
    )

    bmaRes <- .ebmaHelper(predSumArray = predSumArray,
                          realArray = realArray,
                          methodBMA = bmaMethod,
                          testMethod = bmaTestMethod,inPercent = options$bmaTestProp,
                          parallel = F)

    bmaResState$object <- bmaRes

    .predanMetricTable(jaspResults = jaspResults,options = options,ready = ready)

    jaspResults[["predanResults"]][["bmaResState"]] <- bmaResState
    jaspResults[["plottableModelsQml"]] <- createJaspQmlSource(sourceID="plottableModelsQml", value= c(options$selectedModels,"BMA"))

    if("BMA" %in% options$"modelsToPlot")
      .predanForecastVerificationResultPlot(jaspResults,options,ready)

  }
  return()
}


.predanBMAWeightsTable <-function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["bmaResState"]])) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["bmaWeightsTable"]]) &&
     options$"bmaWeightsTable"){
    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object

    bmaWeightsTable <- createJaspTable(title = "BMA - Model Weights")
    bmaWeightsTable$dependOn(c("bmaWeightsTable","bmaWeightsTablePerSlice"))
    weightMatrix <- bmaRes$weightMatrix
    bmaWeightsTable$addColumnInfo(name="model",  title="Model", type="string")
    if(options$"bmaWeightsTablePerSlice") {
      start <- ifelse(options$bmaTestPeriod == "bmaTestNextSlice",1,1)
      for (i in start:ncol(weightMatrix)) {
        bmaWeightsTable$addColumnInfo(name=paste0("slice",i),  title=paste0("Slice ",i), type="number")
      }
    } else{
      bmaWeightsTable$addColumnInfo(name="weights",  title= "Weights", type="number")
    }

    bmaWeightsTable[["model"]] <- rownames(weightMatrix)

    if(options$"bmaWeightsTablePerSlice") {

      start <- ifelse(options$bmaTestPeriod == "bmaTestNextSlice",1,1)

      for (i in start:ncol(weightMatrix)) {
        bmaWeightsTable[[paste0("slice",i)]] <- weightMatrix[,i]
      }

    } else{
      bmaWeightsTable[["weights"]] <- rowMeans(weightMatrix)
    }

    jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["bmaWeightsTable"]] <- bmaWeightsTable

  }

  return()
}



###### future predictions


#### helper functions
.makeEmptyFutureFrame <- function(dataEng,options){

  if(options$"futurePredPredictionHorizon" == "days"){
    maxPeriods <- 24*60*60*options$"futurePredictionDays" /33
  } else{
    maxPeriods <- options$"futurePredictionPoints" + 1
  }

  futureFrame <- data.frame(y = NA,
                            time = seq(max(dataEng$time),length.out=maxPeriods,by=33))[-1,]

  if(options$"futurePredPredictionHorizon" == "days"){
    futureFrame <- futureFrame[as.numeric(format(futureFrame$time, "%H")) >= 8 &
                                 as.numeric(format(futureFrame$time, "%H")) < 16 &
                                 as.numeric(format(futureFrame$time, "%d")) < as.numeric(format(max(dataEng$time), "%d")) + options$futurePredictionDays,]
  }


  start <- dataEng$time[which.max(as.numeric(format(dataEng$time, "%d")))]


  startOfDays  <- c(start,futureFrame$time[!duplicated(format(futureFrame$time, "%d"))][-1])

  #stop(gettext(paste0(startOfDays)))


  futureFrameBefore <- dataEng[which(format(dataEng$time, "%d") == max(format(dataEng$time, "%d"))) ,c("y","time")]
  futureFrameBefore$predict <- 0
  futureFrame$predict <- 1

  futureFrame <- rbind(futureFrameBefore,futureFrame)

  print(startOfDays)
  if(TRUE){

    cols <- c(colnames(futureFrame),
              c("n_since_day","n_since_jump","t_since_jump",'t_since_day')[
                c("n_since_day","n_since_jump","t_since_jump",'t_since_day')
                %in% colnames(dataEng)])

    futureFrame$n_since_day <- 0
    futureFrame$n_since_day[futureFrame$time %in% startOfDays] <- 1

    futureFrame$t_since_jump <- .getTimeSinceEvent(futureFrame$n_since_day,futureFrame$time,output = "time")
    futureFrame$n_since_jump <- .getTimeSinceEvent(futureFrame$n_since_day,futureFrame$time,output = "event")

    futureFrame$t_since_day <- .getTimeSinceEvent(futureFrame$n_since_day,futureFrame$time,output = "time")
    futureFrame$n_since_day <- .getTimeSinceEvent(futureFrame$n_since_day,futureFrame$time,output = "event")

    #futureFrame <- futureFrame[cols]

    #View(futureFrame)
    #stop(gettext(paste0(colnames(futureFrame))))

  }



  if("maakstation" %in% decodeColNames(options$factors)){

    modMat <- model.matrix(~.-1,data = data.frame( maakstation = as.factor(rep(c(1,2,3,4,5),length.out= nrow(futureFrame)))))

    futureFrame <- as.data.frame(cbind(futureFrame,modMat))
    if("maakstation99" %in% colnames(dataEng)) futureFrame$maakstation99 <- 0
  }

  #colnames(futureFrame) <- colnames(dataEng)[1:ncol(futureFrame)]


  if(options$featEngLags > 0)
    futureFrame <- cbind(futureFrame,as.data.frame(lagit(futureFrame$y,k = 1:options$featEngLags)))

  if(options$featEngAutoTimeBased)
    futureFrame <- cbind(futureFrame,get_timeseries_signature_date(futureFrame$time))

  futureFrame <- futureFrame[futureFrame$predict == 1,colnames(futureFrame) != "predict"]



  return(futureFrame)
}

.adjustFuturePredictions <- function(predList,dataEng,bmaRes = NULL,futureFrame,options){



  predSummary <- lapply(X = predList,
                        function(x)
                          data.frame(mean = rowMeans(x$pred$dist,na.rm = T),
                                     lowerCI = apply(x$pred$dist,1,quantile,probs= 0.025,na.rm = T),
                                     higherCI= apply(x$pred$dist,1,quantile,probs= 0.975,na.rm = T)
                          ))
  names(predSummary)
  predListAdjusted <- list()


  if(!is.null(bmaRes)){

    bmaRes <- bmaRes$res[[length(bmaRes)]]

    for(i in 1:3){
      d <- as.matrix(as.data.frame(sapply(predSummary,'[',i,simplify = "matrix")))
      colnames(d) <- bmaRes@modelNames
      predListAdjusted[[i]] <- EBMAforecast::EBMApredict(EBMAmodel = bmaRes,Predictions = d)@predTest[,,1]

    }

  } else{
    predListAdjusted <- lapply(1:3,function(i){
      d <- as.matrix(as.data.frame(sapply(predSummary,'[',i,simplify = "matrix")))
      colnames(d) <- names(predList)
      d
    })
  }
  #names(predListAdjusted) <- c("mean","lwr","upr")
  names(predListAdjusted) <- "x"

  predictionsCombined <- do.call(cbind,lapply(predListAdjusted,as.data.frame.table))[,c(1:3,6,9)]
  colnames(predictionsCombined) <- c("tt","model","mean","lwr","upr")
  predictionsCombined$time <- futureFrame$time


  predictionsCombined$tt <- NA
  return(predictionsCombined)
}

.getTimeSinceEvent <- function(var,time,output=c("time","event")){
  output <- match.arg(output)
  events <- which(var == 1)
  start <- events
  end <- c(events[-1]-1,length(var))
  seqs <- mapply(seq,start,end)
  if(output == "time")
    difs <- c(rep(0,start[1]-1),unlist(sapply(seqs, function(x) difftime(time[x],time[x[1]]))))
  else
    difs <- c(rep(0,start[1]-1),unlist(sapply(seqs, function(x) x - x[1])))
  return(difs)
}



.predanFuturePredictionResults <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(is.null(jaspResults[["predanResults"]][["futurePredState"]]) &&
             (options$"futurePredictionDays" > 0 || options$"futurePredictionPoints" > 0)){


    futurePredState <- createJaspState()
    futurePredState$dependOn(c("futurePredPredictionHorizon",
                               "futurePredictionDays",
                               "futurePredictionPoints",
                               "futurePredTrainingPeriod",
                               "futurePredTrainingPoints"))

    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object


    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object


    if(options$futurePredTrainingPeriod == "last") dataEng <- tail(dataEng,options$futurePredTrainingPoints)

    futureFrame <- .makeEmptyFutureFrame(dataEng = dataEng,options)


    modelList <- .createModelListHelper(dataEng,unlist(options$selectedModels))




    predList <- list()
    for (i in 1:length(modelList)) {
      predList[[i]] <- .predAnModelFit(trainData = dataEng,
                                       testData = futureFrame,
                                       predictFuture = T,
                                       method = modelList[[i]]$model,
                                       formula = modelList[[i]]$modelFormula,
                                       model_args =list())
    }
    names(predList) <- paste0(sapply(modelList,'[', "model"),1:length(modelList))

    predictionsCombined <- .adjustFuturePredictions(predList,dataEng,bmaRes,futureFrame = futureFrame,options)

    predictionsCombined$model <- as.character(predictionsCombined$model)
    predictionsCombined$model[predictionsCombined$model == "EBMA"] <- "BMA"
    futurePredState$object <- predictionsCombined


    jaspResults[["predanResults"]][["futurePredState"]] <- futurePredState

  }


  return()

}

.predanFuturePredictionPlot <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["futurePredState"]])) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["futurePredPlot"]])
     && options$"checkFuturePredictionPlot"){

    futurePredPlot <- createJaspPlot(title = "Future prediction plot", height = 480, width = 720)
    futurePredPlot$dependOn(c("checkFuturePredictionPlot","futurePredSpreadPointsEqually","selectedFuturePredictionModel"))


    predictionsCombined <- jaspResults[["predanResults"]][["futurePredState"]]$object
    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object

    dataOld <- cbind(dataEng[c('y', 'time')],upr= NA, lwr= NA,tt = NA,model = 'Actual')
    dataOld$tt <- 1:nrow(dataEng)



    colnames(dataOld)[1] <- 'mean'
    dataOld <- tail(dataOld,500)
    predictionsCombined <- rbind(dataOld,predictionsCombined)

    modelList <- .createModelListHelper(dataEng,options$selectedModels)


    modelNames <-  paste0(sapply(modelList,'[', "model"),1:length(modelList))
    names(modelNames) <- names(modelList)

    selectedModPlot <- modelNames[names(modelNames) ==options$"selectedFuturePredictionModel"]
    if("BMA" %in% unique(predictionsCombined$model)) selectedModPlot <- c(selectedModPlot,"BMA")

    predictionsCombined <- subset(predictionsCombined, model %in% c(selectedModPlot,"Actual"))

    t_var <- ifelse(options$futurePredSpreadPointsEqually,"tt","time")


    predanResults <-jaspResults[["predanResults"]][["predanBounds"]][["object"]]
    upperLimit <- predanResults[["upperLimit"]]
    lowerLimit <- predanResults[["lowerLimit"]]
    plotLimit <- predanResults[["plotLimit"]]


    xBreaks <- pretty(predictionsCombined[[t_var]])
    yBreaks <- pretty(plotLimit)





    p <- ggplot2::ggplot(predictionsCombined,ggplot2::aes_string(t_var,"mean",color = "model")) + ggplot2::geom_line() +
      jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     legend.position = "bottom") +
      ggplot2::scale_y_continuous(name = "Y",breaks = yBreaks,limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks)) +

      jaspGraphs::scale_JASPcolor_discrete("viridis") +
      ggplot2::labs(color = "Type") +
      ggplot2::geom_hline(na.rm = T,yintercept = upperLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_vline(xintercept = max(dataOld[[t_var]]),linetype="dashed") +
      ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                      plotLimit[[1]]))

    futurePredPlot$plotObject <- p

    jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["futurePredPlot"]] <- futurePredPlot

  }

  return()
}
.predanFuturePredictionTable <- function(jaspResults,dataset,options,ready){

}



