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


.predanComputeResults <- function(jaspResults, dataset, options,ready) {

  ##TODO: store limits somehwere properly and not in DF - maybe list?
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
    predanBoundsState$dependOn(c("errorBoundMethodDrop","manualBoundMethod","manualBoundMean","manualBoundErrorBound","manualUpperBound","manualUpperLower","sigmaBound","controlPeriodCheck","controlPeriodStart","controlPeriodEnd","trimmedMeanCheck","trimmedMeanPercent"))
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


.predanComputeBinaryResults <- function(jaspResults,dataset,options,ready){
  if(!ready | !options$binaryControlChartCheck) return()

  if (is.null(jaspResults[["predanResults"]][["predanBinaryBounds"]])){
    predanBinaryBoundsState <- createJaspState()

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

