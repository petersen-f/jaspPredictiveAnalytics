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
    #.predanBinaryControlChart(jaspResults,dataset,options,ready)
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
    predanBoundsState$dependOn(c("errorBoundMethod","controlMean","controlError","sigmaBound","controlPeriodCheck","controlPeriod"))
    jaspResults[["predanResults"]][["predanBounds"]] <- predanBoundsState
  }

}

.predanComputeBounds <- function(dataset,options) {

  dataControl <- data.frame(y = dataset[,options[["dependent"]]])
  dataControl$time <- 1:nrow(dataControl)

  if(options$errorBoundMethod == "manualBound") {

    upperLimit <- options$controlMean + options$controlError
    lowerLimit <- options$controlMean - options$controlError
    plotLimit <- c(options$controlMean + 2*options$controlError,options$controlMean - 2*options$controlError)
  } else {
    controlPeriod <- seq_len(ifelse(options$controlPeriodCheck,
                                    options$controlPeriod,
                                    nrow(dataControl)))
    upperLimit <- mean(dataControl$y[controlPeriod]) + sd(dataControl$y[controlPeriod])*options$sigmaBound
    lowerLimit <- mean(dataControl$y[controlPeriod]) - sd(dataControl$y[controlPeriod])*options$sigmaBound
    plotLimit <- c(mean(dataControl$y[controlPeriod]) + 2*sd(dataControl$y[controlPeriod])*options$sigmaBound,
                   mean(dataControl$y[controlPeriod]) - 2*sd(dataControl$y[controlPeriod])*options$sigmaBound)
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

  if(options$zoomControlPlotCheck && options$zoomControlLength >0)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivePlots,options,zoom=T)


  jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlot <- function(jaspResults,predanResults,predanDescriptivePlots,options,zoom){

  controlData <- predanResults[["dataControl"]]

  upperLimit <- predanResults[["upperLimit"]]
  lowerLimit <- predanResults[["lowerLimit"]]
  plotLimit <- predanResults[["plotLimit"]]
  if(zoom)
    controlData <- tail(controlData,options$zoomControlLength)


    predanControlPlot <- createJaspPlot(title= "Basic Control Plot", height = 320, width = 480)
    p <-ggplot2::ggplot(controlData,ggplot2::aes(time,y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
      ggplot2::geom_hline(yintercept = upperLimit,linetype="dashed",color="darkred")+
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred")+
      ggplot2::scale_color_manual(name="Out of Bounds",values=c("#4E84C4","#D16103")) + ggplot2::theme_bw()


  if(options$controlLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=1,ggplot2::aes(colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit)),
                                lineend = "round",linejoin ="round")
  if(options$controlLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=2)

  if(options$xAxisLimit == "controlBounds")
    p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                             plotLimit[[1]]))

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
    if(options$binaryControlMethod=="state space")
      predanBinaryBounds <- .predanBinaryStateSpaceResults(jaspResults,controlData,dataset,options)
    else if (options$binaryControlMethod == "beta distribution")
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

  ts.model <- bsts::bsts(y , ss, niter = 4000,
                   family = "logit")

  return(ts.model)

}








#.predanBinaryControlChart <- function(jaspResults,dataset,options,ready) {
#  if(!ready | !options$binaryControlChartCheck) return()
#
#  predanBinaryControl <- createJaspContainer(title=gettext("BinaryControl"))
#
#  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
#
#  if(options$binaryControlMethod == "state space"){
#    .predanBinaryStateSpaceResultsHelper(jaspResults,predanResults,dataset,options)
#    .predanBinaryControlStateSpacePlot(jaspResults,predanResults,dataset,options)
#  }



#  return()
#}
#
#.predanBinaryControlStateSpacePlot <- function(jaspResults,predanResults,dataset,options) {
#
#
#}

