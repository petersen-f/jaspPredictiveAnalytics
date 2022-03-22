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
    jaspResults[["predanResults"]][["predanBounds"]] <- predanBoundsState
  }

}

.predanComputeBounds <- function(dataset,options) {


  dataControl <- data.frame(y = dataset[,options[["dependent"]]])
  dataControl$time <- 1:nrow(dataControl)
  upperLimit <- options$controlMean + options$controlError
  lowerLimit <- options$controlMean - options$controlError

  dataControl$outBound <- ifelse(dataControl$y > upperLimit | dataControl$y < lowerLimit,T,F)

  return(dataControl)
}

.predanPlotsDescriptives <- function(jaspResults,dataset,options,ready) {
  if(!ready) return()

  predanDescriptivePlots <- createJaspContainer(title=gettext("Descriptives"))

  controlData <- jaspResults[["predanResults"]][["predanBounds"]]$object

  ##TODO: add dependencies

  if(options$controlPlotCheck)
    .predanBasicControlPlot(jaspResults,predanDescriptivePlots,controlData,options)


  jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlot <- function(jaspResults,predanDescriptivePlots,controlData,options){

  upperLimit <- options$controlMean + options$controlError
  lowerLimit <- options$controlMean - options$controlError

  predanControlPlot <- createJaspPlot(title= "Basic Control Plot", height = 320, width = 480)
  p <- ggplot2::ggplot(controlData,ggplot2::aes(x = time,y= y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
    ggplot2::geom_line(size=1,ggplot2::aes(colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit)),
                       lineend = "round",linejoin ="round")+
    ggplot2::geom_point(size=2)+
    ggplot2::geom_hline(yintercept = upperLimit,linetype="dashed",color="darkred")+
    ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred")+
    ggplot2::scale_color_manual(name="Out of Bounds",values=c("#4E84C4","#D16103")) + ggplot2::theme_bw()
  predanControlPlot$plotObject <- p
  #jaspResults[["testPlot"]] <- predanControlPlot
  predanDescriptivePlots[["predanControlPlot"]] <- predanControlPlot
  return()
}





