#### bsts functions
.bstsFitHelper  <- function(trainData,formula,method,niter=500,keepModel =F, ...){
  ss <- list()
  y <- trainData[[all.vars(formula)[1]]]

  if(formula == as.formula("y ~ time"))
    formula <- trainData$y

  if(method == "bstsAr")
    ss <- bsts::AddAr(ss,y = y)
  else if(method == "bstsLinear")
    ss <- bsts::AddLocalLinearTrend(ss,y=y)

  model <- bsts::bsts(formula = formula,state.specification = ss,data = trainData,niter = niter,...)
  distr <-rowSums(aperm(model$state.contribution, c(3, 1, 2)), dims = 2)


  return(list(model=model,distr = distr))
}

.bstsPredictHelper <- function(model,testData, horizon,formula,...){


  if(formula == "y ~ time" && is.null(testData))
    pred <- bsts::predict.bsts(model,horizon = horizon,burn = 0)
  else if(formula == "y ~ time" && !is.null(testData))
    pred <- bsts::predict.bsts(model,horizon = nrow(testData),burn = 0)
  else
    pred <- bsts::predict.bsts(model,newdata = testData,burn = 0)


  dist <- t(pred$distribution)
  if(all(!is.na(testData$y)))
    return(list(pred = pred, dist = dist, trueVal = testData$y))

  return(list(dist = dist))

}


.prophetFitHelper <- function(trainData,formula,keepModel =F, ...){


  colnames(trainData)[colnames(trainData)=="time"] <- "ds"
  m <- prophet::prophet(...)
  if(formula != as.formula("y ~ time")){
    for(reg in labels(terms(formula))[-1])
      m <- prophet::add_regressor(m,reg)

  }

  model <- prophet::fit.prophet(m,trainData)

  distr <- prophet::predictive_samples(model,trainData)[["yhat"]]
  return(list(model = model,distr = distr))
}


.prophetPredictHelper <- function(fit,testData, horizon,recursive = F,...){

  if(is.null(testData))
    future <- prophet::make_future_dataframe(fit, periods = horizon,freq = 33,include_history = F)
  else if(!is.null(testData)){
    future <- data.frame(ds = testData$time)
    future[,names(fit$extra_regressors)] <- testData[,names(fit$extra_regressors)]
  }
  pred <- predict(fit,future)
  dist <- prophet::predictive_samples(fit,future)[["yhat"]]

  if(all(!is.na(testData$y)))
    return(list(pred = pred, dist = dist, trueVal = testData$y))
  return(list(dist = dist))
}



### lin_reg baseline


.lmSpikeFitHelper <-  function(trainData,formula,method,niter=2000,keepModel =F , ...){

  model <- BoomSpikeSlab::lm.spike(formula = formula,niter,data = trainData)
  return(list(model = model))
}


.lmSpikePredictHelper <- function(fit,testData, trainData,horizon,formula,niter =500,lags = NULL,...){

  pred <- BoomSpikeSlab::predict.lm.spike(fit,newdata = testData,burn = 0)


  dist <- pred
  if(all(!is.na(testData$y)))
    return(list(pred = pred, dist = dist, trueVal = testData$y))

  return(list(dist = dist))
}




.recursivePredict <- function(testData,trainData,formula, method,model,horizon = nrow(testData),lags = 10,model_args = list,...){

  if(all(!is.na(testData$y)))
    real <- testData$y
  else
    real <- NULL

  testData$y <- NA
  # set all lags to NA to avoid leakage
  testData[,grepl("y_lag",colnames(testData))] <- NA
  i = 1



  lag_temp <- lagit(c(tail(trainData$y,lags +1 -i),testData$y),1:lags)[lags +1,]


  testData[i,grepl("y_lag",colnames(testData))] <- lag_temp

  test_temp <- testData[i,]
  pred_temp <- switch (method,
                       "lmSpike" = do.call(.lmSpikePredictHelper,c(list(model,test_temp,trainData,formula,...),model_args)),
                       "bstsAr" = do.call(.bstsPredictHelper,c(list(model,test_temp,trainData,formula,...),model_args)),
                       "bstsLinear" = do.call(.bstsPredictHelper,c(list(model,test_temp,trainData,formula,...))),
                       "prophet" = do.call(.prophetPredictHelper,c(list(model,test_temp,trainData,formula,...))),
                       "xgboost" = do.call(.xgbPredHelper,list(model,test_temp,formula)))$dist

  # automatically initiate matrix size to be filled recursively since MCMC size not same argument across
  #View(pred_temp)
  predArray <- matrix(nrow = horizon,ncol = dim(pred_temp)[2])
  predArray[1,] <- pred_temp[1,]
  testData$y[1] <- mean(pred_temp)
  for (i in 2:horizon) {

    test_temp <- testData[i,]

    lag_temp <- lagit(c(tail(trainData$y,lags +1 -i),testData$y),1:lags)[lags +1,]
    testData[i,grepl("y_lag",colnames(testData))] <- lag_temp
    test_temp <- testData[i,]
    #lag_temp <- lagit(c(tail(trainData$y,lags +1 -i),testData$y),1:lags)[lags +1,]
    #test_temp[,grepl("y_lag",colnames(testData))] <- t(lag_temp)

    #lag_temp <- lagit(c(tail(trainData$y,lags +1 -i),testData$y),1:lags)[lags +1,-1]
    #test_temp <- cbind(testData[i,],data.frame(t(lag_temp)))

    predArray[i,] <- switch (method,
                             "lmSpike" = do.call(.lmSpikePredictHelper,c(list(model,test_temp,trainData,formula,...),model_args)),
                             "bstsAr" = do.call(.bstsPredictHelper,c(list(model,test_temp,trainData,formula,...),model_args)),
                             "bstsLinear" = do.call(.bstsPredictHelper,c(list(model,test_temp,trainData,formula,...))),
                             "prophet" = do.call(.prophetPredictHelper,c(list(model,test_temp,trainData,formula,...),model_args)),
                             "xgboost" = do.call(.xgbPredHelper,list(model,test_temp,formula)))$dist

    testData$y[i] <- mean(predArray[i,])
  }

  if(any(!is.null(real)))
    return(list( dist = predArray, trueVal = real))
  return(list(dist = predArray))
}



#technically faster than for loop but currently problematic as <<- tries to assign to global envir and not the one from which it is called
#lmSpikeRecursiveFit <- function(testData,trainData,model,horizon,lags = 20,i = NULL){
#
#  if(is.null(i)){
#
#    i = 1
#  }
#  lag_temp <- lagit(c(tail(trainData$y,lags +1 -i),testData$y),1:lags)[lags +1,-1]
#
#  if(all(class(model) == "xgb.Booster")){
#    d <- as.matrix(cbind(testData[i,colnames(testData) !="y"],data.frame(t(lag_temp))))
#    input <- xgb.DMatrix(d,label = testData$y[i])
#    pred[,i] <<- print(predict(model,input))
#  }
#  else {
#    pred[,i] <<- predict(model,cbind(testData[i,],data.frame(t(lag_temp))))
#  }
#  testData$y[i] <- mean(pred[,i])
#
#  print(i)
#  if(horizon == i)
#    return()
#  i = i+1
#  lmSpikeRecursiveFit(testData,trainData,model,horizon,lags,i = i)
#
#}


#xgboost

.xgbFitHelper <- function(trainData,formula,nrounds = 20,...){
  trainData$time <- as.numeric(trainData$time)

  args <- list(...)
  print(args)


  reg_vars <- colnames(trainData)[colnames(trainData)!="y" & colnames(trainData) %in% labels(terms(formula))]
  model <- xgboost::xgboost(data = as.matrix(trainData[,reg_vars]),nrounds = nrounds,nthread=1,
                            label = trainData$y,  metrics = list("rmse"), objective = "reg:squarederror",...)


  return(list(model=model))
}

.xgbPredHelper <- function(fit,testData,formula,...){
  testData$time <- as.numeric( testData$time )
  reg_vars <- colnames(testData)[colnames(testData)!="y" & colnames(testData) %in% labels(terms(formula))]

  pred <- matrix(predict(fit ,newdata = as.matrix(testData[,reg_vars])),nrow = nrow(testData))

  if(all(!is.na(testData$y)))
    return(list(dist = pred, trueVal = testData$y))
  return(list(dist = pred))

}