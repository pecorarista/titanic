# データ学習

trainXgb <- function(data.train, data.test, names.var, path.output){
  # 事前設定パラメータ
  cv.nfold <- 10
  cv.stopping <- 10
  # ハイパーパラメーター
  hyperparamerters <- expand.grid(
    nrounds = c(2, 5, 10, 20),
    eta = seq(0.1, 0.5, by = 0.1),
    min_child_weight = c(0.5, 1, 1.5),
    max_depth = seq(5, 15, by = 3),
    max_delta_step = c(0), # クラスのバランスが悪い時のみつかう
    gamma = c(0, 0.5, 1),
    subsample = c(0.5, 1, 1.5),
    colsample_bytree = c(0.5, 1, 1.5)
  )
  n.hyper <- nrow(hyperparamerters)
  v.hyper <- vector(length = n.hyper)
  # プログレスバー
  pb <- txtProgressBar(min = 1, max = n.hyper, style = 3)
  for(i in 1 : n.hyper){
    setTxtProgressBar(pb, i)
    param <- list(objective = "binary:logistic",
                  eval_metric = "auc",
                  max_depth = hyperparamerters$max_depth[i],
                  eta = hyperparamerters$eta[i],
                  gamma = hyperparamerters$gamma[i],
                  subsample = hyperparamerters$subsample[i],
                  colsample_bytree = hyperparamerters$colsample_bytree[i],
                  min_child_weight = hyperparamerters$min_child_weight[i],
                  max_delta_step = hyperparamerters$max_delta_step[i]
    )
    tmp.trained.model <- xgb.cv(data = data.train, missing = -9999, nrounds = hyperparamerters$nrounds[i]
                                , params = param, nfold = cv.nfold, early.stop.round = cv.stopping, verbose = F)
    tmp.auc <- tmp.trained.model %>% summarise(max = max(test.auc.mean))
    v.hyper[i] <- tmp.auc %>% as.numeric()
  }
  best.hyper <- which.max(v.hyper)
  param <- list(objective = "binary:logistic",
                eval_metric = "auc",
                max_depth = hyperparamerters$max_depth[best.hyper],
                eta = hyperparamerters$eta[best.hyper],
                gamma = hyperparamerters$gamma[best.hyper],
                subsample = hyperparamerters$subsample[best.hyper],
                colsample_bytree = hyperparamerters$colsample_bytree[best.hyper],
                min_child_weight = hyperparamerters$min_child_weight[best.hyper],
                max_delta_step = hyperparamerters$max_delta_step[best.hyper]
  )
  trained.model <- xgboost(data = data.train, missing = -9999, nrounds = hyperparamerters$nrounds[best.hyper]
                           , params = param, nfold = cv.nfold, verbose = F, prediction = T)
  prediction.value <- predict(object = trained.model, data.test)
  predition.01 <- ifelse(test = prediction.value > 0.5, 1, 0)
  # 後処理----
  # importance
  importance <- xgb.importance(names.var, model = trained.model)
  return(list(pred = predition.01, imp = importance, pram = cbind(hyperparamerters, v.hyper)))
}

trainGlmnet <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "glmnet",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 5
  )
  prediction.value <- predict(object = trained.model, data.test)
  coef.mat <- coef(trained.model$finalModel, trained.model$bestTune$lambda) %>% as.matrix() %>% as.data.frame()
  names(coef.mat) <- "coef"
  return(list(pred = prediction.value, imp = coef.mat))
}

trainRanger <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "ranger",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 5
  )
  prediction.value <- predict(object = trained.model, data.test)
  return(list(pred = prediction.value))
}

trainRf <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "rf",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 5
  )
  prediction.value <- predict(object = trained.model, data.test)
  return(list(pred = prediction.value))
}

trainRborist <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "Rborist",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 5
  )
  prediction.value <- predict(object = trained.model, data.test)
  return(list(pred = prediction.value))
}

trainXgbTreeCaret <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "xgbTree",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 3
  )
  prediction.value <- predict(object = trained.model, data.test)
  return(list(pred = prediction.value))
}

trainXgbLinerCaret <- function(data.train, data.test){
  tr = trainControl(
    method = "cv",
    number = 10,
    allowParallel = TRUE
  )
  trained.model <- train(
    as.factor(Survived) ~.,
    data = data.train,
    method = "xgbLinear",
    preProcess = c('center', 'scale'),
    trControl = tr,
    tuneLength = 3
  )
  prediction.value <- predict(object = trained.model, data.test)
  return(list(pred = prediction.value))
}
