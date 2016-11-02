# 欠損値とカテゴリカル変数をどうしましょう。
# カテゴリカル変数はone hot encodingするようだ。

# 初期化
rm(list = ls())
gc();gc()

# パッケージ
require(data.table)
require(dplyr)
require(xgboost)
require(caret)
require(tcltk)

# データ加工----
# データ読み込み
data.train.raw <- fread("inputs/train.csv", data.table = F)
data.test.raw <- fread("inputs/test.csv", data.table = F)
# あとでデータを分割するために、行数を取得しておく
n.train <- nrow(data.train.raw)
n.test <- nrow(data.test.raw)
# train, testをくっつける
data.full <- bind_rows(data.train.raw, data.test.raw)
# 欠損を表す極端な値を入れておく
data.full[is.na(data.full)] <- -9999
# one hot encoding
data.full.s.m.m <- sparse.model.matrix(Survived~.-1, data = data.full)
# xgbに対応できるようにデータの型変換
data.full.xgbD <- xgb.DMatrix(data = data.full.s.m.m, missing = -9999, label = data.full$Survived)
# train, testにデータを分割する
train.xgbD <- slice(data.full.xgbD, 1:n.train)
test.xgbD <- slice(data.full.xgbD, (n.train+1):(n.train+n.test))

# モデリング----
# 事前設定パラメータ
cv.nrounds <- 100
cv.nfold <- 10
cv.stopping <- 10
# ハイパーパラメーター
hyperparamerters <- expand.grid(
  max_depth = seq(5, 15, by = 3),
  eta = seq(0.01, 0.1, by = 0.001),
  gamma = seq(1, 1.1, by = 0.2), 
  subsample = seq(1, 1.1, by = 0.2),
  colsample_bytree = seq(1, 1.1, by = 0.2), 
  min_child_weight = seq(1, 1.1, by = 0.2),
  max_delta_step = seq(1, 1.1, by = 0.2)
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
  tmp.trained.model <- xgb.cv(data = train.xgbD, missing = -9999, nrounds = cv.nrounds
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
trained.model <- xgboost(data = train.xgbD, missing = -9999, nrounds = cv.nrounds
                            , params = param, nfold = cv.nfold, verbose = F, prediction = T)
prediction.value <- predict(object = trained.model, test.xgbD)
predition.01 <- ifelse(test = prediction.value > 0.5, 1, 0)
prediction.test <- data.frame(PassengerId = data.test.raw$PassengerId, Survived = predition.01)
write.csv(prediction.test, file = "prediction.csv", row.names = F)

# 後処理----
# importance
importance <- xgb.importance(data.full.s.m.m@Dimnames[[2]], model = trained.model)
xgb.plot.importance(importance_matrix = importance)

