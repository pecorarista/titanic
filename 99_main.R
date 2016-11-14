# 初期化
rm(list = ls())
gc();gc()

# パッケージ
require(data.table)
require(dplyr)
require(xgboost)
require(caret)
require(Matrix)
require(useful)
require(tcltk)
# install.packages('rstan', repos='https://cloud.r-project.org/', dependencies=TRUE)
require(rstan)
# library(doParallel)
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)

# 関数読み込み
source("01_feauture_engineering.R")
source("02_preprocessing.R")
source("03_train_models.R")

# 第一弾の特徴量加工
data.has.feauture <- createRawfeauture(train = "inputs/train.csv", test = "inputs/test.csv")

# 前処理
data.list.xgb <- preProcessRawFeauture(method = "xgb", data.full = data.has.feauture)
data.train.xgb <- data.list.xgb[[1]]
data.test.xgb <- data.list.xgb[[2]]
names.var.xgb <- data.list.xgb[[3]]

# モデリング-xgb
list.res.xgb <- trainXgb(data.train = data.train.xgb, data.test = data.test.xgb, names.var = names.var.xgb, path.output = "outputs/")
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.xgb$pred)
write.csv(prediction, file = "predictionFiles/pred_xgb.csv", row.names = F)

#　変数重要度
name.file <- paste0("outputs/imp.png")
png(filename = name.file)
xgb.plot.importance(importance_matrix = list.res.xgb$imp)
dev.off()

# 前処理
data.list.mi <- preProcessRawFeauture(method = "mi-one-hot", data.full = data.has.feauture)
data.train.mi <- data.list.mi[[1]]
data.test.mi <- data.list.mi[[2]]
names.var.mi <- data.list.mi[[3]]

# モデリング-glmnet
list.res.glmnet <- trainGlmnet(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.glmnet$pred)
write.csv(prediction, file = "predictionFiles/pred_glmnet.csv", row.names = F)

# # 係数
# coefficients <- list.res.glmnet$imp
# coefficients %>% filter(coef > 0)

# モデリング-rf
list.res.rf <- trainRf(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.rf$pred)
write.csv(prediction, file = "predictionFiles/pred_rf.csv", row.names = F)

# モデリング-ranger
list.res.ranger <- trainRanger(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.ranger$pred)
write.csv(prediction, file = "predictionFiles/pred_ranger.csv", row.names = F)

# モデリング-rborist
list.res.rborist <- trainRborist(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.rborist$pred)
write.csv(prediction, file = "predictionFiles/pred_rborist.csv", row.names = F)

# モデリング-xgbliner
list.res.xgbliner <- trainXgbLinerCaret(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.xgbliner$pred)
write.csv(prediction, file = "predictionFiles/pred_xgbliner.csv", row.names = F)

# モデリング-xgbtree
list.res.xgbtree <- trainXgbTreeCaret(data.train = data.train.mi, data.test = data.test.mi)
prediction <- data.frame(PassengerId = 892:1309, Survived = list.res.xgbtree$pred)
write.csv(prediction, file = "predictionFiles/pred_xgbtree.csv", row.names = F)