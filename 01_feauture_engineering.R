# 特徴量を加工する

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
data.train.raw <- fread("inputs/train.csv", data.table = F, stringsAsFactors = T)
data.test.raw <- fread("inputs/test.csv", data.table = F, stringsAsFactors = T)
# あとでデータを分割するために、行数を取得しておく
n.train <- nrow(data.train.raw)
n.test <- nrow(data.test.raw)
# train, testをくっつける
data.full <- bind_rows(data.train.raw, data.test.raw)

# 特徴量加工
# 敬称
data.full$Title <- gsub('(.*, )|(\\..*)', '', data.full$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
data.full$Title[data.full$Title == 'Mlle']        <- 'Miss' 
data.full$Title[data.full$Title == 'Ms']          <- 'Miss'
data.full$Title[data.full$Title == 'Mme']         <- 'Mrs' 
data.full$Title[data.full$Title %in% rare_title]  <- 'Rare Title'
# 家族の名字
data.full$Surname <- sapply(data.full$Name,  
                            function(x) strsplit(x, split = '[,.]')[[1]][1])
# 掛け算変数
data.full$Pclass_Age <- data.full$Pclass * data.full$Age
data.full$Pclass_SibSp <- data.full$Pclass * data.full$SibSp
data.full$Pclass_Parch <- data.full$Pclass * data.full$Parch
data.full$Pclass_Fare <- data.full$Pclass * data.full$Fare
data.full$Age_SibSp <- data.full$Age * data.full$SibSp
data.full$Age_Parch <- data.full$Age * data.full$Parch
data.full$Age_Fare <- data.full$Age * data.full$Fare
data.full$SibSp_Parch <- data.full$SibSp * data.full$Parch
data.full$SibSp_Fare <- data.full$SibSp * data.full$Fare
data.full$Parch_Fare <- data.full$Pclass * data.full$Fare

# # 家族について
data.full$Fsize <- data.full$SibSp + data.full$Parch + 1
data.full$Family <- paste(data.full$Surname, data.full$Fsize, sep='_')
data.full$FsizeD[data.full$Fsize == 1] <- 'singleton'
data.full$FsizeD[data.full$Fsize < 5 & data.full$Fsize > 1] <- 'small'
data.full$FsizeD[data.full$Fsize > 4] <- 'large'
# # 居室
data.full$Deck <- sapply(data.full$Cabin, function(x) strsplit(x, NULL)[[1]][1])
# # 人の属性など
data.full$Child[data.full$Age < 18] <- 'Child'
data.full$Child[data.full$Age >= 18] <- 'Adult'
data.full$Child[data.full$Age < 5] <- 'Baby'
data.full$Mother <- 'Not Mother'
data.full$Mother[data.full$Sex == 'female' & data.full$Parch > 0 & data.full$Age > 18 & data.full$Title != 'Miss'] <- 'Mother'
data.full$femaleChild <- ifelse(data.full %>% select(Sex) == "female" | data.full %>% select(Age) < 10, yes = 1, no = 0 ) %>% as.vector()
data.full$maleAdult <- ifelse(data.full %>% select(Sex) == "male" & (data.full %>% select(Age) > 20 & data.full %>% select(Age) < 35), yes = 1, no = 0 ) %>% as.vector()
data.full$Pclass_char <- data.full$Pclass %>% paste0("A")
# チケット番号
vector_ticket <- data.full$Ticket

# チケット番号
tic_num <- vector(length = length(vector_ticket))
tic_char <- vector(length = length(vector_ticket))
for(i in 1 : length(vector_ticket)){
  temp.tic <- strsplit(vector_ticket[i], "\\s") %>% unlist()
  if(length(temp.tic) >= 1){
    for(j in 1 : length(temp.tic)){
      if(is.na(as.numeric(temp.tic[j]))){
        tic_char[i] <- temp.tic[j]
      }else{
        tic_num[i] <- as.numeric(temp.tic[j])
      }
    }
  }else{
    tic_num[i] <- NA
    tic_char[i] <- NA
  }
}
data.full$tic_num <- tic_num
data.full$tic_char <- tic_char
table(data.full %>% select(Survived, tic_char))
data.full$tic_char[data.full$tic_char %in% c('A./5.', 'A.5.', 'A/5', 'A/5.')] <- 'A5'
data.full$tic_char[data.full$tic_char %in% c('A/4', 'A/4.', 'A4.')] <- 'A4'
data.full$tic_char[data.full$tic_char %in% c('C.A.', 'CA.', 'A4.')] <- 'CA'
data.full$tic_char[data.full$tic_char %in% c('F.C.', 'F.C.C.')] <- 'FC'
data.full$tic_char[data.full$tic_char %in% c('SC/Paris', 'SC/PARIS', 'S.C./PARIS')] <- 'SCPARIS'
data.full$tic_char[data.full$tic_char %in% c('S.C./A.4.', 'SC/A4')] <- 'SCA4'
data.full$tic_char[data.full$tic_char %in% c('W/C', 'W./C.')] <- 'WC'
data.full$tic_char[data.full$tic_char %in% c('WE/P', 'W.E.P.')] <- 'WEP'
data.full$tic_char[data.full$tic_char %in% c('SOTON/O2', 'STON/O2.')] <- 'SOTONO2'
data.full$tic_char[data.full$tic_char %in% c('SOTON/O.Q.', 'SOTON/OQ', 'STON/OQ.')] <- 'SOTONOQ'
# キャビン関係
cabin_n <- c(NA, length = dim(data.full)[1])
cabin_class_first <- c(NA, length = dim(data.full)[1])
cabin_class_second <- c(NA, length = dim(data.full)[1])
cabin_num <- c(NA, length = dim(data.full)[1])
for(i in 1 : dim(data.full)[1]){
  temp_cabin <- strsplit(data.full$Cabin[i], "\\s") %>% unlist()
  cabin_n[i] <- length(temp_cabin)
  if(length(temp_cabin) != 0){
    cabin_class_first[i] <- gsub(pattern = "[0-9]",replacement = "",temp_cabin) %>% head(1)
    cabin_class_second[i] <- gsub(pattern = "[0-9]",replacement = "",temp_cabin) %>% tail(1)
    cabin_num[i] <- gsub(pattern = "[A-Z]",replacement = "",temp_cabin) %>% as.numeric() %>% mean()
  }else{
    cabin_class_first[i] <- NA
    cabin_class_second[i] <- NA
    cabin_num[i] <- NA
  }
}
data.full$cabin_n <- cabin_n
data.full$cabin_num <- cabin_num
data.full$cabin_class_first <- cabin_class_first
data.full$cabin_class_second <- cabin_class_second

# 生データからいらない変数は除去
data.full <- data.full %>% select(-Name, -Ticket) %>% as.data.frame()
# カテゴリカル変数の欠損には"unknown"を入れておく
for(i in 1 : ncol(data.full)){
  class.col <- class(data.full[, i])
  print(paste(i, class.col))
  switch (class.col,
    "numeric" = {data.full[is.na(data.full[, i]), i] <- -9999}
    ,"integer" = {data.full[is.na(data.full[, i]), i] <- -9999}
    ,"character" = {
      data.full[data.full[, i]=="" | is.na(data.full[, i]), i] <- "unknown"
      data.full[, i] <- data.full[, i] %>% as.factor()
    }
    ,"factor" = {
      data.full[, i] <- data.full[, i] %>% as.character()
      data.full[data.full[, i]=="" | is.na(data.full[, i]), i] <- "unknown"
      data.full[, i] <- data.full[, i] %>% as.factor()
    }
  )
}
# one hot encoding
data.full.s.m.m <- sparse.model.matrix(Survived~.-1, data = data.full)
# xgbに対応できるようにデータの型変換
data.full.xgbD <- xgb.DMatrix(data = data.full.s.m.m, missing = -9999, label = data.full$Survived)
# train, testにデータを分割する
train.xgbD <- slice(data.full.xgbD, 1:n.train)
test.xgbD <- slice(data.full.xgbD, (n.train+1):(n.train+n.test))

# モデリング----
# 事前設定パラメータ
cv.nrounds <- 10
cv.nfold <- 10
cv.stopping <- 10
# ハイパーパラメーター
hyperparamerters <- expand.grid(
  eta = seq(0.01, 0.5, by = 0.01),
  min_child_weight = seq(1, 1.1, by = 0.3),
  max_depth = seq(5, 15, by = 3),
  max_delta_step = seq(0, 0.2, by = 0.3),
  gamma = seq(0, 0.2, by = 0.3), 
  subsample = seq(1, 1.1, by = 0.3),
  colsample_bytree = seq(1, 1.1, by = 0.3)
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
