# データ確認

# 初期化
rm(list = ls())
gc();gc()

# パッケージ
require(data.table)
require(dplyr)
require(RMeCab)
require(ggplot2)
require(caret)
require(useful)
# データ読み込み
data.train.raw <- fread("inputs/train.csv")
data.test.raw <- fread("inputs/test.csv")

# trainとtestを結合
names(data.train.raw)
names(data.test.raw)
data.all.raw <- rbind(data.train.raw %>% select(-Survived), data.test.raw)
data.objective <- c(data.train.raw %>% select(Survived) %>% unlist(), rep(NA, nrow(data.test.raw)))
full <- data.frame(y = data.objective, data.all.raw) %>% as.data.table()

# 基礎集計
full %>% select(Age, y) %>% group_by(y) %>% summarise(n = mean(Age, na.rm = T))

# 特徴量作成----
# 爵位など
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full$Embarked[c(62, 830)] <- 'C'
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)
# 女子供フラグ
female_child_flag <- ifelse(full %>% select(Sex) == "female" | full %>% select(Age) < 10, yes = 1, no = 0 ) %>% as.vector()
# 成年男子フラグ
male_adult_flag <- ifelse(full %>% select(Sex) == "male" & (full %>% select(Age) > 20 & full %>% select(Age) < 35), yes = 1, no = 0 ) %>% as.vector()

# pclassを文字列化
Pclass_char <- full$Pclass %>% paste0("A")

# 名前でnグラム
# unigram_name <- RMeCab::docNgramDF(mojiVec = full$Name
#                                         , type = 1, pos = "名詞", minFreq = 1, N = 1)
# bygram_name <- RMeCab::docNgramDF(mojiVec = full$Name
#                                       , type = 1, pos = "名詞", minFreq = 1, N = 2)
# チケット番号
vector_ticket <- full$Ticket
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
# 部屋番号
cabin_n <- c(NA, length = dim(full)[1])
cabin_class_first <- c(NA, length = dim(full)[1])
cabin_class_second <- c(NA, length = dim(full)[1])
cabin_num <- c(NA, length = dim(full)[1])
for(i in 1 : dim(full)[1]){
  temp_cabin <- strsplit(full$Cabin[i], "\\s") %>% unlist()
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

# データ結合
data.all <- full %>% select(y, PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>% 
  mutate(Pclass_char = Pclass_char, tic_num = tic_num
         , tic_char = tic_char
         , cabin_n = cabin_n
         , cabin_class_first = cabin_class_first, cabin_class_second = cabin_class_second, cabin_num = cabin_num
         , female_child_flag = female_child_flag, male_adult_flag = male_adult_flag )
# 欠損代入
data.all$Age[is.na(data.all$Age)] <-  median(data.all$Age, na.rm = T)
Age_na <- ifelse(is.na(data.all$Age), 1, 0)
data.all$Fare[is.na(data.all$Fare)] <-  median(data.all$Fare, na.rm = T)
Fare_na <- ifelse(is.na(data.all$Fare), 1, 0)
data.all$cabin_class_first[is.na(data.all$cabin_class_first)] <-  median(data.all$cabin_class_first, na.rm = T)
cabin_class_first_na <- ifelse(is.na(data.all$cabin_class_first), 1, 0)
data.all$cabin_class_second[is.na(data.all$cabin_class_second)] <-  median(data.all$cabin_class_second, na.rm = T)
cabin_class_second_na <- ifelse(is.na(data.all$cabin_class_second), 1, 0)
data.all$cabin_num[is.na(data.all$cabin_num)] <-  median(data.all$cabin_num, na.rm = T)
cabin_num_na <- ifelse(is.na(data.all$cabin_num), 1, 0)
data.all$female_child_flag[is.na(data.all$female_child_flag)] <-  median(data.all$female_child_flag, na.rm = T)
female_child_flag_na <- ifelse(is.na(data.all$female_child_flag), 1, 0)
data.all$male_adult_flag[is.na(data.all$male_adult_flag)] <-  median(data.all$male_adult_flag, na.rm = T)
male_adult_flag_na <- ifelse(is.na(data.all$male_adult_flag), 1, 0)

data.all$y[is.na(data.all$y)] <-  -9999
data.all <- data.frame(data.all, Age_na, Fare_na, cabin_class_first_na , cabin_class_second_na, cabin_num_na, female_child_flag_na, male_adult_flag_na)

# onehotencoding
data.all <- data.frame(y = data.all$y, build.x(y~.-1, data.all %>% as.data.frame()))

data.train.bygram <- data.frame(data.all ) %>% filter(!y == -9999)
data.test.bygram <- data.frame(data.all ) %>% filter(y == -9999)
data.train.bygram$y <- as.factor(data.train.bygram$y)
write.csv(data.train.bygram, "train.csv")
write.csv(data.test.bygram, "test.csv")

# 変数の事前スクリーニング
res.var.imp <- vector(length = (ncol(data.train.bygram)))
res.var.imp[1] <- 0
for(i in 2 : (ncol(data.train.bygram))){
  temp_data <- data.train.bygram %>% select(1, i)
  n.unique <- nrow(unique(temp_data %>% select(-y)))
  if(n.unique == 1){ # 変数がユニークかどうか
    res.var.imp[i] <- 1
  }else{
    print(i)
    temp_model <- glm(formula = y~., temp_data, family = "binomial")
    li <- summary(temp_model)
    coefficients <- tail(li$coefficients[2, ], 1)
    res.var.imp[i] <- coefficients
  }
}
name.use <- names(data.train.bygram)[res.var.imp < 0.95]
# データ学習
xgb_grid_1 = expand.grid(
  nrounds = c(20, 30, 40), 
  max_depth = seq(5, 15, by = 2), 
  eta = seq(0.01, 0.2, by = 0.01), 
  gamma = seq(0.8, 1.2, by = 0.1), 
  colsample_bytree = seq(0.7, 1.2, by = 0.1), 
  min_child_weight = seq(0.7, 1.2, by = 0.1)
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 10,  
  allowParallel = TRUE
)
trained_model <- train(
  data = data.train.bygram,
  y ~.,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)
pred <- predict(object = trained_model, data.test.bygram)
prediction <- data.frame(PassengerId = data.test.raw$PassengerId, Survived = pred)
write.csv(prediction, file = "pred.csv", row.names = F)
