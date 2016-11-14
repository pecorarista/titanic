# 手法に合わせた前処理を行う

# xgb :カテゴリカル変数の欠損をunknownで埋める。数値型は-9999を代入。その後はsparse model matrix型に変換（同時にone hot encoding）
# mi-one-hot : カテゴリカル変数の欠損をunknownで埋める。数値型はhogeで代入。その後はone hot encoding
preProcessRawFeauture <- function(method, data.full){
  # あとでデータを分割するために、行数を取得しておく
  n.train <- nrow(data.full) - sum(is.na(data.full$Survived))
  n.test <- sum(is.na(data.full$Survived))
  switch (method,
          "xgb" = {
            for(i in 1 : ncol(data.full)){
              class.col <- class(data.full[, i])
              name.col <- names(data.full %>% select(i))
              if(name.col == "Survived"){
                # 欠損に代入するだけ
                data.full[is.na(data.full[, i]), i] <- -9999
                next
              }
              # nullフラグを作る
              idx.null <- ifelse(is.na(data.full[, i]), 1, 0)
              if(sum(idx.null) > 0){
                data.full <- data.frame(data.full, idx.null)
                names(data.full)[ncol(data.full)] <- paste0(names(data.full)[i], "_null")
                if(class.col %in% c("numeric", "integer")){
                  data.full[is.na(data.full[, i]), i] <- -9999
                  next
                }
                if(class.col %in% c("factor", "character")){
                  data.full[, i] <- data.full[, i] %>% as.character()
                  data.full[data.full[, i]=="" | is.na(data.full[, i]), i] <- "unknown"
                  data.full[, i] <- data.full[, i] %>% as.factor()
                }
              }else{
                if(class.col == "character"){
                  data.full[, i] <- data.full[, i] %>% as.factor()
                }
              }
            }
            # one hot encoding
            data.full.s.m.m <- sparse.model.matrix(Survived~.-1, data = data.full)
            # xgbに対応できるようにデータの型変換
            data.full.xgbD <- xgb.DMatrix(data = data.full.s.m.m, missing = -9999, label = data.full$Survived)
            # train, testにデータを分割する
            train.xgbD <- slice(data.full.xgbD, 1:n.train)
            test.xgbD <- slice(data.full.xgbD, (n.train+1):(n.train+n.test))
            return(list(train = train.xgbD, test = test.xgbD, names = data.full.s.m.m@Dimnames[[2]]))
          }
          ,"mi-one-hot" = {
            for(i in 1 : ncol(data.full)){
              class.col <- class(data.full[, i])
              name.col <- names(data.full %>% select(i))
              if(name.col == "Survived"){
                # 欠損に代入するだけ
                data.full[is.na(data.full[, i]), i] <- -9999
                next
              }
              # nullフラグを作る
              idx.null <- ifelse(is.na(data.full[, i]), 1, 0)
              if(sum(idx.null) > 0){
                data.full <- data.frame(data.full, idx.null)
                names(data.full)[ncol(data.full)] <- paste0(names(data.full)[i], "_null")
                if(class.col %in% c("numeric", "integer")){
                  data.full[is.na(data.full[, i]), i] <- mean(data.full[, i], na.rm = T)
                  next
                }
                if(class.col %in% c("factor", "character")){
                  data.full[, i] <- data.full[, i] %>% as.character()
                  data.full[data.full[, i]=="" | is.na(data.full[, i]), i] <- "unknown"
                  data.full[, i] <- data.full[, i] %>% as.factor()
                }
              }else{
                if(class.col == "character"){
                  data.full[, i] <- data.full[, i] %>% as.factor()
                }
              }
            }
            # one hot encoding
            data.full.b.x <- build.x(data = data.full, formula = Survived~.-1)
            data.full.b.y <- build.y(data = data.full, formula = Survived~.-1)
            data.full.b <- cbind(Survived = data.full.b.y, data.full.b.x)
            # train, testにデータを分割する
            train.mat <- data.full.b[1:n.train, ]
            test.mat <- data.full.b[(n.train+1):(n.train+n.test), ]
            idx.zerovar <- caret::nearZeroVar(x = train.mat)
            return(list(train = train.mat[, -idx.zerovar], test = test.mat[, -idx.zerovar], names = colnames(train.mat)))
          }
  )
}
