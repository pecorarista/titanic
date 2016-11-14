# 生データを読みだして、特徴量を加工する

createRawfeauture <- function(train, test){
  # データ加工----
  # データ読み込み
  data.train.raw <- fread(train, data.table = F, stringsAsFactors = T)
  data.test.raw <- fread(test, data.table = F, stringsAsFactors = T)
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
  data.full$Child[data.full$Age > 60] <- 'Old'
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
  data.full <- data.full %>% select(-Name, -Ticket, -Cabin) %>% as.data.frame()
  return(data.full)
}