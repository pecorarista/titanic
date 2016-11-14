# プロットしてみる

# パッケージ
require(data.table)
require(ggplot2)
require(dplyr)
require(reshape2)

# 読み込み
data.train.raw <- fread("inputs/train.csv", data.table = F, stringsAsFactors = F)

# とりあえず数値型の変数をプロット
# pClass
data.plot <- data.train.raw %>% select(Survived, Pclass) %>% mutate(Survived = as.factor(Survived)) %>%
  group_by(Survived, Pclass) %>% summarise(count = n())
g <- ggplot(
  data.plot,
  aes (
    x = Pclass,
    y = count,
    fill= Survived
  )
)
g <- g + geom_bar(position = "dodge", stat = "identity")
ggsave(filename = "outputs/plot_pclass.png", plot = g)

data.plot <- data.train.raw %>% select(Survived, Sex) %>% mutate(Survived = as.factor(Survived)) %>%
  group_by(Survived, Sex) %>% summarise(count = n())
g <- ggplot(
  data.plot,
  aes (
    x = Sex,
    y = count,
    fill= Survived
  )
)
g <- g + geom_bar(position = "dodge", stat = "identity")
ggsave(filename = "outputs/plot_sex.png", plot = g)

data.plot <- data.train.raw %>% select(Survived, Age) %>% mutate(Survived = as.factor(Survived))
g <- ggplot(
  data.plot,
  aes (
    x = Age,
    fill= Survived
  )
)
g <- g + geom_histogram(alpha = 0.5,position = "identity")
ggsave(filename = "outputs/plot_age.png", plot = g)

data.plot <- data.train.raw %>% select(Survived, SibSp) %>% mutate(Survived = as.factor(Survived)) %>%
  group_by(Survived, SibSp) %>% summarise(count = n())
g <- ggplot(
  data.plot,
  aes (
    x = SibSp,
    y = count,
    fill= Survived
  )
)
g <- g + geom_bar(position = "dodge", stat = "identity")
ggsave(filename = "outputs/plot_sibsp.png", plot = g)

data.plot <- data.train.raw %>% select(Survived, Parch) %>% mutate(Survived = as.factor(Survived)) %>%
  group_by(Survived, Parch) %>% summarise(count = n())
g <- ggplot(
  data.plot,
  aes (
    x = Parch,
    y = count,
    fill= Survived
  )
)
g <- g + geom_bar(position = "dodge", stat = "identity")
ggsave(filename = "outputs/plot_parch.png", plot = g)

data.plot <- data.train.raw %>% select(Survived, Embarked) %>% mutate(Survived = as.factor(Survived)) %>%
  group_by(Survived, Embarked) %>% summarise(count = n())
g <- ggplot(
  data.plot,
  aes (
    x = Embarked,
    y = count,
    fill= Survived
  )
)
g <- g + geom_bar(position = "dodge", stat = "identity")
ggsave(filename = "outputs/plot_embarked.png", plot = g)
