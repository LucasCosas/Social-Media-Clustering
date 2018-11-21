install.packages("assertive")
library(assertive)

#Importando o Dataset
rawdata <- read.csv("../data/snsdata.csv")

names(rawdata)
rawdata$abercrombie

#Escolhendo só as colunas necessárias para o estudo
df <- rawdata[,1:4]

#Explorando o dataset
names(df)
class(df)
sapply(df, function(x) class(x))
sum(df$friends)

#checando os NAs
sapply(df, function(x) sum(is.na(x)))

#transformando a idade de float para inteiro
df$age <- as.integer(df$age)

summary(df)

sum(sapply(df$age, function(x) sum(x>30)))

#plotando as idades
barplot(sort(table(df$age), decreasing = TRUE), las=2, main="Ages")
#vemos aqui que 106 pessoas tÃªm 106 anos, 24 tÃªm 105, 6 tÃªm 103...
sort(table(df$age), decreasing = TRUE)

plot(df$age)
boxplot(df$age, outline = FALSE, main="Ages Boxplot")

# NÃºmero de linhas com mais de 23 anos de idade
nrow(df[df$age > 23, ])

#removendo alunos com mais de 23 anos no high school
df <- df[df$age < 23, ]

# NÃºmero de linhas com menos de 14 anos de idade
nrow(df[df$age < 14, ])
#removendo alunos com mais de 14 anos no high school
df <- df[df$age > 14, ]


####################### Prevendo a idade dos NAs

rmse <- function(error)
{
  sqrt(mean(error^2))
}

set.seed(1234)

df2 <- df[complete.cases(df),]

ind <- sample(2, nrow(df2), replace = TRUE, prob = c(0.8, 0.2))
train <- df2[ind == 1, ]
test <- df2[ind == 2,]

train$gender <- NULL
test$gender <- NULL

train$friends <- NULL
test$friends <- NULL

sapply(df2, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

modelm <- lm(age ~ ., data=train)
summary(modelm)
plot(modelm)

predict <- predict(modelm, test)
test$pred <- predict(modelm, test)

test$error <- abs(test$age - test$pred)

rmse(test$age - test$pred)

View(test)
summary(predict)
# com uma assertividade de 50%, acho que n vale a pena prever. 

#######################

#deixando somente as linhas com infos completas
df <- df[complete.cases(df),]

summary(df)
sapply(df, function(x) class(x))

#normalizando os dados
df$gradyear <- df$gradyear/max(df$gradyear)
df$age <- df$age/max(df$age)
df$friends <- df$friends/max(df$friends)

# FunÃ§Ã£o elbow para buscar o Kmeans
elbow <- function(df) {
  wss <- numeric(15)
  for (i in 1:15) wss[i] <- sum(kmeans(df, centers = i, nstart = 100)$withinss)
  plot(1:15, wss, type = "b", main = "Elbow method", xlab = "Number of Clusters", 
       ylab = "Within groups sum of squares", pch = 8)
}

elbow(df)
sapply(df, function(x) sum(is.na(x)))

#transformando os dados de sexo em numérico
df$gender <- ifelse(df$gender=="M", 1, 0)

elbow(df)

model = kmeans(df, centers = 3)

df$cluster <- model$cluster

plot(df$gradyear, df$gender, pch=19, col=df$cluster)
plot(df$gradyear, df$age, pch=19, col=df$cluster)
plot(df$gradyear, df$friends, pch=19, col=df$cluster)
plot(df$gender, df$age, pch=19, col=df$cluster)
plot(df$gender, df$friends, pch=19, col=df$cluster)
plot(df$age, df$friends, pch=19, col=df$cluster)

# E aí? o que fazer?
