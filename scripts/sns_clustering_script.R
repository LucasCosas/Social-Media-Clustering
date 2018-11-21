install.packages("assertive")
library(assertive)

#Importando o Dataset
rawdata <- read.csv("../data/snsdata.csv")

names(rawdata)
rawdata$abercrombie

df <- rawdata[,1:4]
names(df)
class(df)

#checando os NAs
sapply(df, function(x) sum(is.na(x)))
sum(df$friends)

#deixando somente as linhas com infos completas
df <- df[complete.cases(df),]

summary(df)

sum(sapply(df$age, function(x) sum(x>30)))
sapply(df, function(x) class(x))

df$age <- as.integer(df$age)

barplot(sort(table(df$age), decreasing = TRUE), las=2, main="Ages")

#vemos aqui que 106 pessoas têm 106 anos, 24 têm 105, 6 têm 103...
sort(table(df$age), decreasing = TRUE)

plot(df$age)
boxplot(df$age, outline = FALSE, main="Ages Boxplot")

# Número de linhas com mais de 23 anos de idade
nrow(df[df$age > 23, ])

#removendo alunos com mais de 23 anos no high school
df <- df[df$age < 23, ]

# Número de linhas com menos de 14 anos de idade
nrow(df[df$age < 14, ])
#removendo alunos com mais de 14 anos no high school
df <- df[df$age > 14, ]

summary(df)

df$gender <- NULL
df$gradyear <-NULL

#normalizando os dados
df$age <- df$age/max(df$age)
df$friends <- df$friends/max(df$friends)


model = kmeans(df, centers = 4)

# Função elbow para buscar o Kmeans
elbow <- function(df) {
  wss <- numeric(15)
  for (i in 1:15) wss[i] <- sum(kmeans(df, centers = i, nstart = 100)$withinss)
  plot(1:15, wss, type = "b", main = "Elbow method", xlab = "Number of Clusters", 
       ylab = "Within groups sum of squares", pch = 8)
}

elbow(df)

df$cluster <- model$cluster

plot(df$friends, df$age, pch=19, col=df$cluster)
