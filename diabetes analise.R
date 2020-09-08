#diabetes - 75% de probabiblidade 

#carregando os dados

diabetes <- read.csv(
  file = "C:/Users/Andreia/Desktop/DATASETS/diabetes.csv"
)
head(diabetes)
View(diabetes)

#preparando dados
?str
str(diabetes)

#campos nao preenchidos 
?is.na
colSums(is.na(diabetes))

summary(diabetes$Insulin)
boxplot(diabetes$Insulin)

diabetes$Outcome <- as.factor(diabetes$Outcome)

boxplot(diabetes)

#analise exploratoria
hist(diabetes$Pregnancies)
hist(diabetes$Age)
hist(diabetes$BMI)

library(dplyr)
install.packages("dplyr")
diabetes2<- diabetes %>% filter(Insulin <= 250)
diabetes2

boxplot(diabetes2$insulin)

install.packages("caTools")
install.packages("tidyr")
library(caTools)
library(tidyr)

set.seed(123)

index = sample.split(diabetes2$Pregnancies, SplitRatio =  .7)
train = subset(diabetes2, index == TRUE)
test = subset(diabetes2, index ==FALSE)

#ver se adivisao esta correta
dim(diabetes2)
dim(train)
dim(test)

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

?caret::train

modelo <- train(
  Outcome ~., data = train, method = "knn")

modelo
#cria o modelo
modelo$results
modelo$bestTune

#testando novos valores para k 

modelo2 <- train(
  Outcome ~., data = train, method = "knn",
  tuneGrid = expand.grid(k = c(1:20)))

modelo2$results
modelo2$bestTune

plot(modelo2)

#algoritimo diferente naive bayes

train

modelo3 <- train(
  Outcome ~., data = train, method = "naive_bayes")

  modelo3$results
  modelo3$bestTune

  
  set.seed(100)
  
  modelo4 <- train(
    Outcome ~., data = train, method = "svmRadialgma",
    preProcess=c("center"))
  
    modelo4$results
    modelo4$bestTune
    
    #Avaliando o modelo 
    predicoes <- predict(modelo2,test)
    
    predicoes
    
    #comparar os  dados do teste e do treinamento 
    ?caret::confusionMatrix
    confusionMatrix(predicoes, test$Outcome)
  


