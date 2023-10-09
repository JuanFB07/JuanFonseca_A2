library(tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)

folder<-dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <-dirname(folder)
data <-
  read.csv(paste0(parentFolder,"/data/diabetes_012.csv"))

data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)

set.seed(10)
data1 <- data[sample(nrow(data), 3000), ]

table(data1$Sex)
table(data1$Smoker)
table(data1$CholCheck)


pairs.panels(data1 [c("Age", "BMI", "Education", "GenHlth")],
             pch = 21,
             bg = c("red", "green3", "blue", "orange" , "yellow")[unclass(data1$Diabetes_012)])

### Modelo KNN para detectar diabetes #####################################################################


## Seleccionar 1500 muestras del dataseet

set.seed(10)
data_est <- data %>%
  group_by(Diabetes_012) %>%
  sample_n(1500, replace = TRUE) %>%
  ungroup()


sample.index <- sample(1:nrow(data_est)
                       ,nrow(data_est)*0.7
                       ,replace = F)


predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Original data
train.data <- data_est[sample.index, c(predictors, "Diabetes_012"), drop = FALSE]
test.data <- data_est[-sample.index, c(predictors, "Diabetes_012"), drop = FALSE]


train.data$Diabetes_012 <- factor(train.data$Diabetes_012)
test.data$Diabetes_012 <- factor(test.data$Diabetes_012)

train.data$Diabetes_012 <- factor(train.data$Diabetes_012)
test.data$Diabetes_012 <- factor(test.data$Diabetes_012)

# Train the k-NN model
ctrl <- trainControl(method = "cv", p = 0.7)
knnFit <- train(Diabetes_012 ~ .
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 50)

plot(knnFit)

# Ejecutar Predicciones
knnPrediccion <- predict(knnFit, newdata = test.data)

# Crear la matriz confusion
confusionMatrix(data = knnPrediccion, reference = test.data$Diabetes_012)




