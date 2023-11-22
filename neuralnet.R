library(dplyr)
library(caret)
library(neuralnet)

# Загрузка данных
olive_data <- read.csv("olive.csv")

# Преобразование region и area в факторы
olive_data$region <- as.factor(olive_data$region)
olive_data$area <- as.factor(olive_data$area)

# Удаление столбца name
olive_data <- select(olive_data, -name)

# Нормализация числовых данных
num_cols <- sapply(olive_data, is.numeric)
olive_data[num_cols] <- scale(olive_data[num_cols])

# Разделение данных на обучающую и тестовую выборки
set.seed(42)
trainIndex <- createDataPartition(olive_data$region, p = .8, list = FALSE, times = 1)
trainData <- olive_data[trainIndex, ]
testData <- olive_data[-trainIndex, ]

# Создание списка предикторов и сохранение столбца region отдельно
predictors <- setdiff(names(trainData), "region")
trainDataRegion <- trainData$region
testDataRegion <- testData$region

# Кодирование факторов для использования в neuralnet
trainDataTransformed <- dummyVars(~ ., data = select(trainData, predictors))
trainDataNN <- data.frame(predict(trainDataTransformed, newdata = trainData))
trainDataNN$region <- trainDataRegion

testDataTransformed <- dummyVars(~ ., data = select(testData, predictors))
testDataNN <- data.frame(predict(testDataTransformed, newdata = testData))
testDataNN$region <- testDataRegion

# Создание формулы для модели
formula_region <- reformulate(termlabels = names(trainDataNN)[-which(names(trainDataNN) == "region")], response = "region")

# Обучение модели neuralnet
model_region <- neuralnet(formula_region, data = trainDataNN, hidden = c(5), linear.output = FALSE)

# Предсказание региона на тестовых данных
testDataNNPredict <- testDataNN
testDataNNPredict$region <- NULL  # Удаление region перед предсказанием
predictions_region <- compute(model_region, testDataNNPredict)
predicted_labels_region <- apply(predictions_region$net.result, 1, which.max)
predicted_regions <- levels(testData$region)[predicted_labels_region]

# Вычисление точности (accuracy) для региона
correct_predictions <- sum(predicted_regions == testDataRegion)
accuracy <- correct_predictions / length(testDataRegion)
cat("Accuracy for region:", accuracy, "\n")
