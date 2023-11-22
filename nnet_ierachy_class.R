library(dplyr)
library(caret)
library(nnet)


# Загрузка данных
olive_data <- read.csv("olive.csv")

# Удаление столбцов region и name
olive_data <- select(olive_data, -c(name))


# Преобразование region и area в факторы
olive_data$region <- as.factor(olive_data$region)
olive_data$area <- as.factor(olive_data$area)

# Нормализация числовых данных
num_cols <- sapply(olive_data, is.numeric)
olive_data[num_cols] <- scale(olive_data[num_cols])

# Разделение данных на обучающую и тестовую выборки
set.seed(42)
trainIndex <- createDataPartition(olive_data$region, p = .8, list = FALSE, times = 1)
trainData <- olive_data[trainIndex, ]
testData <- olive_data[-trainIndex, ]

# Обучение модели для предсказания региона
model_region <- multinom(region ~ ., data = trainData, maxit = 200)

# Предсказание региона на тестовых данных
predictions_region <- predict(model_region, newdata = testData)

# Обучение отдельной модели для каждого региона для предсказания области
models_area_per_region <- list()
for(region in unique(trainData$region)) {
  region_train_data <- subset(trainData, region == region)
  region_test_data <- subset(testData, region == region)
  
  if(nrow(region_train_data) > 0 && nrow(region_test_data) > 0) {
    model_area <- multinom(area ~ ., data = region_train_data, maxit = 200)
    models_area_per_region[[as.character(region)]] <- model_area
  }
}



# Функция для предсказания области с учетом региона
predict_area_based_on_region <- function(test_row, predicted_region) {
  # Выбор соответствующей модели для региона
  model_for_area <- models_area_per_region[[as.character(predicted_region)]]
  
  # Удаление столбца 'area' из test_row перед предсказанием
  test_row_without_area <- test_row[, !(names(test_row) %in% 'area')]
  
  # Предсказание области с использованием выбранной модели
  if (!is.null(model_for_area)) {
    return(predict(model_for_area, newdata = test_row_without_area))
  } else {
    return(NA)
  }
}

# Применение функции к каждой строке тестовых данных с учетом предсказанного региона
predicted_areas <- sapply(1:nrow(testData), function(i) {
  predict_area_based_on_region(testData[i, ], predictions_region[i])
})

# Добавление предсказаний области к тестовым данным
testData$predicted_area <- predicted_areas

# Проверка результатов
head(testData)


# Преобразование predicted_area в фактор и установка тех же уровней, что и у исходного area
testData$predicted_area <- factor(testData$predicted_area, levels = levels(testData$area))

# Вычисление и вывод матрицы ошибок и метрик качества
conf_matrix <- confusionMatrix(testData$predicted_area, testData$area)

print(conf_matrix)

# Вывод точности (accuracy)
accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy:", accuracy, "\n")




