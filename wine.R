
#  Читаем данные
wine <-read.table("Wine.txt",header=T, sep = "")

#  Проверяем имена переменных
names(wine)

#  Для стандартизации переменных для каждого столбца 
#  находим минимальное значение и размах
a <- sapply(wine[ , -ncol(wine)], min)
b <- sapply(wine[ , -ncol(wine)], max) - a

#  Собственно стандартизация входных переменных
wine.x <- scale(wine[, 1:13], center=a, scale=b)

#  Преобразование выходной переменной в три столбца, в три индикаторные переменные.
#  Поясним преобразование на примере
#  Вектор (3, 1, 2) преобразуется в матрицу
#  0	0	1
#  1	0	0
#  0	1	0


y1 <- rep(0, nrow(wine))
y1[wine[ , 14]==0] <-1

y2 <- rep(0, nrow(wine))
y2[wine[ , 14]==1] <-1

y3 <- rep(0, nrow(wine))
y3[wine[ , 14]==2] <-1

#   Другой способ получить тот-же результат
#    Процедура  class.ind из пакета nnet
#    wine.class <- class.ind(factor(wine[, 14]))


#  Подключаем библиотеку neuralnet.
library(neuralnet)

z.1 <- as.data.frame(cbind(wine.x, y1))

n <- names(z.1)

#  paste(n[!n %in% "y1"], collapse = " + ")
#  paste("y1 ~", paste(n[!n %in% "y1"], collapse = " + "))

f <- as.formula(paste("y1 ~", paste(n[!n %in% "y1"], collapse = " + ")))


#  Перебираю зерна датчика случайных чисел, 
#  чтобы строить разные нейронные сети, но иметь возможность воспроизвести их
i.seed <- 0

set.seed(12345+i.seed)


a.min <- 100500
for (i in 1:100)
{
  nn <- neuralnet(y1 ~ Input1 + Input2 + Input3 + Input4 + Input5 + Input6 + 
                    Input7 + Input8 + Input9 + Input10 + Input11 + Input12 + Input13,
                  data=z.1 ,hidden = 8, linear.output=F)
  res.z <- compute(nn, z.1[, 1:13])
  a <- sum(res.z$net.result != y1)
  if (a<a.min)
  {
    nn.min <- nn
    a.min <- a
  }
  
}


#  Обучаем нейронную сеть
#  Рекомендуют  decay=0.001
wine.net <- nnet(wine.x, wine.class, size = 6, rang=0.1, decay=0.1)

На выходе имеем матрицу
class(wine.net$fitted.values)


Переделываю матрицу индикаторных переменных в столбец с указанием сорта вина.
Лучше использовать процедуру which.is.max из пакета nnet.

Готовлю вектор с результатами
a.2 <- rep(0, nrow(wine.net$fitted.values))

for(i in 1:nrow(wine.net$fitted.values))
{
  a.2[i]<- which.max(wine.net$fitted.values[i, ])
}

Таблица сопряженности
table(a.2, wine[, 14])

a.2  0  1  2
1 59  0  0
2  0 48  1
3  0  0 70

Получаю, что на обучающем множестве нейронная сеть делает 1 ошибку






Задача 2
Нейронные сети для решения задач регрессии