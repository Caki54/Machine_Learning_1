library(nnet)
library(caret)
library(NeuralNetTools)

# Wczytanie danych z pliku
data_path <- "C:/Users/lukca/Desktop/bank.csv"
data <- read.csv(data_path, sep=";")

# One-hot encoding za pomocą dummyVars
dummies <- dummyVars(~ job + marital + education + default + housing + loan + contact + month + poutcome + y, data=data)
encoded_data <- predict(dummies, newdata = data)

# Konwersja na ramkę danych
encoded_df <- data.frame(encoded_data)

# Przekształcenie kolumny 'y' na wartości numeryczne (0 lub 1)
encoded_df$y <- as.numeric(data$y == "yes")

# Usunięcie niepotrzebnych kolumn
encoded_df$yno <- NULL
encoded_df$yyes <- NULL

# Konwersja kolumny 'y' na typ faktor
encoded_df$y <- factor(encoded_df$y)

# Podział danych na zbiór treningowy i walidacyjny
set.seed(123)  # Zapewnienie powtarzalności wyników
split <- createDataPartition(y = encoded_df$y, p = 0.8, list = FALSE)
train_data <- encoded_df[split, ]
validation_data <- encoded_df[-split, ]

# Budowa i trenowanie sieci neuronowej
nn_model <- nnet(y ~ ., data = train_data, size = 10, decay = 5e-4, maxit = 200)

# Wykres sieci neuronowej za pomocą funkcji plotnet
plotnet(nn_model)

# Wyświetlenie wag sieci neuronowej za pomocą funkcji neuralweights
neuralweights(nn_model)

# Predykcja na zbiorze treningowym
train_predictions <- predict(nn_model, newdata = train_data, type = 'class')
train_predictions <- factor(as.integer(train_predictions))

# Macierz pomyłek dla zbioru treningowego
train_conf_matrix <- confusionMatrix(train_predictions, train_data$y)
print(train_conf_matrix)

# Predykcja na zbiorze walidacyjnym
validation_predictions <- predict(nn_model, newdata = validation_data, type = 'class')
validation_predictions <- factor(as.integer(validation_predictions))

# Macierz pomyłek dla zbioru walidacyjnego
validation_conf_matrix <- confusionMatrix(validation_predictions, validation_data$y)
print(validation_conf_matrix)
