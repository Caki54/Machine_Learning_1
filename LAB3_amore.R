library(AMORE)
library(data.table)
library(caret)

# Wczytanie danych
data_path <- "C:/Users/lukca/Desktop/bank.csv"
data_sep <- ";"
data <- fread(data_path, sep=data_sep)

# One-hot encoding
dummy_var <- dummyVars(~ ., data = data)
encoded_data <- predict(dummy_var, newdata = data)

# Konwersja na ramkę danych
encoded_df <- as.data.frame(encoded_data)

# Podział na zbiory treningowy i testowy
set.seed(123)
train_index <- createDataPartition(y = data$y, p = 0.8, list = FALSE)
train_data <- encoded_df[train_index, ]
test_data <- encoded_df[-train_index, ]

# Ekstrakcja kolumny wynikowej
target_var <- data$y
target_train <- target_var[train_index]
target_test <- target_var[-train_index]

# Konwersja wyników na wartości numeryczne
train_labels <- as.numeric(target_train == "yes")
test_labels <- as.numeric(target_test == "yes")

# Konwersja danych na macierze
train_matrix <- as.matrix(train_data)
test_matrix <- as.matrix(test_data)
train_labels_matrix <- matrix(train_labels, ncol = 1)
test_labels_matrix <- matrix(test_labels, ncol = 1)

# Definicja sieci neuronowej
neural_net <- newff(n.neurons=c(ncol(train_matrix), 40, 40, 1), learning.rate.global=1e-2, 
                    momentum.global=0.5, error.criterium="LMS", hidden.layer="tansig", 
                    output.layer="purelin", method="ADAPTgdwm")

# Trenowanie sieci
train_result <- train(neural_net, train_matrix, train_labels_matrix, test_matrix, test_labels_matrix, 
                      error.criterium="LMS", report=TRUE, show.step=100, n.shows=20)

# Predykcja na danych testowych
test_preds <- sim(train_result$net, test_matrix)

# Zaokrąglenie wyników
rounded_preds <- round(test_preds, 0)

# Tworzenie ramki wynikowej
results_df <- data.frame(Actual = test_labels_matrix, Predicted = rounded_preds)

# Konwersja na typ factor
results_df$Actual <- factor(results_df$Actual, levels = c(0, 1))
results_df$Predicted <- factor(results_df$Predicted, levels = c(0, 1))

# Macierz pomyłek
conf_mat <- confusionMatrix(results_df$Predicted, results_df$Actual)
print(conf_mat)

# Wykres funkcji błędu
plot_colors <- c("white", "black")
plot_symbols <- 21:23
matplot(train_result$Merror, pch=plot_symbols, bg=plot_colors, type="o", col="black", xlim=c(1, 30), ylim=c(0, 0.2))

# Wykres wyników predykcji
plot(results_df$Actual, results_df$Predicted, main="Actual vs Predicted", xlab="Actual", ylab="Predicted", col="blue", pch=19)
abline(0, 1, col="red")
