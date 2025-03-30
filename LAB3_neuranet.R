
library(caret)
library(nnet)
library(neuralnet)

bank <- read.csv("C:/Users/lukca/Desktop/bank.csv", sep=";")

# One-Hot Encoding
dummies_model <- dummyVars(~ job + marital + education + default + housing + loan + contact + month + poutcome + y, data=bank)
bank_transformed <- predict(dummies_model, newdata = bank)

# Konwersja wyniku do ramki danych
bank_transformed <- data.frame(bank_transformed)
colnames(bank_transformed)
# kolumnę 'yyes' = wartości numeryczne (0 lub 1)
bank_transformed$y <- as.numeric(bank_transformed$yyes == 1)

# Usuwam kolumnę 'yno'
bank_transformed$yno <- NULL
bank_transformed$yyes <- NULL

# Podział danych na zbiór treningowy i testowy
set.seed(123)  # Dla powtarzalności wyników
partition <- createDataPartition(y = bank_transformed$y, p = 0.8, list = FALSE)
train_set <- bank_transformed[partition, ]
test_set <- bank_transformed[-partition, ]

# Wyświetlenie pierwszych kilku wierszy zbioru treningowego i testowego
head(train_set)
head(test_set)

# Konwersja etykiet do postaci liczbowej
train_labels <- class.ind(train_set$y)
test_labels <- class.ind(test_set$y)

# Budowa modelu sieci neuronowej
nnet_model <- neuralnet(y ~ . , data = train_set, hidden = c(5, 3), linear.output = FALSE)


# Wydrukowanie wag
print(nnet_model)

# Wygenerowanie grafu sieci
plot(nnet_model)

# Podstawienie nowych wartości
predicted_output <- predict(nnet_model, test_set[, -ncol(test_set)])

# Zaokrąglenie wyliczonych wartości do całkowitych
predicted_output <- round(predicted_output, 0)

# Wydrukowanie przewidywanych wartości
print(predicted_output)

# Przekodowanie wartości wyjściowych z macierzy binarnej na wektor o wartościach 1 i 2
predicted_output <- as.factor(apply(predicted_output, 1, which.max))

# Konwersja etykiet i przewidywanych wartości na factor z tymi samymi poziomami
test_set$y <- factor(test_set$y)
predicted_output <- factor(predicted_output, levels = levels(test_set$y))

# Wygenerowanie macierzy pomyłek
confusion_matrix <- confusionMatrix(test_set$y, predicted_output)
print(confusion_matrix)

# Obliczenie accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

