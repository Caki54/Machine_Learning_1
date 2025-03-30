library(RSNNS)
library(NeuralNetTools)
library(caret)

# Wczytanie danych
sciezka_dane <- "C:/Users/lukca/Desktop/bank.csv"
bank <- read.csv(sciezka_dane, sep=";")

# One-hot encoding
model_dummies <- dummyVars(~ job + marital + education + default + housing + loan + contact + month + poutcome + y, data=bank)
bank_encoded <- predict(model_dummies, newdata = bank)

# Konwersja do ramki danych
bank_encoded <- data.frame(bank_encoded)

# Konwersja kolumny 'y' na wartości numeryczne
bank_encoded$y <- as.numeric(bank$y == "yes")

# Usunięcie niepotrzebnych kolumn
bank_encoded$yno <- NULL
bank_encoded$yyes <- NULL

# Przygotowanie zmiennych wejściowych i wyjściowych
input_data <- bank_encoded[, -ncol(bank_encoded)]
target_data <- decodeClassLabels(bank_encoded$y)

# Podział danych na zbiory treningowy i testowy
set.seed(123)  # Ustawienie losowości
podzial <- splitForTrainingAndTest(input_data, target_data, ratio=0.3)

# Normalizacja zbiorów danych
podzial <- normTrainingAndTestSet(podzial)

# Budowa modelu sieci neuronowej
model_nn <- mlp(podzial$inputsTrain, podzial$targetsTrain, size=15, learnFuncParams = 0.1, maxit = 50, 
                inputsTest = podzial$inputsTest, targetsTest = podzial$targetsTest)

# Wykres sieci neuronowej
plotnet(model_nn)

# Predykcja na podstawie modelu
predykcje <- predict(model_nn, podzial$inputsTest)

# Wykres ROC
plotROC(predykcje[,1], podzial$targetsTest[,1])

# Wykres błędu iteracyjnego
plotIterativeError(model_nn)

# Wielkości wag sieci neuronowej
neuralweights(model_nn)

# Zaokrąglenie predykcji do wartości całkowitych
predykcje <- round(predykcje, 0)

# Rozkodowanie predykcji
predykcje <- factor(encodeClassLabels(predykcje))

# Rozkodowanie prawdziwych etykiet
prawdziwe_etykiety <- factor(encodeClassLabels(podzial$targetsTest))

# Macierz pomyłek
conf_matrix <- confusionMatrix(prawdziwe_etykiety, predykcje)

# Macierz pomyłek z pakietu caret
caret_conf_matrix <- caret::confusionMatrix(prawdziwe_etykiety, predykcje)

# Wyniki macierzy pomyłek
print(conf_matrix)
print(caret_conf_matrix)
