# Ładowanie niezbędnych pakietów
library(readxl)      
library(RSNNS)       
library(quantmod)    
library(RcmdrMisc)   
library(readr)       

# Wczytuję dane z pliku CSV
data_hour <- read_csv('http://jolej.linuxpl.info/hour.csv')

# Przypisuję kolumnę 'temp' z danych do zmiennej 'temperature'
temperature <- data_hour$temp

# Tworzę wykres pokazujący zmianę temperatury
plot(ts(temperature),
     main = "Zmiana temperatury w czasie",
     xlab = "Indeks", 
     ylab = "Temperatura", 
     type = "l", 
     col = "green")

# Dzielę dane na zbiór treningowy (pierwsze 1300 obserwacji)
training_indices <- 1:1300

# Tworzę obiekt 'zoo' z danymi temperatury
temperature_zoo <- as.zoo(temperature)

# Tworzę opóźnione wartości temperatury dla modelu
lag_4 <- Lag(temperature_zoo, k = 4)
lag_5 <- Lag(temperature_zoo, k = 5)
lag_6 <- Lag(temperature_zoo, k = 6)

# Łączę opóźnione wartości temperatury w jedną macierz
temperature_matrix <- cbind(temperature_zoo, lag_4, lag_5, lag_6)

# Usuwam pierwsze 6 wierszy (ze względu na brak danych dla opóźnionych wartości)
temperature_matrix <- temperature_matrix[-(1:6),]

# Przygotowuję dane wejściowe (input_data) i wyjściowe (output_data) dla modelu
input_data <- temperature_matrix[, 2:4]
output_data <- temperature_matrix[, 1]

# Trenuję sieć neuronową Elman z danymi treningowymi
neural_net <- elman(input_data[training_indices],          
                    output_data[training_indices],         
                    size = c(10, 3),        # Definiuję liczbę neuronów w warstwach ukrytych (10 neuronów w pierwszej i 3 w drugiej)
                    learnFuncParams = c(0.1), 
                    maxit = 4500)           # Ustawiam maksymalną liczbę iteracji (epok) w trakcie trenowania

# Rysuję wykres błędu iteracyjnego (w trakcie trenowania)
plotIterativeError(neural_net)

# Ustawiam podział wykresów na 2 wiersze
par(mfrow = c(2, 1))

# Rysuję wykres rzeczywistych wartości wyjściowych
actual_output <- as.vector(output_data[-training_indices])
plot(actual_output, type = "l", col = "black")

# Dokonuję predykcji na podstawie modelu sieci neuronowej
predicted_output <- predict(neural_net, input_data[-training_indices])

# Rysuję wykres przewidywanych wartości
plot(predicted_output, col = "orange", type = "l")

# Tworzę ramkę danych z rzeczywistymi i przewidywanymi wartościami
results <- data.frame(actual_output, predicted_output)

# Wnioski:
# Im większe okienko temperaturowe, czyli większy przedział czasu chcę przewidywać,
# tym amplituda temperatury się zmniejsza. Wykres się spłaszcza i nie pokazuje "trendów".
