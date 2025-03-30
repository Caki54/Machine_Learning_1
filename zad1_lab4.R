library(readxl)
library(RSNNS)
library(quantmod)
library(corrplot)
library(AMORE)

# Wczytanie danych
setwd("C:/Users/lukca/Desktop/8/MUM_2/LAB4")
waluty <- read_excel("2007.xls", sheet="Sheet1")

# Usuwanie pierwszego wiersza i niepotrzebnych kolumn
waluty <- waluty[-1,]
waluty <- waluty[1:24]
waluty <- within(waluty, rm('1 BGN', '1 RON'))
waluty <- lapply(waluty, function(x) if(is.character(x)) as.numeric(x) else x)
waluty_df <- as.data.frame(waluty)
waluty_df <- waluty_df[2:22]

# Inicjalizacja macierzy korelacji
macierz_korelacji <- cor(waluty_df)
corrplot(macierz_korelacji, method = "circle")

# Usunięcie najmniej skorelowanych kursów walut
waluty <- within(waluty, rm('1 CZK', '1 CAD', '1 NOK', '1 AUD', '1 SKK'))
waluty_df <- as.data.frame(waluty)
waluty_df <- waluty_df[2:17]

# Ponowna inicjalizacja macierzy korelacji
macierz_korelacji <- cor(waluty_df)
corrplot(macierz_korelacji, method = "circle")

# Wybór trzech najlepiej skorelowanych walut
waluty_df <- waluty_df[,c("X1.USD","X1.ZAR","X100.HUF","X100.JPY")]
macierz_korelacji <- cor(waluty_df)
corrplot(macierz_korelacji, method = "circle")

# Przesunięcie kolumn o 2 miejsca w dół
waluty_df$X1.ZAR <- c(rep(NA,2), waluty_df$X1.ZAR)[1:nrow(waluty_df)]
waluty_df$X100.HUF <- c(rep(NA,2), waluty_df$X100.HUF)[1:nrow(waluty_df)]
waluty_df$X100.JPY <- c(rep(NA,2), waluty_df$X100.JPY)[1:nrow(waluty_df)]
waluty_df <- waluty_df[-c(1, 2), ]

# Przygotowanie danych treningowych i walidacyjnych
zbior_walidacyjny <- tail(waluty_df, 51)
zbior_treningowy <- head(waluty_df, 200)
dane_treningowe <- as.matrix(zbior_treningowy[,2:4])
wyniki_treningowe <- as.vector(as.numeric(zbior_treningowy[,1]))
dane_walidacyjne <- zbior_walidacyjny[,2:4]
wyniki_walidacyjne <- as.vector(as.numeric(zbior_walidacyjny[,1]))

# Budowa i uczenie sieci neuronowej
sieć <- newff(n.neurons = c(3, 10, 10, 1), learning.rate.global = 0.01, momentum.global = 0.5, error.criterium = "TAO", Stao = NA, hidden.layer = "tansig", output.layer = "purelin", method = "ADAPTgdwm")
rezultat_uczenia <- train(sieć, dane_treningowe, wyniki_treningowe, dane_walidacyjne, wyniki_walidacyjne, error.criterium = "LMS", report = TRUE, show.step = 100, n.shows = 20)

# Symulacja wyników
przewidywania <- sim(rezultat_uczenia$net, dane_walidacyjne)
matplot(rezultat_uczenia$Merror, pch = 21:23, bg = c("white", "black"), type = "o", col = "black", xlim = c(1, 30), ylim = c(0, 0.2))

# Wyświetlanie rzeczywistych wartości i predykcji
par(mfrow = c(2, 1))
rzeczywiste_wartosci <- as.vector(wyniki_walidacyjne)
plot(rzeczywiste_wartosci, type = "l", main = "Rzeczywiste wartości")
plot(przewidywania, col = "red", type = "l", main = "Predykcje")

wyniki <- data.frame(rzeczywiste_wartosci, przewidywania)

# Wybór jednej zmiennej objaśniającej
waluty_df <- as.data.frame(waluty)
waluty_df <- waluty_df[,c("X1.USD","X1.HKD")]
macierz_korelacji <- cor(waluty_df)
corrplot(macierz_korelacji, method = "circle")

waluty_df$X1.HKD <- c(rep(NA,2), waluty_df$X1.HKD)[1:nrow(waluty_df)]
waluty_df <- waluty_df[-c(1, 2), ]

# Przygotowanie danych treningowych i walidacyjnych dla jednej zmiennej
zbior_walidacyjny <- tail(waluty_df, 51)
zbior_treningowy <- head(waluty_df, 200)
dane_treningowe <- as.matrix(zbior_treningowy[,2])
wyniki_treningowe <- as.vector(as.numeric(zbior_treningowy[,1]))
dane_walidacyjne <- zbior_walidacyjny[,2]
wyniki_walidacyjne <- as.vector(as.numeric(zbior_walidacyjny[,1]))

# Budowa i uczenie sieci neuronowej z jedną zmienną objaśniającą
sieć <- newff(n.neurons = c(1, 10, 10, 1), learning.rate.global = 0.01, momentum.global = 0.5, error.criterium = "TAO", Stao = NA, hidden.layer = "tansig", output.layer = "purelin", method = "ADAPTgdwm")
rezultat_uczenia <- train(sieć, dane_treningowe, wyniki_treningowe, dane_walidacyjne, wyniki_walidacyjne, error.criterium = "LMS", report = TRUE, show.step = 100, n.shows = 20)

# Symulacja wyników
przewidywania <- sim(rezultat_uczenia$net, dane_walidacyjne)
matplot(rezultat_uczenia$Merror, pch = 21:23, bg = c("white", "black"), type = "o", col = "black", xlim = c(1, 30), ylim = c(0, 0.2))

par(mfrow = c(2, 1))
rzeczywiste_wartosci <- as.vector(wyniki_walidacyjne)
plot(rzeczywiste_wartosci, type = "l", main = "Rzeczywiste wartości")
plot(przewidywania, col = "red", type = "l", main = "Predykcje")

wyniki <- data.frame(rzeczywiste_wartosci, przewidywania)
