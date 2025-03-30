# Pobranie CSV z podanego URL
Dane <- read.csv("http://jolej.linuxpl.info/CC_GENERAL.csv", header=TRUE)
dane <- as.data.frame(Dane)

# Wyświetlenie pierwszych kilku wierszy i podsumowanie danych
head(dane)
summary(dane)

# Usuwanie brakujących wartości
dane <- na.omit(dane)
daneKorelacje <- dane[2:18]
macierz_korelacji <- cor(daneKorelacje)
library(corrplot)

# Wyświetlanie macierzy korelacji
corrplot(macierz_korelacji, method = "number", tl.pos = "n")

# Funkcja wybierająca zmienną, która koreluje z największą liczbą zmiennych, które nie korelują ze sobą.
# Argumenty:
# - macierz zmiennych numerycznych
# - wartość korelacji między zmienną objaśnianą a objaśniającymi
# - wartość korelacji między zmiennymi objaśniającymi
choose_variables <- function(macierz_korelacji, prog_wyjasniajaca = 0.5, prog_wyjasniane = 0.5) {
  zmienne <- colnames(macierz_korelacji)
  liczba_korelacji <- numeric(length(zmienne))
  lista_korelacji <- vector("list", length(zmienne))
  for (i in seq_along(zmienne)) {
    wyjasniana <- zmienne[i]
    korelowane_zmienne <- c()
    for (j in seq_along(zmienne)) {
      if (i != j) {
        aktualna_zmienna <- zmienne[j]
        if (abs(macierz_korelacji[wyjasniana, aktualna_zmienna]) > prog_wyjasniajaca) {
          niekorelacyjne <- all(abs(macierz_korelacji[aktualna_zmienna, korelowane_zmienne]) <= prog_wyjasniane)
          if (niekorelacyjne) {
            korelowane_zmienne <- c(korelowane_zmienne, aktualna_zmienna)
          }
        }
      }
    }
    liczba_korelacji[i] <- length(korelowane_zmienne)
    lista_korelacji[[i]] <- korelowane_zmienne
  }
  max_korelacje <- max(liczba_korelacji)
  najlepsze_zmienne <- zmienne[liczba_korelacji == max_korelacje]
  najlepsze_korelacje <- lista_korelacji[liczba_korelacji == max_korelacje]
  return(list(
    wyjasniane_zmienne = najlepsze_zmienne,
    korelowane_zmienne = najlepsze_korelacje
  ))
}

# Wybór zmiennych do analizy
wybraneDane <- daneKorelacje[, c(7,8,9)]

# Funkcja normalizująca dane metodą min/max
normalize <- function(df) {
  df_normalized <- df
  for (col in names(df)) {
    min_val <- min(df[[col]], na.rm = TRUE)
    max_val <- max(df[[col]], na.rm = TRUE)
    if (max_val != min_val) {
      df_normalized[[col]] <- (df[[col]] - min_val) / (max_val - min_val)
    } else {
      df_normalized[[col]] <- 0
    }
  }
  
  return(df_normalized)
}
summary(wybraneDane)
wybraneDane <- normalize(wybraneDane)

library('kohonen')
set.seed(10)

idx_n <- sample(nrow(wybraneDane),as.integer(0.70*nrow(wybraneDane)))

train <- wybraneDane[idx_n,]
row.names(train) <- NULL

test <- wybraneDane[-idx_n,]
row.names(test) <- NULL

# Standaryzacja zmiennych
train.sc <- as.matrix(train)
som_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")  
som.data <- som(train.sc, grid = som_grid, rlen = 100, alpha = c(0.05, 0.01), keep.data = TRUE)

# Klasteryzacja hierarchiczna
set_cluster <- 4
som.data.hc <- cutree(hclust(dist(som.data$codes[[1]])), set_cluster)
train_cluster <- as.factor(as.vector(som.data.hc[som.data$unit.classif]))
train.l.sc <- list(x = train.sc, y = train_cluster)
mygrid <- somgrid(10, 10, "hexagonal")
som.data.l <- supersom(train.l.sc, grid = mygrid, maxNA.fraction = .5)

# Predykcja dla nowego zbioru
# Standaryzacja zbioru testowego
test.l.sc <- list(x = as.matrix(test))

# Predykcja dla zbioru testowego
test.pred <- predict(som.data.l, newdata = test.l.sc)
train_final <- cbind(train, cluster = train_cluster)
test_final <- cbind(test, cluster = test.pred$predictions$y)

by(train_final, train_final$cluster, summary)
by(test_final, test_final$cluster, summary)
