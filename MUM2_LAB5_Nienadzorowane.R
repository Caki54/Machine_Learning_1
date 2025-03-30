# Pobieranie danych z podanego URL
Dane <- read.csv("http://jolej.linuxpl.info/CC_GENERAL.csv", header=TRUE)
dane <- as.data.frame(Dane)

# Wyświetlanie pierwszych kilku wierszy i podsumowanie danych
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
wybierz_zmienne <- function(macierz_korelacji, prog_wyjasniajaca = 0.5, prog_wyjasniane = 0.5) {
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
wybraneDane <- daneKorelacje[, c(7, 8, 9)]

# Funkcja normalizująca dane metodą min/max
normalizuj <- function(df) {
  df_normalizowany <- df
  for (kol in names(df)) {
    min_val <- min(df[[kol]], na.rm = TRUE)
    max_val <- max(df[[kol]], na.rm = TRUE)
    if (max_val != min_val) {
      df_normalizowany[[kol]] <- (df[[kol]] - min_val) / (max_val - min_val)
    } else {
      df_normalizowany[[kol]] <- 0
    }
  }
  return(df_normalizowany)
}
summary(wybraneDane)
wybraneDane <- normalizuj(wybraneDane)

# Wczytanie biblioteki Kohonen i budowa modelu SOM
library("kohonen")
wybraneDane.sc <- as.matrix(wybraneDane)
siatka_som <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
som_model <- som(wybraneDane.sc, grid = siatka_som, rlen = 50, alpha = c(0.05, 0.01))

# Podsumowanie modelu
summary(som_model)

# Rysowanie wykresów modelu SOM
plot(som_model, type = "changes")
plot(som_model, type = "count")
plot(som_model, type = "mapping")
plot(som_model, type = "dist.neighbours")
plot(som_model, type = "codes")

# Klasteryzacja hierarchiczna
liczba_grup <- 5
som_hc <- cutree(hclust(dist(som_model$codes[[1]])), liczba_grup)
plot(som_model, type = "codes", bgcol = rainbow(liczba_grup)[som_hc])

liczba_grup <- 4
som_hc <- cutree(hclust(dist(som_model$codes[[1]])), liczba_grup)
plot(som_model, type = "codes", bgcol = rainbow(liczba_grup)[som_hc])

liczba_grup <- 3
som_hc <- cutree(hclust(dist(som_model$codes[[1]])), liczba_grup)
plot(som_model, type = "codes", bgcol = rainbow(liczba_grup)[som_hc])

# Wnioski:
# Najlepsze wyniki daje podział na 3 lub 4 grupy. Większy podział nie wnosi znaczących zmian.
