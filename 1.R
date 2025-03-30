---
  title: "LAB6"
author: "Łukasz Cakała"
date: "2024-06-15"
output: html_document
---
  
  ## Instalacja i ładowanie pakietów
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Instalacja pakietów
install.packages("magick")
install.packages("tibble")
install.packages("tensorflow")
install.packages("keras")
install.packages("reticulate")

# Ładowanie pakietów
library(magick)
library(tibble)
library(tensorflow)
library(keras)
library(reticulate)

# Utworzenie nowego wirtualnego środowiska
virtualenv_create("r-tensorflow")

# Aktywacja nowego środowiska
use_virtualenv("r-tensorflow", required = TRUE)

# Instalacja najnowszych wersji TensorFlow i Keras
py_install(c("tensorflow", "keras"), envname = "r-tensorflow")

# Ścieżka do głównego folderu ze zdjęciami
main_folder_path <- "C:/Users/lukca/Desktop/8/MUM_2/LAB6/PetImages"

safe_process_image <- function(image_path) {
  tryCatch({
    image <- image_read(image_path)
    image <- image %>% 
      image_resize("100x100!")%>%
      image_convert(colorspace = "gray")
    as.numeric(image_data(image))
  }, error = function(e) {
    message(paste("Błąd podczas przetwarzania obrazu:", image_path))
    message(e)
    return(NULL) # Zwraca NULL, jeśli obraz jest uszkodzony
  })
}

# Lista etykiet (podfolderów)
labels <- list.dirs(main_folder_path, full.names = TRUE, recursive = FALSE)

# Tworzenie pustej listy do przechowywania danych
image_data_list <- list()
image_labels <- c()
file_names <- c()

# Iteracja po wszystkich etykietach (podfolderach)
for (label in labels) {
  image_files <- list.files(label, pattern = "\\.(jpg|jpeg|png|bmp|gif)$", full.names = TRUE)
  for (image_file in image_files) {
    image_data <- safe_process_image(image_file)
    if (!is.null(image_data)) {  # Jeśli obraz został poprawnie wczytany
      image_data_list <- c(image_data_list, list(image_data))
      image_labels <- c(image_labels, basename(label)) # Nazwa folderu jako etykieta
      file_names <- c(file_names, basename(image_file)) # Nazwa pliku
    }
  }
}

# Tworzenie tibble z danymi obrazów i etykietami
image_dataset <- tibble(
  file_name = file_names,
  label = image_labels,
  pixel_data = image_data_list
)

# Wyświetlenie pierwszego obrazu w datasetcie
first_image_path <- file.path(main_folder_path, image_dataset$label[1], image_dataset$file_name[1])
image_read(first_image_path) %>% plot()

# Konwersja listy wektorów pikseli do macierzy
image_data_matrix <- do.call(rbind, image_dataset$pixel_data)

# Konwersja etykiet do formatu binarnego (0 dla kota, 1 dla psa)
image_labels_binary <- as.integer(factor(image_dataset$label, levels = c("Cat", "Dog")))

# Reshape macierzy danych do formatu 4D (liczba obrazów, wysokość, szerokość, liczba kanałów)
image_data_reshaped <- array(image_data_matrix, dim = c(nrow(image_data_matrix), 100, 100, 1))

set.seed(10) # dla powtarzalności wyników
indices <- sample(1:nrow(image_data_reshaped), size = 0.8 * nrow(image_data_reshaped))

x_train <- image_data_reshaped[indices, , , ]
y_train <- image_labels_binary[indices]
y_train <- y_train - 1

x_test <- image_data_reshaped[-indices, , , ]
y_test <- image_labels_binary[-indices]
y_test <- y_test - 1

model <- keras_model_sequential()

# Dodawanie warstw do modelu
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = 'relu', input_shape = c(100, 100, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Kompilacja modelu
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2
)

model %>% evaluate(x_test, y_test)
