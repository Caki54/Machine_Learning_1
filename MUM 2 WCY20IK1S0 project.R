# Install keras and tensorflow packages from CRAN
install.packages("keras")
install.packages("tensorflow")


# Load the necessary libraries
library(keras)
library(tensorflow)
library(imager)
library(ggplot2)
library(caret)
library(magick)
library(reticulate)
library(dplyr)

# Use the virtual environment
use_virtualenv("C:/Users/lukca/Documents/myenv", required = TRUE)

# Ensure Pillow is installed
py_install("Pillow")

# Set up the data generators
train_datagen <- image_data_generator(
  rescale = 1/255,
  validation_split = 0.2
)

train_generator <- flow_images_from_directory(
  directory = 'C:/Users/lukca/Desktop/8/MUM_2/projekt/flowers',
  generator = train_datagen,
  target_size = c(128, 128),
  batch_size = 32,
  class_mode = 'categorical',
  subset = 'training'
)

validation_generator <- flow_images_from_directory(
  directory = 'C:/Users/lukca/Desktop/8/MUM_2/projekt/flowers',
  generator = train_datagen,
  target_size = c(128, 128),
  batch_size = 32,
  class_mode = 'categorical',
  subset = 'validation'
)

# Display a random image from the dataset
random_image_path <- sample(list.files('C:/Users/lukca/Desktop/8/MUM_2/projekt/flowers', recursive = TRUE, full.names = TRUE), 1)
random_image <- load.image(random_image_path)
plot(random_image)

# Get the number of classes
num_classes <- length(unique(train_generator$class_indices))

# Define the model using the Functional API from TensorFlow, ensuring integer shape values
input <- tf$keras$layers$Input(shape = c(as.integer(128), as.integer(128), as.integer(3)))
conv1 <- tf$keras$layers$Conv2D(filters = as.integer(32), kernel_size = c(as.integer(3), as.integer(3)), activation = 'relu')(input)
pool1 <- tf$keras$layers$MaxPooling2D(pool_size = c(as.integer(2), as.integer(2)))(conv1)
conv2 <- tf$keras$layers$Conv2D(filters = as.integer(64), kernel_size = c(as.integer(3), as.integer(3)), activation = 'relu')(pool1)
pool2 <- tf$keras$layers$MaxPooling2D(pool_size = c(as.integer(2), as.integer(2)))(conv2)
flat <- tf$keras$layers$Flatten()(pool2)
dense1 <- tf$keras$layers$Dense(units = as.integer(128), activation = 'relu')(flat)
dropout <- tf$keras$layers$Dropout(rate = 0.5)(dense1)
output <- tf$keras$layers$Dense(units = as.integer(num_classes), activation = 'softmax')(dropout)

# Create the model
model <- tf$keras$Model(inputs = input, outputs = output)

# Compile the model
model$compile(
  optimizer = tf$keras$optimizers$Adam(),
  loss = 'categorical_crossentropy',
  metrics = list('accuracy')
)

# Calculate steps_per_epoch and validation_steps
steps_per_epoch <- as.integer(ceiling(as.numeric(train_generator$n) / as.numeric(train_generator$batch_size)))
validation_steps <- as.integer(ceiling(as.numeric(validation_generator$n) / as.numeric(validation_generator$batch_size)))

# Train the model manually in a loop for multiple epochs
total_epochs <- 10
history_list <- list()

for (epoch in 1:total_epochs) {
  cat(sprintf("Epoch %d/%d\n", epoch, total_epochs))
  history <- model$fit(
    train_generator,
    epochs = as.integer(1),
    validation_data = validation_generator,
    steps_per_epoch = steps_per_epoch,
    validation_steps = validation_steps
  )
  history_list[[epoch]] <- history$history
}


# Combine the history data
history_df <- do.call(rbind, lapply(history_list, as.data.frame))
write.csv(history_df, "training_history.csv", row.names = FALSE)


# Visualize the results
history_df <- do.call(rbind, lapply(1:total_epochs, function(epoch) {
  cbind(data.frame(epoch=epoch), as.data.frame(history_list[[epoch]]))
}))

# Plotting accuracy
accuracy_plot <- ggplot(history_df, aes(x=epoch)) +
  geom_line(aes(y=accuracy, color="Training Accuracy")) +
  geom_line(aes(y=val_accuracy, color="Validation Accuracy")) +
  labs(title="Model Accuracy", x="Epoch", y="Accuracy") +
  scale_color_manual("", 
                     breaks = c("Training Accuracy", "Validation Accuracy"),
                     values = c("Training Accuracy"="blue", "Validation Accuracy"="red"))

# Plotting loss
loss_plot <- ggplot(history_df, aes(x=epoch)) +
  geom_line(aes(y=loss, color="Training Loss")) +
  geom_line(aes(y=val_loss, color="Validation Loss")) +
  labs(title="Model Loss", x="Epoch", y="Loss") +
  scale_color_manual("", 
                     breaks = c("Training Loss", "Validation Loss"),
                     values = c("Training Loss"="blue", "Validation Loss"="red"))

# Display the plots
print(accuracy_plot)
print(loss_plot)

# Predykcje na zestawie walidacyjnym
validation_steps <- validation_generator$n %/% validation_generator$batch_size
predictions <- model %>% predict(validation_generator, steps = validation_steps)
predicted_classes <- apply(predictions, 1, which.max) - 1
true_classes <- validation_generator$classes
class_labels <- names(validation_generator$class_indices)

# Tworzenie macierzy konfuzji
confusion_mtx <- confusionMatrix(factor(predicted_classes, levels = 0:(num_classes-1)), 
                                 factor(true_classes[1:length(predicted_classes)], levels = 0:(num_classes-1)))

# Wizualizacja macierzy konfuzji
confusion_df <- as.data.frame(confusion_mtx$table)
confusion_df <- confusion_df %>%
  mutate(Predicted = factor(Prediction, levels = 0:(num_classes-1), labels = class_labels),
         Actual = factor(Reference, levels = 0:(num_classes-1), labels = class_labels))

confusion_plot <- ggplot(data = confusion_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Macierz Konfuzji", x = "Faktyczne", y = "Przewidywane") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(confusion_plot)

# Generowanie raportu klasyfikacyjnego
report <- classification_report(true_classes, predicted_classes, target_names=class_labels, output_dict=True)
report_df <- pd.DataFrame(report).transpose()
print(report_df)
