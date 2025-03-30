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

# Use the virtual environment
use_virtualenv("C:/Users/lukca/Documents/myenv", required = TRUE)

# Ensure Pillow is installed
py_install("Pillow")

# Set up the data generators
train_datagen <- image_data_generator(
  rescale = 1/255,
  validation_split = 0.2
)

# Custom generator to ensure data repetition
custom_flow_images_from_directory <- function(directory, generator, target_size, batch_size, class_mode, subset, repeat_times) {
  dataset <- flow_images_from_directory(
    directory = directory,
    generator = generator,
    target_size = target_size,
    batch_size = batch_size,
    class_mode = class_mode,
    subset = subset
  )
  
  repeated_dataset <- reticulate::iterate(dataset, function(x) x)
  for (i in 1:(repeat_times - 1)) {
    repeated_dataset <- c(repeated_dataset, reticulate::iterate(dataset, function(x) x))
  }
  return(repeated_dataset)
}


# Train and validation generators with custom repetition
train_generator <- custom_flow_images_from_directory(
  directory = 'C:/Users/lukca/Desktop/8/MUM_2/projekt/flowers',
  generator = train_datagen,
  target_size = c(128, 128),
  batch_size = 32,
  class_mode = 'categorical',
  subset = 'training',
  repeat_times = repeat_times
)

validation_generator <- custom_flow_images_from_directory(
  directory = 'C:/Users/lukca/Desktop/8/MUM_2/projekt/flowers',
  generator = train_datagen,
  target_size = c(128, 128),
  batch_size = 32,
  class_mode = 'categorical',
  subset = 'validation',
  repeat_times = repeat_times
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
steps_per_epoch <- as.integer(ceiling(train_generator$n / train_generator$batch_size))
validation_steps <- as.integer(ceiling(validation_generator$n / validation_generator$batch_size))

# Train the model
history <- model$fit(
  train_generator,
  epochs = as.integer(10),
  validation_data = validation_generator,
  steps_per_epoch = steps_per_epoch,
  validation_steps = validation_steps
)

# Save the training history to a CSV file
history_df <- as.data.frame(history$history)
write.csv(history_df, "training_history.csv", row.names = FALSE)