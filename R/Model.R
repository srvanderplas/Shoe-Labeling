set.seed(777777)

library(keras)
library(tensorflow)
library(tidyverse)
library(abind)
#Begin with VGG16 Model
conv_base <- application_vgg16( 
  weights = "imagenet", 
  include_top = FALSE, 
  #This could be important, if input_shape is not stated, the network will be able to process inputs of any size
  input_shape = c(150, 150, 3)
)

use_backend("tensorflow")
# install_keras()
if (!exists("classes")) {
  classes <- c(
    "logo", "circle"
  )
}

#Freeze so we can create a new layer head
# freeze_weights(conv_base)
model <- keras_model_sequential() %>% conv_base %>% 
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = length(classes), activation = "softmax") %>%
  freeze_weights(conv_base) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3, 1), activation = "relu", input_shape = c(150, 150, 3, 1)) layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3, 1), activation = "relu") layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3, 1), activation = "relu") layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3, 1), activation = "relu") layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  
  # layer_dense(units = 128, activation = "relu") %>%
  # layer_dense(units = 64, activation = "relu") %>%
  # layer_dense(units = 32, activation = "relu") %>%
  # layer_dense(units = 4, activation = "sigmoid", name = "bbox_head") %>%
  # layer_dense(units = 512, activation = "relu") %>%
  # layer_dropout(rate=.5) %>%
  # layer_dense(units = 512, activation = "relu") %>%
  # layer_dropout(rate=.5) %>%
  # layer_dense(units = length(classes), activation = "softmax", name = "class_head")

model %>% compile(optimizer = "rmsprop",
                      loss ="clacategorical_crossentropy",
                      metrics = c("accuracy"))
#two loss functions?
#Loss weights?

#Split into training and test data
#Need base directory with all of said images
#Convert bounding box coordinates to [0,1]?
#load in image 224x224 for VGG?
#Flatten first
#freeze the body layers and remove the fully connected layer head
#Create a new fully connected layer consists of the two branches-one for the bounding box and one for the detection

  


