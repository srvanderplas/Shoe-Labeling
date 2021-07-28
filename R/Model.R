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

#Freeze somewhere?
#freeze_weights(conv_base)
##Vgg-16 Model
model <- keras_model_sequential() %>% conv_base %>% 
  #block 1
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding ="same", input_shape = c(150, 150, 3, 1)) 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu", padding ="same") %>%
  layer_max_pooling_2d(strides = c(2, 2), pool_size = c(2, 2, 2)) %>%
  #block 2
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding ="same", input_shape = c(150, 150, 3, 1)) 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu", padding ="same") %>%
  layer_max_pooling_2d(strides = c(2, 2), pool_size = c(2, 2, 2)) %>%
  #block 3
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same", input_shape = c(150, 150, 3, 1)) 
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same") %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same")
  layer_max_pooling_2d(strides = c(2, 2), pool_size = c(2, 2, 2)) %>%
  #block 4
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same", input_shape = c(150, 150, 3, 1)) 
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same") %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same")
  layer_max_pooling_2d(strides = c(2, 2), pool_size = c(2, 2, 2)) %>%
  #block 5
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same", input_shape = c(150, 150, 3, 1)) 
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same") %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding ="same")
  layer_max_pooling_2d(strides = c(2, 2), pool_size = c(2, 2, 2)) %>%
  #Model head
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(rate=.5) %>%
  layer_dense(units = length(classes), activation = "softmax", name = "class_head")
  #softmax works with the bounding boxes in the [0,1] coordinates

  ##Regional proposal network layer--do these need to be in functions?
  rpn_layer <- function(base_layers, num_anchors) {
  layer_conv_2d(filters = 512, kernel_size = c(3,3, activation = "relu", kernal_initializer ="normal"))
  classification_layer <- layer_conv_2d(filters = num_anchors, kernel_size = c(1,1) ) 
  }
  
  ##Region of interest pooling?
  ##Classifier layer
  ##Intersction over Union--this is fixed
  ##Calculate RPN for anchors
  
  # layer_conv_2d(filters = 128, kernel_size = c(3, 3, 1), activation = "relu") layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  # layer_conv_2d(filters = 128, kernel_size = c(3, 3, 1), activation = "relu") layer_max_pooling_2d(pool_size = c(2, 2, 2)) %>%
  # layer_flatten() %>%
  # layer_dropout(rate = 0.5) %>%
  # layer_dense(units = 512, activation = "relu") %>%
  # layer_dense(units = length(classes), activation = "softmax") %>%
  # freeze_weights(conv_base) %>%
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

  


