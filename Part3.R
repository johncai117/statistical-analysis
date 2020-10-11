### NEural Networks

rm(list = ls())
library(e1071) # For svm
library(Rsafd)
library(keras)
library(dplyr)



load(paste("/Users/johncai/Documents/FIN\ 505/FINAL_SUBMISSION/Final_project/", "Jan2020FinalData.r", sep =""))

## Question 1

##SCALING 

xx3_train <- x_train ##USe all 4 variables
xx3_test <- x_test 

input_1_scaled <- scale(x_train[,1,])
input_2_scaled <- scale(x_train[,2,])
input_3_scaled <- scale(x_train[,3,])
input_4_scaled <- scale(x_train[,4,])

input_scales_1 <- attributes(input_1_scaled)
input_scales_2 <- attributes(input_2_scaled)
input_scales_3 <- attributes(input_3_scaled)
input_scales_4 <- attributes(input_4_scaled)

input.mean_1 <- input_scales_1$`scaled:center`
input.mean_2  <- input_scales_2$`scaled:center`
input.mean_3 <- input_scales_3$`scaled:center`
input.mean_4  <- input_scales_4$`scaled:center`

input.sd_1 <- input_scales_1$`scaled:scale`
input.sd_2  <- input_scales_2$`scaled:scale`
input.sd_3 <- input_scales_3$`scaled:scale`
input.sd_4  <- input_scales_4$`scaled:scale`

tst.scaled_1 <- scale(x_test[,1,], center = input.mean_1, scale = input.sd_1)
tst.scaled_2 <- scale(x_test[,2,], center = input.mean_2, scale = input.sd_2)
tst.scaled_3 <- scale(x_test[,3,], center = input.mean_3, scale = input.sd_3)
tst.scaled_4 <- scale(x_test[,4,], center = input.mean_4, scale = input.sd_4)

xx3_train[,1,] <- input_1_scaled
xx3_train[,2,] <- input_2_scaled
xx3_train[,3,] <- input_3_scaled
xx3_train[,4,] <- input_3_scaled

xx3_test[,1,] <- tst.scaled_1
xx3_test[,2,] <- tst.scaled_2
xx3_test[,3,] <- tst.scaled_3
xx3_test[,4,] <- tst.scaled_4

xx3_train <- cbind(input_1_scaled, input_3_scaled, input_4_scaled)
xx3_test <- cbind(tst.scaled_1, tst.scaled_3, tst.scaled_4)

dim(xx3_train)


xx1_train = input_1_scaled
xx1_test = tst.scaled_1

xx2_train = input_2_scaled
xx2_test = tst.scaled_2


#Rename variables for ease of reference
y_train_new <- to_categorical(y_train)
y_test_new <- to_categorical(y_test)
b_size <- 32 #Batch size

dim(x_train)


###SUBQUESTION 1



##FNN

FNN <- function(tr, te, output = 3){

  input_sh <- dim(tr)[2]
  nnmodel <- keras_model_sequential()
  dropout <- 0.5
  nnmodel %>%
    layer_dense(units = 512, input_shape = input_sh) %>%
    layer_activation(activation = 'relu') %>%
    layer_dense(units = 512) %>% ##this is an additional second layer that I added - it can be removed
    layer_activation(activation = 'relu') %>% ##this is an additional second layer that I added - it can be removed
    layer_dense(units = 512) %>% ##this is an additional second layer that I added - it can be removed
    layer_activation(activation = 'relu') %>% ##this is an additional second layer that I added - it can be removed
    layer_dense(units = output) %>% ## last layer must be the number of types of activation
    layer_activation(activation = 'softmax') ## if regression, last layer shpuld turn into activation = 'linear'
  
  nnmodel %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
  )
  #Training
  history <- nnmodel %>% fit(
    tr, y_train_new, 
    batch_size = b_size,
    epochs = 20,
    verbose =1,
    validation_split = 0.1
  )
  
  score <- nnmodel %>% evaluate(
    te, y_test_new,
    batch_size = b_size,
    verbose = 1
  )
  print(score)
}

CNN <- function(tr, te, ep = 5, output = 3){
  model <- keras_model_sequential()
  
  dim(tr) <- c(9352, 1000,1)
  dim(te) <- c(2434, 1000,1)
  
  model %>%
    layer_conv_1d(filters = 32, kernel_size = c(3), activation = "relu",
                  input_shape = c(dim(tr)[2], 1), padding = 'same') %>%
    layer_max_pooling_1d(pool_size = c(3)) %>%
    layer_conv_1d(filters = 64, kernel_size = c(3), activation = "relu", padding = 'same') %>%
    layer_max_pooling_1d(pool_size = c(3)) %>%
    layer_conv_1d(filters = 64, kernel_size = c(3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    #layer_dropout(0.5) %>%  
    layer_dense(units = 64, activation = "relu") %>%
    #layer_dropout(0.5) %>%
    layer_dense(units = output, activation = "softmax")
  
  ## compiling the model
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  model %>% fit(
    tr, y_train_new,
    epochs = ep, batch_size=64
  )
  score <- model %>% evaluate(
    te, y_test_new,
    batch_size = b_size,
    verbose = 1
  )
  print(score)
}


##Sub 1
FNN(xx1_train, xx1_test)
CNN(xx1_train, xx1_test)


##Sub2

FNN(xx2_train, xx2_test)
CNN(xx2_train, xx2_test)

#Sub3

xx3_train_flat <- xx3_train
dim(xx3_train_flat)
xx3_test_flat <- xx3_test
dim(xx3_test_flat)

CNNv2 <- function (tr, te, ep = 5, output=3){
  ## Defining the architecture
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_2d(filters = 32, kernel_size = c(2, 1), activation = "relu",
                  input_shape = c(1000, 3, 1)) %>%
    layer_max_pooling_2d(pool_size = c(2, 1)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2, 2), activation = "relu", padding = "same") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2, 1), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = output, activation = "softmax")
  
  model
  
  ## compiling the model
  model %>% compile(
    optimizer = "adam",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  ## training
  
  model %>% fit(
    tr, y_train_new,
    epochs = ep, batch_size=64
  )
  score <- model %>% evaluate(
    te, y_test_new,
    batch_size = b_size,
    verbose = 1
  )
  print(score)
}

FNN(xx3_train_flat, xx3_test_flat)

xx3_train_new <- xx3_train_flat
dim(xx3_train_new) <- c(9352, 1000, 3, 1)

xx3_test_new <- xx3_test_flat
dim(xx3_test_new) <- c(2434, 1000, 3, 1)

CNNv2(xx3_train_new, xx3_test_new, ep = 12)








