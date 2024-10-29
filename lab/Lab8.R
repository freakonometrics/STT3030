 #   install.packages("keras")
 #   reticulate::install_python()
library(keras)
install_keras()
library(tensorflow)

tf$constant("Hello TensorFlow!")
 #  source('Utility.R')

EQM <- function(pred,true){
  return (mean((pred-true)^2))
}

mnist <- dataset_mnist ()

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train)

dim(x_test)

library(tidyr)
library(ggplot2)

image_1  <- as.data.frame(x_train[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

x_train <- array_reshape(x_train , c(nrow(x_train), 784))
x_test <- array_reshape(x_test , c(nrow(x_test), 784))
y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)

dim(x_train)

y_test

x_train <- x_train / 255
x_test <- x_test / 255

NN_MNIST <- keras_model_sequential ()
NN_MNIST %>%
   layer_dense(units = 256, activation = "relu",
                input_shape = c(784)) %>%
   layer_dropout(rate = 0.4) %>%
   layer_dense(units = 128, activation = "relu") %>%
   layer_dropout(rate = 0.3) %>%
   layer_dense(units = 10, activation = "softmax")

# https://tensorflow.rstudio.com/reference/keras/layer_dropout.html

NN_MNIST %>% compile(loss = "categorical_crossentropy",
                      optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

system.time(hist <- NN_MNIST %>% fit(x_train , y_train , epochs = 30, batch_size = 128,
          validation_split = 0.2))
plot(hist , smooth = FALSE)

y_test <- mnist$test$y
y_test

NN_MNIST %>% predict(x_test)
pred = as.array(NN_MNIST %>% predict(x_test) %>% k_argmax())

library(pROC)
result <- pROC::multiclass.roc(pred,y_test)
plot.roc(result$rocs[[1]],
         print.auc=TRUE,
         legacy.axes = TRUE)

Y <- as.factor(mnist$train$y)
ent_data <- data.frame(cbind(x_train,Y))
ent_data$Y <- as.factor(ent_data$Y)


Y <- as.factor(mnist$test$y)
tes_data <- data.frame(cbind(x_test,Y))
tes_data$Y <- as.factor(tes_data$Y)

library(randomForest)
rf <- randomForest(Y~.,data=ent_data,ntree=20,nodesize=1000)
pred <- predict(rf,tes_data)
result <- pROC::multiclass.roc(pred,tes_data$Y)
plot.roc(result$rocs[[1]],
         print.auc=TRUE,
         legacy.axes = TRUE)

CNN_MNIST <- keras_model_sequential() %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu', input_shape = c(28,28,1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 10, activation = 'softmax')

CNN_MNIST

(28-3)/1 + 1

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train , c(nrow(x_train), 28,28,1))

y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)

CNN_MNIST %>% compile(loss = "categorical_crossentropy",
                     optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

hist <- CNN_MNIST %>% fit(x_train , y_train , epochs = 30, batch_size = 128,
                                     validation_split = 0.2)
plot(hist , smooth = FALSE)

y_test <- mnist$test$y
y_test


pred = as.array(CNN_MNIST %>% predict(x_test) %>% k_argmax())
CNNPrec <- Prec(pred,y_test)

CNN_MNIST$layers[[1]]$weights

filtered1 = CNN_MNIST$layers[[1]](x_train)
dim(filtered1)

image_1 = as.data.frame(as.matrix(filtered1[1,,,4]))
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

filtered2 = CNN_MNIST$layers[[2]](filtered1)
dim(filtered2)

image_1 = as.data.frame(as.matrix(filtered2[1,,,3]))
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

cifar10 <- dataset_cifar10()

?dataset_cifar10

x_train <- cifar10$train$x/255
x_test <- cifar10$test$x/255
y_train <- cifar10$train$y
y_test <- cifar10$test$y

dim(x_train)

y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)

CNN_CIFAR <- keras_model_sequential()


CNN_CIFAR <- CNN_CIFAR %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu', input_shape = c(32,32,3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 10, activation = 'softmax')

CNN_CIFAR

CNN_CIFAR %>% compile(loss = "categorical_crossentropy",
                      optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

hist <- CNN_CIFAR %>% fit(x_train , y_train , epochs = 50, batch_size = 256,
                          validation_split = 0.1)

CNN_CIFAR %>% evaluate(x_test, y_test)


