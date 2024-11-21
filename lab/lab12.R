# setwd()
library(keras)
library(tidyr)
library(ggplot2)

encoder <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 64, activation = 'relu')%>%
  layer_dense(units = 2)


decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 256, activation = 'relu')%>%
  layer_dense(units = 784, activation = 'sigmoid')

autoencoder <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output))

autoencoder %>% compile(optimizer = 'adam', loss = 'MSE')

data <- dataset_mnist()

x_train <- data$train$x
x_test <- data$test$x
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
x_train <- x_train / 255
x_test <- x_test / 255

autoencoder %>% fit(x_train, x_train, epochs = 50, batch_size = 256, validation_data = list(x_test, x_test))


z = encoder(x_train)
plot(x=z[,1],y=z[,2],col=data$train$y)


image_1  <- as.data.frame(matrix(data=x_train[2, ],nrow=28,byrow=TRUE))
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

z[1:10,]

xtilde = autoencoder(x_train[1:10, ])
xtilde = decoder(z[1:10,])

image_1  <- as.data.frame(matrix(data=xtilde[2, ],nrow=28,byrow=TRUE))
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

plot(x=z[,1],y=z[,2],col=data$train$y)

par( mfrow= c(10,10),mai = c(0.01, 0.01, 0.01, 0.01) )
grid<- seq(-30,30,length.out=10)
zs <- expand.grid(grid,grid)
xtilde = decoder(as_tensor(zs))


for (idx in 1:100) {
  im <- matrix(data=xtilde[idx, ],nrow=28,byrow=T)
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255),xaxt='n',yaxt='n')
}
