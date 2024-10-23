library(gbm)
library(ISLR2)
set.seed(123)

train = sample(nrow(Boston),size=round(.7*nrow(Boston)))

## Boosting

boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
gbm.perf(boost.boston)

yhat.boost <- predict(boost.boston,
                      newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - Boston[-train,"medv"])^2)


boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", 
                    interaction.depth = 4,
                    n.trees = 5000, 
                    cv.folds = 10)
summary(boost.boston)
gbm.perf(boost.boston)

boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", 
                    interaction.depth = 4,
                    n.trees = 5000, 
                    shrinkage=0.0005,
                    cv.folds = 10)
summary(boost.boston)
gbm.perf(boost.boston)

boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", 
                    interaction.depth = 4,
                    n.trees = 5000, 
                    shrinkage=0.025,
                    cv.folds = 10)
summary(boost.boston)
gbm.perf(boost.boston)

boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, 
                    shrinkage = 0.2, verbose = FALSE)

yhat.boost <- predict(boost.boston,
                        newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - Boston[-train,"medv"])^2)

## neural nets

library(MASS)
library(neuralnet)

maxs <- apply(Boston,2,max)
mins <- apply(Boston,2,min)
scaled <- as.data.frame(scale(Boston, center=mins, scale=maxs
                              - mins))

boston_train <- scaled[train,]
boston_test <- scaled[-train,]

#13:5:3:1 configuration
n <- names(boston_train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"],
                                      collapse = " + ")))
nn <- neuralnet(f, data=boston_train, hidden=c(5,3),linear.output=TRUE)

#y~ is not allowed, needs to be passed as an argument in the fitting function

#plot
plot(nn)
nn$net.result

#predicting medv using the neural network
pr.nn <- compute(nn,boston_test[,1:13])
pr.nn_ <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv)+
                             min(Boston$medv))
plot(Boston[-train,"medv"],pr.nn_)
abline(a=0,b=1,col="red")
mean((Boston[-train,"medv"] - pr.nn_)^2)
