library(ISLR2)
library(glmnet)
library(leaps)
library(pls)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary ~ ., data = Hitters ,
                            nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary$rsq
plot(1:19,reg.summary$rsq,type="h",ylim=0:1)

which.max(reg.summary$adjr2)
plot(1:19,reg.summary$adjr2,type="h",ylim=0:1)

plot(regfit.full , scale = "adjr2")

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters ,
                           nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters ,
                           nvmax = 19, method = "backward")
summary(regfit.bwd)


set.seed (1)
train <- sample(c(TRUE , FALSE), nrow(Hitters),
                  replace = TRUE)
test <- (! train)
regfit.best <- regsubsets(Salary ~ .,
                            data = Hitters[train , ], nvmax = 19)

test.mat <- model.matrix(Salary ~ ., data = Hitters[test , ])

val.errors <- rep(NA , 19)
for (i in 1:19) {
   coefi <- coef(regfit.best , id = i)
   pred <- test.mat[, names(coefi)] %*% coefi
   val.errors[i] <- mean(( Hitters$Salary[test] - pred)^2)
}

which.min(val.errors)
coef(regfit.best , 7)

k <- 10
n <- nrow(Hitters)
set.seed (1)
folds <- sample(rep (1:k, length = n))
cv.errors <- matrix(NA , k, 19,
                      dimnames = list(NULL , paste (1:19)))

predict.regsubsets <- function(object , newdata , id , ...) {
   form <- as.formula(object$call [[2]])
   mat <- model.matrix(form , newdata)
   coefi <- coef(object , id = id)
   xvars <- names(coefi)
   mat[, xvars] %*% coefi
   }

 for (j in 1:k) {
   best.fit <- regsubsets(Salary ~ .,
                           data = Hitters[folds != j, ],
                           nvmax = 19)
   for (i in 1:19) {
     pred <- predict(best.fit , Hitters[folds == j, ], id = i)
     cv.errors[j, i] <-
      mean(( Hitters$Salary[folds == j] - pred)^2)
     }
   }

mean.cv.errors <- apply(cv.errors , 2, mean)
plot(mean.cv.errors , type = "b")

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

library(glmnet)
grid <- 10^ seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

ridge.mod$lambda [50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
predict(ridge.mod , s = 50, type = "coefficients")[1:20 , ]

set.seed (1)
train <- sample (1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train , ], y[train], alpha = 0,
                      lambda = grid , thresh = 1e-12)
ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ])
mean(( ridge.pred - y.test)^2)

mean(( mean(y[train ]) - y.test)^2)
ridge.pred <- predict(ridge.mod , s = 1e10 , newx = x[test , ])
mean(( ridge.pred - y.test)^2)

ridge.pred <- predict(ridge.mod , s = 0, newx = x[test , ],
                        exact = T, x = x[train , ], y = y[train ])
mean(( ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod , s = 0, exact = T, type = "coefficients",
          x = x[train , ], y = y[train ])[1:20 , ]

set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod , s = bestlam ,
                        newx = x[test , ])
mean(( ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out , type = "coefficients", s = bestlam)[1:20 , ]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1,
                      lambda = grid)
plot(lasso.mod)

set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam ,
                        newx = x[test , ])
mean(( lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients",
                        s = bestlam)[1:20 , ]
lasso.coef

