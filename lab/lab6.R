 library(randomForest)
 set.seed(1)
 bag.boston <- randomForest(medv ~ ., data = Boston,
                            subset = train, mtry = 12, importance = TRUE)  
 bag.boston
 yhat.bag_1 <- predict(bag.boston, newdata = Boston[-train, ]) 
 y <- Boston[-train, "medv"]
 plot(yhat.bag, y)
 abline(0, 1, col="red")
 mean((yhat.bag_1 - y)^2)
 bag.boston <- randomForest(medv ~ ., data = Boston, 
                            subset = train, mtry = 12, ntree = 25)
 yhat.bag_2 <- predict(bag.boston, newdata = Boston[-train, ]) 
 mean((yhat.bag_2 - y)^2)
 set.seed(1)
 rf.boston <- randomForest(medv ~ ., data = Boston,
                             subset = train, mtry = 6, importance = TRUE)
 yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
 mean((yhat.rf - y)^2)
 importance(rf.boston)
 importance(rf.boston)[order(importance(rf.boston)[,1]),]
 varImpPlot(rf.boston)
 
 ####
 library(leaps)
 regfit.full <- regsubsets(medv ~ ., data = Boston)
 summary(regfit.full)
 
 
 library(glmnet)
 Boston_X_train <- model.matrix(medv  ~ ., data = Boston[train, ])
 Boston_X_test  <- model.matrix(medv ~ ., data = Boston[-train, ])
 Boston_y_train <- Boston[train, "medv"]
 Boston_y_test <- Boston[-train, "medv"]
 
 set.seed (1)
 fit_lasso <- glmnet(Boston_X_train, Boston_y_train, alpha = 1)
 plot(fit_lasso)
 coef(fit_lasso,s=5)
 
 fit_lasso <- glmnet(Boston_X_train, Boston_y_train, alpha = 1,
                     standardize = TRUE)
 plot(fit_lasso)
 
 cv_fit_lasso <- cv.glmnet(Boston_X_train, Boston_y_train, alpha = 1)
 plot(cv.out)
 cv.out$glmnet.fit$beta[,1:7]
 