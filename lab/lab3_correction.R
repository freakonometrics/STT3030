## ------------------------------------------------------------------------------------------------------------------
getwd()
# A modifier:
#setwd("/Users/agathefernandesmachado/stt3030/lab2")


## ------------------------------------------------------------------------------------------------------------------
#install.packages("ISLR")
#install.packages("nnet")
#install.packages("glmnet")


## ------------------------------------------------------------------------------------------------------------------
library(ISLR)
library(nnet)
library(glmnet)


## ------------------------------------------------------------------------------------------------------------------
head(Hitters)
?Hitters


## ------------------------------------------------------------------------------------------------------------------
dim(Hitters)


## ------------------------------------------------------------------------------------------------------------------
cat("Nombre de lignes comportant des données manquantes:", sum(is.na(Hitters)), "\n")
cat("Indice des lignes avec des données manquantes:", which(is.na(Hitters)), "\n")


## ------------------------------------------------------------------------------------------------------------------
Hitters <- na.omit(Hitters)


## ------------------------------------------------------------------------------------------------------------------
dim(Hitters)
sum(is.na(Hitters)) # on s'assure qu'on a bel et bien retirer les lignes avec valeurs manquantes


## ------------------------------------------------------------------------------------------------------------------
cat(round((322-263)/322*100, 2), "% de lignes", "\n")


## ------------------------------------------------------------------------------------------------------------------
colnames(Hitters)


## ------------------------------------------------------------------------------------------------------------------
hist(Hitters$Salary, xlab = "Salary", main = "Histogram of Salary", breaks = 20)


## ------------------------------------------------------------------------------------------------------------------
?glmnet


## ------------------------------------------------------------------------------------------------------------------
i_Y <- which(colnames(Hitters)=="Salary")
i_Y


## ------------------------------------------------------------------------------------------------------------------
x <- as.matrix(Hitters[,-19])
x[1:3,]


## ------------------------------------------------------------------------------------------------------------------
x <- model.matrix(Salary~., Hitters)[, -1]
x[1:3,]


## ------------------------------------------------------------------------------------------------------------------
y <- Hitters$Salary


## ------------------------------------------------------------------------------------------------------------------
ridgefit <- glmnet(x, y, alpha = 0)


## ------------------------------------------------------------------------------------------------------------------
plot(ridgefit, xvar = "lambda")

## ------------------------------------------------------------------------------------------------------------------
dim(coef(ridgefit))


## ------------------------------------------------------------------------------------------------------------------
?predict.glmnet
predict(ridgefit , s = 50, type = "coefficients")[1:20,]


## ------------------------------------------------------------------------------------------------------------------
set.seed(2024)
n <- nrow(x) # Nombre d'observations dans le jeu de données "Hitters"
prop <- 0.8 # Proportion du jeu de données qu'on désire avoir dans l'échantillon d'entraînement (80%)
ntrain <- floor(nrow(x)*prop) # Nombre d'observations souhaité dans l'échantillon d'entraînement (80% * n)
ntest <- n - ntrain # Nombre d'observations dans l'échantillon de test (20% * n)

# On mélange les numéros de ligne du jeu de données et on conserve les ntrain premières 
train_id <- sample(1:nrow(x), size = ntrain, replace = FALSE)
test <- (-train_id)


## ------------------------------------------------------------------------------------------------------------------
Hitters_train <- Hitters[train_id, ]
x_train <- model.matrix(Salary~., Hitters_train)[,-1]
y_train <- Hitters_train$Salary
dim(x_train) # Dimension de la matrice obtenue


## ------------------------------------------------------------------------------------------------------------------
Hitters_test <- Hitters[(ntrain+1):n,]
x_test <- model.matrix(Salary~., Hitters_test)[,-1]
y_test <- Hitters_test$Salary
dim(x_test)


## ------------------------------------------------------------------------------------------------------------------
tabl <- exp(seq(-8, 14, by = .01))


## ------------------------------------------------------------------------------------------------------------------
cvfit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = tabl)
# k = nfolds ici (10 par défaut)


## ------------------------------------------------------------------------------------------------------------------
plot(cvfit)


## ------------------------------------------------------------------------------------------------------------------
cvfit$lambda.min # lambda optimal
coef(cvfit, s = "lambda.min")[1:20,] # paramètres optimaux
cvfit$cvm[cvfit$lambda==cvfit$lambda.min] # EQM avec paramètres optimaux


## ------------------------------------------------------------------------------------------------------------------
cvfit$lambda.1se
coef(cvfit, s = "lambda.1se")[1:20,] # paramètres associés à lambda.1se
cvfit$cvm[cvfit$lambda==cvfit$lambda.1se] # EQM associé à lambda.1se


## ------------------------------------------------------------------------------------------------------------------
fit <- glmnet(x_train, y_train, alpha = 0, lambda = tabl)
plot(fit, xvar="lambda")
abline(v = log(cvfit$lambda.min)) 
abline(v = log(cvfit$lambda.1se))

# On peut ajouter le numéro des coefficients (associé à un numéro de colonnes de x_train) pour chaque courbe
vnat <- coef(fit)
vnat <- vnat[-1, ncol(vnat)]
axis(2, at = vnat, line = -2, label = as.character(1:19), las = 1, tick = FALSE, cex.axis = 1)


## ------------------------------------------------------------------------------------------------------------------
pred <- predict(cvfit, s = cvfit$lambda.min, newx = x_test)
pred[1:5, ]


## ------------------------------------------------------------------------------------------------------------------
# calcul_eqm: retourne l'EQM entre deux vecteurs.
# pred: vecteurs de prédictions
# y: vecteur de vraies valeurs
calcul_EQM <- function(y, preds){
  eqm <- sum((preds - y)^2)/length(y)
  eqm
}


## ------------------------------------------------------------------------------------------------------------------
ridge_EQM <- calcul_EQM(y_test, pred)
ridge_EQM


## ------------------------------------------------------------------------------------------------------------------
tabl <- exp(seq(-8, 14, by = .01))


## ------------------------------------------------------------------------------------------------------------------
cvfit <- cv.glmnet(x_train, y_train, alpha = 1, lambda = tabl)
# k = nfolds ici (10 par défaut)


## ------------------------------------------------------------------------------------------------------------------
plot(cvfit)


## ------------------------------------------------------------------------------------------------------------------
lambda <- cvfit$lambda.min
cat("Lambda:", lambda, "\n")
coef <- coef(cvfit, s = "lambda.min")[1:20,]
cat("Coefficients associés à lambda:", coef, "\n")
eqm <- cvfit$cvm[cvfit$lambda==cvfit$lambda.min] 
cat("EQM associée à lambda:", eqm, "\n")


## ------------------------------------------------------------------------------------------------------------------
lambda <- cvfit$lambda.1se
cat("Lambda:", lambda, "\n")
coef <- coef(cvfit, s = "lambda.1se")[1:20,]
cat("Coefficients associés à lambda:", coef, "\n")
eqm <- cvfit$cvm[cvfit$lambda==cvfit$lambda.1se] 
cat("EQM associée à lambda:", eqm, "\n")


## ------------------------------------------------------------------------------------------------------------------
fit <- glmnet(x_train, y_train, alpha = 1, lambda = tabl)
plot(fit, xvar="lambda")
abline(v = log(cvfit$lambda.min)) 
abline(v = log(cvfit$lambda.1se))

# On peut ajouter le numéro des coefficients (associé à un numéro de colonnes de x_train) pour chaque courbe
vnat <- coef(fit)
vnat <- vnat[-1, ncol(vnat)]
axis(2, at = vnat, line = -2, label = as.character(1:19), las = 1, tick = FALSE, cex.axis = 1)


## ------------------------------------------------------------------------------------------------------------------
pred <- predict(cvfit, s = cvfit$lambda.min, newx = x_test)
pred[1:5, ]


## ------------------------------------------------------------------------------------------------------------------
lasso_EQM <- calcul_EQM(y_test, pred)
lasso_EQM
ridge_EQM


## ------------------------------------------------------------------------------------------------------------------
lmfit <- lm(Salary~., data = Hitters_train)
pred <- predict(lmfit, newdata = Hitters_test)
lm_EQM <- calcul_EQM(y_test, pred)
lm_EQM
lasso_EQM
ridge_EQM

