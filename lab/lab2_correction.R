# Installation des packages nécessaires
#install.packages("MASS")
#install.packages("ISLR")
#install.packages("nnet")
#install.packages("ggplot2")

# Chargement des packages nécessaires
library(MASS)
library(ISLR)
library(nnet)
library(ggplot2)

# Répertoire de travail
# setwd("/Users/agathefernandesmachado/stt3030/lab2")

# Visualisation des premières lignes du jeu de données Boston
head(Boston)

# Type d'objet sous R du jeu de données Boston
class(Boston)

# Nombre d'observations et de variables explicatives
n <- nrow(Boston)
p <- ncol(Boston) - 1
cat("Nombre d'observations:", n, "\n")
cat("Nombre de variables explicatives:", p, "\n")

# Données manquantes
na_values <- sum(is.na(Boston))
cat("Nombre d'observations avec des données manquantes:", na_values, "\n")

# Histogramme de la variable medv
hist(Boston$medv, breaks = 20, main = "Histogramme de medv", xlab = "medv")

# Variables qualitatives et leur nombre de catégories
str(Boston)
unique(Boston$chas)
k1 <- length(unique(Boston$chas))
cat("Nombre de catégories de la variable chas:", k1, "\n")

unique(Boston$rad)
k2 <- length(unique(Boston$rad))
cat("Nombre de catégories de la variable rad:", k2, "\n")

# Régression linéaire simple avec lstat
set.seed(2024)
id <- sample(nrow(Boston))
prop <- 0.8
n_a <- round(prop * nrow(Boston))
Boston_a <- Boston[id[1:n_a],]
Boston_t <- Boston[id[(n_a + 1):nrow(Boston)],]

cat("Proportion de données d'entraînement:", round(nrow(Boston_a) / nrow(Boston) * 100), "%", "\n")
cat("Proportion de données de test:", round(nrow(Boston_t) / nrow(Boston) * 100), "%", "\n")

calcul_EQM <- function(y, preds) {
  eqm <- sum((preds - y)^2) / length(y)
  return(eqm)
}

fit_1 <- lm(medv ~ lstat, data = Boston_a)
summary(fit_1)

fit_1$coefficients
confint(fit_1)

b0 <- fit_1$coefficients[1]
b1 <- fit_1$coefficients[2]
preds_1 <- b0 + b1 * Boston_t$lstat
preds_1[1:5]

preds_1 <- predict(fit_1, newdata = Boston_t)
preds_1[1:5]

plot(Boston_a$lstat, Boston_a$medv, pch = 20, col = "coral1",
     xlab = "lstat", ylab = "medv")
lines(Boston$lstat, b0 + b1 * Boston$lstat, col = "brown4", lwd = 2)
points(Boston_t$lstat, Boston_t$medv, col = "blue", pch = 19, lwd = 2)
legend("topright", legend = c("Données d'entraînement", "Données de test"), 
       col = c("coral1", "blue"), pch = c(20, 19))

cat("EQM pour les données d'entraînement:", 
    round(calcul_EQM(Boston_a$medv, fit_1$fitted.values), 2), "\n")

cat("EQM pour les données de test:", 
    round(calcul_EQM(Boston_t$medv, preds_1), 2), "\n")

pred_int <- predict(fit_1, Boston_t, interval = "prediction")
pred_int[1:5,]

# Ajout de la variable qualitative chas
Boston$chas <- as.factor(Boston$chas)
Boston_a$chas <- as.factor(Boston_a$chas)
Boston_t$chas <- as.factor(Boston_t$chas)

unique(Boston$chas)

fit_2 <- lm(medv ~ lstat + chas, data = Boston_a)
summary(fit_2)

levels(Boston$chas)

chas_modif <- factor(Boston$chas, levels = c("1", "0"))
levels(chas_modif)

b0 <- fit_2$coefficients[1]
b1 <- fit_2$coefficients[2]
a1 <- fit_2$coefficients[3]
preds_2 <- b0 + b1 * Boston_t$lstat + a1 * as.numeric(Boston_t$chas == "1")
preds_2[1:5]

preds_2 <- predict(fit_2, newdata = Boston_t)
preds_2[1:5]

df <- data.frame(medv = Boston_t$medv, lstat = Boston_t$lstat, 
                 chas = Boston_t$chas, medv_pred = preds_2)
ggplot(df, aes(x = lstat, y = medv, color = chas)) +
  geom_point() +
  geom_line(aes(y = medv_pred), linewidth = 1) +  
  scale_color_manual(values = c("0" = "coral1", "1" = "darkorchid")) +
  labs(x = "lstat",
       y = "medv") +
  theme_minimal()

cat("EQM pour les données d'entraînement:", 
    round(calcul_EQM(Boston_a$medv, fit_2$fitted.values), 2), "\n")

cat("EQM pour les données de test:", 
    round(calcul_EQM(Boston_t$medv, preds_2), 2), "\n")

# Ajout d'un terme d'interaction entre lstat et chas
fit_3 <- lm(medv ~ lstat * chas, data = Boston_a)
summary(fit_3)

b0 <- fit_3$coefficients[1]
b1 <- fit_3$coefficients[2]
a1 <- fit_3$coefficients[3]
gamma <- fit_3$coefficients[4]
preds_3 <- b0 + b1 * Boston_t$lstat + a1 * as.numeric(Boston_t$chas == "1") +
  gamma * Boston_t$lstat * as.numeric(Boston_t$chas == "1")
preds_3[1:5]

preds_3 <- predict(fit_3, newdata = Boston_t)
preds_3[1:5]

df <- data.frame(medv = Boston_t$medv, lstat = Boston_t$lstat, 
                 chas = Boston_t$chas, medv_pred = preds_3)
ggplot(df, aes(x = lstat, y = medv, color = chas)) +
  geom_point() +
  geom_line(aes(y = medv_pred), size = 1) +  
  scale_color_manual(values = c("0" = "coral1", "1" = "darkorchid")) +
  labs(x = "lstat",
       y = "medv") +
  theme_minimal()

cat("EQM pour les données d'entraînement:", 
    round(calcul_EQM(Boston_a$medv, fit_3$fitted.values), 2), "\n")

cat("EQM pour les données de test:", 
    round(calcul_EQM(Boston_t$medv, preds_3), 2), "\n")

anova(fit_2, fit_3)

# Régression linéaire multiple
fit_4 <- lm(medv ~ ., data = Boston_a)
summary(fit_4)

names(summary(fit_4)$coefficients[which(summary(fit_4)$coefficients[,4] <= 0.05), 4])[-1]

preds_4 <- predict(fit_4, newdata = Boston_t)

cat("EQM pour les données d'entraînement:", 
    round(calcul_EQM(Boston_a$medv, fit_4$fitted.values), 2), "\n")

cat("EQM pour les données de test:", 
    round(calcul_EQM(Boston_t$medv, preds_4), 2), "\n")

# Régression linéaire multiple avec interactions
fit_5 <- lm(medv ~ .^2, data = Boston_a)
summary(fit_5)

length(fit_5$coefficients)

# Corrélation entre variables explicatives
cor(Boston$age, Boston$nox, method = "pearson")
cor(Boston$age, Boston$ptratio, method = "pearson")
cor(Boston$nox, Boston$ptratio, method = "pearson")

fit_6 <- lm(medv ~ ptratio + nox + age, data = Boston_a)
summary(fit_6)

fit_7 <- lm(medv ~ ptratio + age, data = Boston_a)
summary(fit_7)

# Régression polynomiale
plot(Boston$lstat, Boston$medv, pch = 20, xlab = "lstat", ylab = "medv")

plot(fit_1)

fit_8 <- lm(medv ~ lstat + I(lstat^2), data = Boston_a)
summary(fit_8)

b0 <- fit_8$coefficients[1]
b1 <- fit_8$coefficients[2]
b2 <- fit_8$coefficients[3]
preds_8 <- b0 + b1 * Boston_t$lstat + b2 * Boston_t$lstat^2
preds_8[1:5]

preds_8 <- predict(fit_8, newdata = Boston_t)
preds_8[1:5]

df <- data.frame(medv = Boston_t$medv, lstat = Boston_t$lstat, 
                 medv_pred = preds_8)
ggplot(df, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_line(aes(y = medv_pred), color = "blue") +
  labs(x = "lstat",
       y = "medv") +
  theme_minimal()

cat("EQM pour les données d'entraînement:", 
    round(calcul_EQM(Boston_a$medv, fit_8$fitted.values), 2), "\n")

cat("EQM pour les données de test:", 
    round(calcul_EQM(Boston_t$medv, preds_8), 2), "\n")
