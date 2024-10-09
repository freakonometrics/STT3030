library("ROCR")

loc_fichier = "http://freakonometrics.free.fr/titanic.RData"
download.file(loc_fichier, "titanic.RData")
load("titanic.RData")
str(base)
base = base[,1:7]
base = base[!is.na(base$Age),]

glm(Survived~1, family=binomial,data=base)
predict(glm(Survived~1, family=binomial,
            data=base), type="response")[1]

reg = glm(Survived ~ Sex+Age+Pclass+SibSp,
          family = "binomial", data = base)

library(ROCR)
Y = base$Survived
S = predict(reg,type="response")
pred = prediction(S,Y)
plot(performance(pred,"tpr","fpr"))
performance(pred, measure = "auc")

set.seed(123)
train <- sample(1:nrow(base), nrow(base)*.7,
                replace = FALSE)
train_base = base[train,]
test_base  = base[test,]

reg = glm(Survived ~ Sex+Age+Pclass+SibSp,
          family = "binomial", data = train_base)

library(ROCR)
par(mfrow = c(1,2))
Y = train_base$Survived
S = predict(reg, type="response", newdata = train_base)
pred = prediction(S,Y)
plot(performance(pred,"tpr","fpr"))
performance(pred, measure = "auc")@y.values[[1]]

Y = test_base$Survived
S = predict(reg, type="response", newdata = test_base)
pred = prediction(S,Y)
plot(performance(pred,"tpr","fpr"))
performance(pred, measure = "auc")@y.values[[1]]

library("readxl")
library("tree")
library("rpart")
library("rpart.plot")

arbre = rpart(as.factor(Survived) ~ Sex+Age+Pclass+SibSp,
              data = train_base)

library(ROCR)
par(mfrow = c(1,2))
Y = train_base$Survived
S = predict(arbre, type="prob", newdata = train_base)[,"1"]
pred = prediction(S,Y)
plot(performance(pred,"tpr","fpr"))
performance(pred, measure = "auc")@y.values[[1]]

Y = test_base$Survived
S = predict(arbre, type="prob", newdata = test_base)[,"1"]
pred = prediction(S,Y)
plot(performance(pred,"tpr","fpr"))
performance(pred, measure = "auc")@y.values[[1]]

library(rpart.plot)
prp(arbre ,type =2, extra =1)
rpart.plot(arbre, box.palette="RdBu", nn=TRUE)

#  https://archive.ics.uci.edu/ml/datasets/Wine
download.file("https://archive.ics.uci.edu/static/public/109/wine.zip","wine.zip")
unzip("wine.zip")

Data = read.table('wine.data',sep=',')
colnames(Data) = c('Cultivars','Alcohol','Malicacid','Ash','Alcalinity', 'Magnesium',
                   'Phenols','Flavanoids', 'Nonflavanoidphenols','Proanthocyanins',
                   'Colorintensity', 'Hue', 'ODtwo','Proline')

head(Data)
dim(Data)
class(Data$Cultivars)
Data$Cultivars = factor(Data$Cultivars)

train_test_split <- function(prop,data) {
  id <- sample(nrow(data))
  nent <- floor(prop*nrow(data))
  ent_data <- data[id[1:nent],]
  tes_data <- data[id[(nent+1):nrow(data)],]

  return(list(ent_data,tes_data))
}

set.seed(1234)
allData <- train_test_split(0.8,Data)
ent_data <- allData[[1]]
tes_data <- allData[[2]]

library(tree)
colnames(ent_data)
fit <- tree(Cultivars~., ent_data)
summary(fit)
fit
plot(fit)
text(fit , pretty = 0)

library(rpart)
fitrp <- rpart(Cultivars~., ent_data)
fitrp
plot(fitrp)
text(fitrp , pretty = 0)
library(rpart.plot)
prp(fitrp ,type =2, extra =1)
rpart.plot(fitrp, box.palette="RdBu", nn=TRUE)

preds <- predict(fit,tes_data)
preds

preds <- factor(apply(preds,1,which.max))
DTPrec = sum(tes_data$Cultivars == preds)/nrow(tes_data)

cv <- cv.tree(fit , FUN = prune.misclass)

cv
par(mfrow = c(1, 2))
plot(cv$size , cv$dev, type = "b")
plot(cv$k, cv$dev, type = "b")

fit.prune <- prune.misclass(fit, best = 4)
par(mfrow = c(1, 1))
plot(fit.prune)
text(fit.prune , pretty = 0)

preds <- predict(fit.prune,tes_data)
preds <- factor(apply(preds,1,which.max))
PrunePrec = sum(tes_data$Cultivars == preds)/nrow(tes_data)

DTPrec
PrunePrec

library(rpart)
fit <- rpart(Cultivars~., data=ent_data)
fit

fit <- rpart(Cultivars~., data=ent_data,minsplit=20,cp=0, method = "class")
fit
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)

preds <- predict(fit,tes_data)
preds <- factor(apply(preds,1,which.max))
sum(tes_data$Cultivars == preds)/nrow(tes_data)

Data <- read_excel('energy.xlsx')
head(Data)
dim(Data)

n <- nrow(Data)
n_train <- floor(n*0.8)
n_test <- n - n_train
ordre <- sample(n,n)
Train <- Data[ordre[1:n_train],]
Test <- Data[ordre[(n_train+1):n],]

xnam <- paste("X", 1:8, sep="")
fmla <- as.formula(paste("Y1 ~ ", paste(xnam, collapse= "+")))

fit <- rpart(fmla, data=Train, minsplit=5,
             cp=0, method='anova')
fit
par(mfrow=c(1,2))
par(xpd = TRUE)
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)

prune(fit,cp=0.01)

printcp(fit)
fit_2 = prune(fit,cp=3.0830e-05)
plot(fit_2)

preds <- predict(fit,Test)
sum(Test$Y1 - preds)^2/n_test

preds <- predict(fit_2,Test)
sum(Test$Y1 - preds)^2/n_test


library(cparty)


