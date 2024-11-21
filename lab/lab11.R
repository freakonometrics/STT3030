library(FactoMineR)

data(decathlon)
deca <- decathlon[14:41,1:10]
head(deca)

deca.scaled <- scale(x = deca,
                     center = TRUE,
                     scale = TRUE)

S <- cor(deca.scaled)

e <- eigen(S)

pca <- prcomp(deca.scaled)

U <- pca$rotation[,1:2]

dim(U)

dim(deca.scaled)

projection <- deca.scaled%*%U

projection

rbPal <- colorRampPalette(c('red','blue'))

decacolors <- rbPal(10)[as.numeric(cut(decathlon[14:41,]$Rank,breaks = 10))]

plot(projection, col=decacolors, pch=19)

biplot(pca,scale=0)
abline(h=0)
abline(v=0)

pca$rotation[,1]

biplot(pca,choices = c(1,3),scale=0)
abline(h=0)
abline(v=0)

res.pca <- PCA(deca.scaled,graph=TRUE)

summary(res.pca)

plot(res.pca)




