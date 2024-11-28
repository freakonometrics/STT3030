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

# --- autres fonctions

library(FactoMineR)
res.pca = PCA(deca.scaled, graph = FALSE)
library(factoextra)
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var = "cos2", repel = TRUE)
fviz_pca_var(res.pca, col.var = "contrib")
fviz_cos2(res.pca, choice = "var", axes = 1:2)
fviz_contrib(res.pca, choice = "var", axes = 1:2)

facto_summarize(res.pca, "var", axes = 1:2)

fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", repel = TRUE)
fviz_pca_ind(res.pca, 
             col.ind = "cos2",
             pointsize = "cos2", 
             pointshape = 21)
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

fviz_pca_biplot(res.pca, repel = TRUE)

var <- get_pca_var(res.pca)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
fviz_pca_var(res.pca, col.var = grp)

res.pca = PCA(decathlon2, ind.sup = 24:27, quanti.sup = 11:12, quali.sup = 13, graph=FALSE)
fviz_pca_ind(res.pca, habillage = 13, addEllipses =TRUE, ellipse.type = "confidence")

decathlon3 = decathlon2
for(i in c(1,5,6,10)) decathlon3[,i]=rank(decathlon3[,i],ties="random")
for(i in c(2,3,4,7,8,9)) decathlon3[,i]=rev(rank(decathlon3[,i],ties="random"))
str(decathlon3)
res.pca = PCA(decathlon3[,1:10], graph = FALSE)
fviz_pca_biplot(res.pca, repel = TRUE)
