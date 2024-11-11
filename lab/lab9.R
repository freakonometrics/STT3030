library(devtools)
devtools::install_github("husson/FactoMineR")
devtools::install_github("ModelOriented/factorMerger")

library(FactoMineR)


library(factoextra)
library(DALEX)
data(apartments)
MF = factorMerger::mergeFactors(response = apartments$m2.price,
                  factor = apartments$district,family = "gaussian")
plot(MF)

df = scale(USArrests)
fviz_nbclust(USArrests, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

km.res = kmeans(df, 4, nstart = 25)

fviz_cluster(km.res, data = USArrests, 
             ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, 
             ggtheme = theme_minimal())


res.dist = dist(df, method = "euclidean")
res.hc = hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)

res.hc = hclust(d = res.dist, method = "single")
fviz_dend(res.hc, cex = 0.5)

res.hc = hclust(d = res.dist, method = "average")
fviz_dend(res.hc, cex = 0.5)

res.dist = dist(df, method = "manhattan")
res.hc = hclust(d = res.dist, method = "average")
fviz_dend(res.hc, cex = 0.5)

hc = res.hc
dend_plot = fviz_dend(hc, k = 4, cex = 0.5, k_colors = "jco")
dend_data = attr(dend_plot, "dendrogram") 
dend_cuts = cut(dend_data, h = 10)
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")

data("decathlon")
df = scale(decathlon[,1:10])

res.dist = dist(df, method = "euclidean")
res.hc = hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)


res.dist = dist(t(df), method = "euclidean")
res.hc = hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)
