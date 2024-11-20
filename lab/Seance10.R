

library(mclust)
clPairs(data)
BIC <- mclustBIC(data)
plot(BIC)
summary(BIC)
gmm <- Mclust(data, x = BIC)
summary(gmm, parameters = TRUE)


GenGroup <- function(n,prob,centres){

 d <- ncol(centres)

  c <- nrow(centres)
  data <- matrix(data=rep(NA,n*d),nrow=n)
  v = rep(NA,n)

  nc <- rmultinom(1, n, prob)
  start=0

  for (i in 1:c){
    for (j in 1:d){
      data[(start+1):(start+nc[i]),j] <- rnorm(nc[i],mean=centres[i,j])
      v[(start+1):(start+nc[i])] <- i
    }
    start=start+nc[i]
  }

  return(list(data = data,
              class = v))
}

set.seed(1234)
n = 1000
prob = c(0.3,0.3,0.4)
centres = matrix(data=c(0,0,3,2,-2,0,5,5,-1),nrow=3,byrow=TRUE)
centres
out <- GenGroup(n,prob,centres)
data <- out[[1]]
assign <- out[[2]]

plot(data[,c(1,2)], col=assign)
plot(data[,c(1,3)], col=assign)
plot(data[,c(2,3)], col=assign)

library(mclust)

clPairs(data)

BIC <- mclustBIC(data)
plot(BIC)
summary(BIC)

  # https://mclust-org.github.io/mclust-book/chapters/04_classification.html
?Mclust
gmm <- Mclust(data, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")

gmm$classification
table(gmm$classification,assign)


clPairs(data,gmm$classification)

plot(gmm, what = "uncertainty")
plot(gmm,what="density")

gmm$z

?kmeans
out_km <- kmeans(data,3)
out_km$cluster

table(out_km$cluster,assign)

Davis = read.table("http://freakonometrics.free.fr/Davis.txt")
data = Davis[,c("height","weight")]

BIC <- mclustBIC(data)
plot(BIC)
summary(BIC)
gmm <- Mclust(data, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")
plot(gmm, what = "uncertainty")
plot(gmm,what="density")


BIC <- mclustBIC(data, G=2)
plot(BIC)
summary(BIC)
gmm <- Mclust(data, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")
plot(gmm, what = "uncertainty")
plot(gmm,what="density")
table(gmm$classification,Davis$sex)

loc = "https://archive.ics.uci.edu/static/public/485/tarvel+review+ratings.zip"
download.file(loc, destfile = 'tarvel+review+ratings.zip')
review <- read.table('google_review_ratings.csv', sep=',',header=TRUE)

review <- cbind(review$Category.7,review$Category.2,review$Category.6,review$Category.23,review$Category.16)
colnames(review) <- c('malls','resort','museums','monuments','clubs')

sum(is.na(review))

review <- na.omit(review)

sum(is.na(review))

head(review)

clPairs(review)

BIC <- mclustBIC(review,G=1:5,modelNames = "VII")

summary(BIC)

gmm <- Mclust(review, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")

gmm$classification

plot(gmm, what = "uncertainty")

# digression (préparation ACP)

library(mnormt)
r=.7
S = matrix(c(1,r*2,r*2,2^2),2,2)
mu = c(0,0)
X = rmnorm(1000, mean = mu, varcov = S)
plot(X)

vx=seq(-7,7,length=251)
vy = dmnorm(expand.grid(vx,vx), mean = mu, varcov = S)
contour(vx,vx,matrix(vy,length(vx),length(vx)),col="red",add=TRUE)

solve(S)
(E = eigen(S))
abline(a=0,b=E$vectors[2,1]/E$vectors[1,1],col="blue")
abline(a=0,b=E$vectors[2,2]/E$vectors[1,2],col="darkgreen")

mat_proj = E$vectors[,1] %*% solve(t(E$vectors[,1]) %*% E$vectors[,1]) %*% t(E$vectors[,1])
eigen(mat_proj)

Px = t(mat_proj %*% t(X))
points(Px,col="blue")
x_proj = sqrt(Px[,1]^2+Px[,2]^2)*c(-1,1)[1+(Px[,1]>0)*1]
var(x_proj)
var(x_proj)/(var(X[,1])+var(X[,2]))

part_variance_expliquee = function(theta){
  u = c(cos(theta),sin(theta))
  mat_proj = u %*% t(u)
  Px = t(mat_proj %*% t(X))
  x_proj = sqrt(Px[,1]^2+Px[,2]^2)*c(-1,1)[1+(Px[,1]>0)*1]
  var(x_proj)/(var(X[,1])+var(X[,2]))
}
(out = optim(fn = function(x) -part_variance_expliquee(x),
      par = pi/4))
c(cos(out$par),sin(out$par))
eigen(S)$vector
