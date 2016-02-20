##
## dim-reductions.R
##
library(ggplot2)
library(plotly)

setwd("~/Documents/AG-LS/")

##################################################
#--- MDS
##################################################

df = read.delim("./data/long_time/beta_diversity/unweighted_unifrac_dm.txt", header = TRUE, stringsAsFactors = FALSE)

dist.matrix = as.matrix(df[,-1])
row.names(dist.matrix) = df$X

fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim
fit # view results

res = data.frame(x = fit$points[,1], y = fit$points[,2])
dates = sapply(row.names(dist.matrix), FUN = function(x) strsplit(x, split = "LS.")[[1]][2]) 
res$sample.dates = as.Date(dates, format = "%m.%d.%Y")

add.stage = function(df.x){
  df.x$stage = 0
  df.x$stage[which(df.x$sample.dates > as.Date("2014-01-01"))] = 1
  df.x$stage = as.factor(df.x$stage)
  return(df.x)
}

res$stage = 0
res$stage[which(res$sample.dates > as.Date("2014-01-01"))] = 1
res$stage = as.factor(res$stage)
ggplot(res, aes(x = x, y = y, label = sample.dates, colour = stage)) + geom_text() -> p

##################################################
#--- PCA
##################################################

pca.computer = function(df.x, take.log = TRUE, imputing.val = 1e-6){
  numeric.df = df.x[,-c(1:3)]
  if (take.log == TRUE){
    numeric.df = log(imputing.val + numeric.df)
  }
  pca = prcomp(numeric.df)
  pca.res = cbind(as.Date(df.x$sample.date), as.data.frame(pca$x))
  names(pca.res)[1] = "sample.dates"
  return(add.stage(pca.res))
}


df.genus = read.csv("./data/long_time_cleaned/AG-LS-L6.csv", header = TRUE, stringsAsFactors = FALSE)
pca.genus = pca.computer(df.genus)

ggplot(pca.genus, aes(x = PC1, y = PC2, label = sample.dates, colour = stage)) + geom_text() -> p

df.family = read.csv("./data/long_time_cleaned/AG-LS-L5.csv", header = TRUE, stringsAsFactors = FALSE)
pca.family = pca.computer(df.family, take.log = FALSE)
ggplot(pca.family, aes(x = PC1, y = PC2, label = sample.dates, colour = stage)) + geom_text() -> p

df.order = read.csv("./data/long_time_cleaned/AG-LS-L4.csv", header = TRUE, stringsAsFactors = FALSE)
pca.order = pca.computer(df.order, take.log = FALSE)
ggplot(pca.order, aes(x = PC1, y = PC2, label = sample.dates, colour = stage)) + geom_text() -> p
