##
## clean-unifrac.R
##

setwd("~/Documents/AG-LS/")

df = read.delim("~/Documents/AG-LS/data/long_time/beta_diversity/unweighted_unifrac_pc.txt", header = TRUE, stringsAsFactors = FALSE)

ids = df$X
dates = sapply(ids, FUN = function(x) strsplit(x, split = "LS.")[[1]][2])
meta.df = cbind(data.frame(smaple.id = ids, sample.date = as.Date(dates, format = "%m.%d.%Y")), df)

df.rel = as.matrix(meta.df[,-c(1:3)])
row.names(df.rel) = colnames(df.rel)

df.rel.sorted = df.rel[order(meta.df$sample.date), order(meta.df$sample.date)]


write.table(df.rel.sorted, file = "./data/long_time_cleaned/unifrac_dist_clean.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
