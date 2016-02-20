##
## data-manip.R
##

setwd("~/Documents/AG-LS/")


otus = list.files("./data/long_time/taxa/", pattern = "*.txt", full.names = TRUE)


df.list = list()
for (i in c(1:length(otus))){
  df = read.delim(otus[i], header = TRUE, stringsAsFactors = FALSE)
  df.list[[i]] = df
  names(df.list)[i] = substr(strsplit(otus[i], split = "otu_15k_")[[1]][2], 1, 2)
}


transpose.df = function(df.x, microbe.names){
  df.x <- as.data.frame(t(df.x))
  colnames(df.x) <- microbe.names
  id = row.names(df.x)
  dates = sapply(id, FUN = function(x) strsplit(x, split = "LS.")[[1]][2]) 
  meta.df = data.frame(sample.id = id,
                       sample.date = as.Date(dates, format = "%m.%d.%Y"))
  meta.df$day = order(meta.df$sample.date) - 1
  df.x = cbind(meta.df, df.x) 
  return(df.x)
}

transposed.df.list = list()
for (i in c(1:length(df.list))){
  animal.level = as.numeric(substr(names(df.list)[i], 2,2))
  df.temp = df.list[[i]]
  sample.cols = grep("LS", names(df.temp))
  transposed.df.list[[i]] = transpose.df(df.temp[,sample.cols], microbe.names = df.temp[,animal.level])
  names(transposed.df.list)[i] = names(df.list)[i]
}

## save transposed df's

for (i in c(1:length(transposed.df.list))){
  write.csv(transposed.df.list[[i]], file = 
              paste0("./data/long_time_cleaned/AG-LS-", names(transposed.df.list)[i], ".csv"), 
            row.names = FALSE, quote = FALSE)
}