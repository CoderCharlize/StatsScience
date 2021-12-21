# Q1
replicate(50, rt(10, 5), simplify = FALSE)
mapply(rt, df = 1:50, n = 10)
lapply(1:20, seq)

set.seed(2020)
X <- matrix(rexp(200), 20, 10)
apply(X, 2, min)

sapply(CO2, is.numeric)


# Q2
setwd("~/desktop/MSc/Statistical Programming")
# we use skip = 2 (include row names) to skip the first commented line 
# we specify sep = "\t" as the column names have spaces
system.time(GTEX <- read.table("GTEx_analysis.txt.gz", skip = 2, sep = "\t", header = TRUE))
# install.packages("R.utils")
system.time(GTE <- data.table::fread("GTEx_analysis.txt.gz", data.table = FALSE))

GTEX_subset <- subset(GTEX,  select = -which(colnames(GTEX) == "Bladder"))
system.time(save(GTEX_subset, file = tempfile()))
system.time(data.table::fwrite(GTEX_subset, file = tempfile()))
# data.table::fwrite is faster than save


# Q3
sprays <- read.table("sprays.txt", header = TRUE)
head(sprays)
str(sprays)

for (i in unique(sprays[, "spray"])){
  print(mean(sprays[sprays[, "spray"] == i, "count"]))
}
tapply(sprays[, "count"], sprays[, "spray"], mean)
tapply(sprays[, "count"], sprays[, "spray"], quantile)


# Q4
GTE <- data.table::fread("GTEx_analysis.txt.gz", data.table = FALSE)
# use "sample" function in case of a tie
tissue_with_highest_expression <- apply(GTE[, -c(1:2)], 1, function(x){sample(names(x)[which.max(x)], 1)})
# tissue_with_highest_expression <- apply(GTE[, -c(1:2)], 1, function(x){sample(names(x)[x == max(x)], 1)})
# tissue_with_highest_expression <- colnames(GTE[, -c(1:2)])[max.col(GTE[, -c(1:2)], ties.method = "random")]
head(sort(table(tissue_with_highest_expression), decreasing = TRUE))

summary <- t(sapply(GTE[, -c(1:2)], function(i){
  c(mean = mean(i), 
    sd = sd(i), 
    median = median(i), 
    quantile(i, probs = c(0.05, 0.95)))
}))
head(summary)

APOB <- as.numeric(GTE[GTE[, 2] == "APOB", -c(1, 2)])
system.time(results <- apply(GTE[, -c(1:2)], 1, function(i){sum((i - APOB) ** 2)}))

# ignore time for transposition
# faster after transposing
GTE_t <- t(GTE[, -c(1:2)])
system.time(results <- apply(GTE_t, 2, function(i){sum((i - APOB) ** 2)}))
# system.time(results <- sapply(GTE_t, function(i){sum((i - APOB) ** 2)}))

print(GTE[order(results)[1:11], 2])
