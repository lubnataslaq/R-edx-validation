# R-edx-validation
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
library(tidyverse)
library(caret)
install.packages("e1071")
library(e1071)
# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,which(pvals < 0.01)]
fit <- train(x_subset, y, method = "glm")
fit$results

tt <- colttests(x, y,tstatOnly = FALSE)
pvals <- tt$p.value

tt <- colttests(x, y)
tt$p.value
dd <- sum(tt$p.value < 0.01)
ind <- which(pvals <= 0.01)
length(ind)
----
library(dslabs)
data("tissue_gene_expression")

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "knn", datax = data.frame(k = seq(1, 7, 2)))
fit

---------
Bootstrsap
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding") # if R 3.6 or later

ddf <- createResample(y, 10000)
xx <- sapply(ddf, function(num){
  quantile(y[num], 0.75)
})
sd(xx)
mean(xx)
