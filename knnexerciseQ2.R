library(tidyverse)
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1, sample.kind="Rounding")

head(tissue_gene_expression$x)
dat_tge <- as.data.frame(tissue_gene_expression)
y<-dat_tge$y

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat_tge %>% slice(-test_index)
test_set <- dat_tge%>% slice(test_index)


ns <- seq(1, 11, 2)
res <- sapply(ns, function(n){
  
  set.seed(1, sample.kind="Rounding")
  
  fit<-knn3(y~., data=train_set, k=n)
  y_hat <- predict(fit, test_set, type = "class") 
  o_acc <- confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
  list(k=n, o_acc=o_acc)
  
  
  
})
res
