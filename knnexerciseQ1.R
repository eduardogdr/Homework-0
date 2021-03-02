library(tidyverse)
library(caret)
library(dslabs)


datheights <- heights
set.seed(1, sample.kind="Rounding")
y<-heights$sex            
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- datheights %>% slice(-test_index)
test_set <- datheights %>% slice(test_index)


ns <- seq(1, 101, 3)
res <- sapply(ns, function(n){
                              set.seed(1, sample.kind="Rounding")
                              
                              
                              
                              fit<-knn3(sex ~ height, data=train_set, k=n)
                              y_hat_knn <- predict(fit, test_set, type = "class") 
                              F_val<- F_meas(data = y_hat_knn, reference = factor(test_set$sex))
                              #list(k=n, F_val=F_val)
                          
                               
                              
})
max(res)
ns[which.max(res)]