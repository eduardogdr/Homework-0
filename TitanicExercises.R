library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

dim(titanic_clean)

set.seed(42, sample.kind = "Rounding")
y <- titanic_clean$Survived

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

dim(train_set)
dim(test_set)

train_set %>% summarise(mean(Survived==1))

set.seed(3, sample.kind = "Rounding")
y_hat <- sample(c(0,1),nrow(test_set),replace=TRUE) %>% factor()
mean(y_hat== test_set$Survived)
     
train_set %>% group_by(Sex) %>% summarise(mean(Survived==1))
train_set %>% group_by(Pclass) %>% summarise(mean(Survived==1))

sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy

class_model <- ifelse(test_set$Pclass == "1", 1, 0)    # predict Survived=1 if female, 0 if male
mean(class_model == test_set$Survived)  

train_set %>% group_by(Sex,Pclass) %>% summarise(mean(Survived==1))

class_sex_model <- ifelse(test_set$Sex == "female" & test_set$Pclass==1, 1, 
                   ifelse(test_set$Sex == "female" & test_set$Pclass==2,1,0))# predict Survived=1 if female, 0 if male
                   mean(class_sex_model == test_set$Survived)

confusionMatrix(factor(sex_model),test_set$Survived)
confusionMatrix(factor(class_model),test_set$Survived)
confusionMatrix(factor(class_sex_model),test_set$Survived)

F_meas(factor(sex_model),test_set$Survived)
F_meas(factor(class_model),test_set$Survived)
F_meas(factor(class_sex_model),test_set$Survived)


###Part 2 
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived~Fare, method="lda", data = train_set )
Survived_fit<-predict(fit_lda,test_set)
mean(test_set$Survived==Survived_fit)

set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived~Fare, method="qda", data = train_set )
Survived_fit_qda<-predict(fit_qda,test_set)
mean(test_set$Survived==Survived_fit_qda)

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived~Age, method="glm", data = train_set )
Survived_fit_glm<-predict(fit_glm,test_set)
mean(test_set$Survived==Survived_fit_glm)

set.seed(1, sample.kind = "Rounding")
fit_glm_4 <- train(Survived~Fare+Sex+Pclass+Age, method="glm", data = train_set )
Survived_fit_glm_4<-predict(fit_glm_4,test_set)
mean(test_set$Survived==Survived_fit_glm_4)

set.seed(1, sample.kind = "Rounding")
fit_glm_all <- train(Survived~., method="glm", data = train_set )
Survived_fit_glm_all<-predict(fit_glm_all,test_set)
mean(test_set$Survived==Survived_fit_glm_all)

set.seed(6, sample.kind = "Rounding")
k <- seq(3,51,2)
fit_knn <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn$bestTune

ggplot(fit_knn)

knn_preds <- predict(fit_knn, test_set)
mean(knn_preds == test_set$Survived)

set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune

knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)

set.seed(10, sample.kind = "Rounding")
train_rpart <- train(Survived ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), data = train_set)
train_rpart$bestTune

rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)

plot(train_rpart$finalModel)
text(train_rpart$finalModel, cex = 0.75)

set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ ., method = "rf", tuneGrid = data.frame(mtry = seq(1, 7)), data = train_set, ntree=100)
train_rf$bestTune

rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)

varImp(train_rf)
