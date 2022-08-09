setwd("C:/Users/Monika/Dysk Google/IBwP/Dane dla Studentów")
dreg=read.csv("dane_reg.csv",header=TRUE,sep=";",dec = ",")
# Bêdziemy próbowaæ modele regresji wieku na podstawie pozosta³ych danych
df=dreg[complete.cases(dreg),]

library(caTools)
split <- sample.split(df$TBW, SplitRatio=0.8)

train_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)

library("ModelMetrics") #mse
# 1 Multiple regression model - all variables
lm_tbw <- lm(TBW~., data=train_set)
pred_lm_tbw <- predict(lm_tbw, test_set)
mse_lm_tbw <- round(mse(pred_lm_tbw, test_set$TBW), 2)
wyn_lm <- round(sqrt(mse_lm_tbwl), 2)

# 2 Simple tree
library("rpart") # rpart
regtree_tbw <- rpart(formula = TBW~.,data=train_set, control = rpart.control(cp = 0))
pred_regtree_tbw<- predict(regtree_tbw, test_set, type = "vector")
mse_regtree_tbw <- round(mse(pred_regtree_tbw, test_set$TBW), 2)
wyn_regtree <- round(sqrt(mse_regtree_tbw), 2)

# 3 Random Forest
library("randomForest") #randomForest
randomfor_tbw <- randomForest(formula = TBW~.,data=train_set, ntree=1000)
pred_randomfor_tbw<- predict(randomfor_tbw, test_set)
mse_randomfor_tbw <- round(mse(pred_randomfor_tbw, test_set$TBW), 2)
wyn_randomfor <- round(sqrt(mse_randomfor_tbw), 2)
# 4 Gradient Boosting
library("gbm")
boosting_tbw <- gbm(formula = TBW~.,data=train_set, n.trees =1000,distribution="gaussian",interaction.depth = 2)
pred_boosting_tbw<- predict(boosting_tbw, test_set,n.trees=1000)
mse_boosting_tbw <- round(mse(pred_boosting_tbw, test_set$TBW), 2)
wyn_boosting <- round(sqrt(mse_boosting_tbw), 2)

# 5 SVM linear
library("e1071")
tune.out <- tune(svm, TBW~., data=train_set, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
bestmod <- tune.out$best.model
pred_svmL_tbw <- predict(bestmod, test_set)
mse_svmL_tbw <- round(mse(pred_svmL_tbw, test_set$TBW), 2)
wyn_svmL <- round(sqrt(mse_svmL_tbw), 2)

# 6 SVM Polynomial
tune.out <- tune(svm, TBW~., data=train_set, kernel = "polynomial", ranges = list(cost=c( 0.01, 0.1, 1, 5, 10), degree=c(0.5,1,2,3,5)))
bestmod <- tune.out$best.model
pred_svmP_tbw <- predict(bestmod, test_set)
mse_svmP_tbw <- round(mse(pred_svmP_tbw, test_set$TBW), 2)
wyn_svmP <- round(sqrt(mse_svmP_tbw), 2)

# 7 SVM Kernel
tune.out <- tune(svm, TBW~., data=train_set, kernel = "radial", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 10, 100,1000), gamma=c(0.01, 0.1, 0.5, 1,2,3,10,30)),cross=4)
bestmod <- tune.out$best.model
pred_svmR_tbw <- predict(bestmod, test_set)
mse_svmR_tbw <- round(mse(pred_svmR_tbw, test_set$TBW), 2)
wyn_svmR <- round(sqrt(mse_svmR_tbw), 2)

                 