setwd("C:/Users/Monika/Dysk Google/IBwP/Dane dla Studentów")
dclas=read.csv("dane_clas.csv",header=TRUE,sep=";",dec = ",")
attach(dclas)

# teraz o b³êdach i ocenie jakoœci klasyfikacji

ocena<<-function(macierz.bledow,nazwa.klasyfikatora="brak nazwy klasyfikatora"){
  # macierz postaci |TP  FP |
  #                 | FN TN |
  # 
  tp=macierz.bledow[1,1]
  fp=macierz.bledow[1,2]
  fn=macierz.bledow[2,1]
  tn=macierz.bledow[2,2]
  
  acc=(tp+tn)/(tp+tn+fp+fn)
  sen=tp/(tp+fn)
  spe=tn/(tn+fp)
  pre=tp/(tp+fp)
  f1=2*pre*sen/(pre+sen)
  
  jakosc=c(acc,sen,spe,pre,f1)
  nazwy=c("dok³adnoœæ","czu³oœæ","specyficznoœæ","precyzja","F1")
  kol=c("slategray2","seashell2","lightpink2","wheat2",'cornflowerblue')
  while(names(dev.cur()) != "null device") dev.off()  
  png(paste(nazwa.klasyfikatora,".png",sep=""),width=1000,height=800)
  barplot(jakosc,col=kol,main=nazwa.klasyfikatora,names=nazwy,ylim=c(0,1))
  dev.off()
  jakosc.ramka=data.frame(acc,sen,spe,pre,f1)
  return(jakosc.ramka)
}

# klasyfikacja: odpowiednie.nawodnienie
library(caTools)
#ograniczamy ramkê tylko do tego, gdzie jest wartoœæ odpowiednie.nawodnienie

df.on=subset(dclas,!is.na(odpowiednie.nawodnienie))
df.on$odpowiednie.nawodnienie=factor(df.on$odpowiednie.nawodnienie)
summary(df.on)

split=sample.split(odpowiednie.nawodnienie,SplitRatio = 0.8)

train_set=subset(df.on,split==TRUE)
test_set=subset(df.on,split==FALSE)

# TRAIN-TEST SPLIT 

trainX = train_set[,-ncol(df.on)] # 88 - odpowiednie.nawodnienie
testX=test_set[,-ncol(df.on)]
trainY=train_set[,ncol(df.on)]
testY = test_set[,ncol(df.on)]
k=5 # ustawiane samodzielnie

# najpierw standaryzacja danych wejœciowych
trainX_s = scale(trainX)
testX_s = scale(testX)

############## KNN #########################
library(class)
knn.pred = knn(trainX_s,testX_s,trainY, k=k)
# nie pojdzie, bo...
complete.cases(df.on)
# missing data :(
summary(df.on)
names(df.on)

do.usun=c(NULL)
for (i in 1:(ncol(df.on)))
{
  if(length(which(is.na(df.on[,i])))>100)
  {
    do.usun=c(do.usun,i)
  }
}
do.usun
df.on=df.on[,-do.usun]
complete.cases(df.on)

df.on=df.on[complete.cases(df.on),]

split=sample.split(df.on$odpowiednie.nawodnienie,SplitRatio = 0.8)

train_set=subset(df.on,split==TRUE)
test_set=subset(df.on,split==FALSE)

trainX = train_set[,-ncol(df.on)] # 88 - odpowiednie.nawodnienie
testX=test_set[,-ncol(df.on)]
trainY=train_set[,ncol(df.on)]
testY = test_set[,ncol(df.on)]
trainX_s = scale(trainX)
testX_s = scale(testX)
knn.pred = knn(trainX_s,testX_s,trainY, k=k)

wyn=table(knn.pred,testY)




wyn.knn=ocena(macierz.bledow=wyn,nazwa.klasyfikatora = "KNN")
wyn.knn

####### Simple Decision Trees ###################
library(rpart)
library(rpart.plot)
# Run Classification tree model on train set
classtree <- rpart(formula = odpowiednie.nawodnienie~., data = train_set, method = "class", control = rpart.control(maxdepth = 3)) # 5, 9

# Plot the decision Tree
rpart.plot(classtree, box.palette = "RdBu", digit = -2)

# Predict value at any point
dd.pred <- predict(classtree, test_set, type = "class")

wyn=table(test_set$odpowiednie.nawodnienie, dd.pred)

wyn.dd=ocena(macierz.bledow=wyn,nazwa.klasyfikatora = "Drzewa decyzyjne depth 3") 
wyn.dd


########### Ensemble Method 1 - Bagging #######################

# Bagging - parcianka, p³ótno workowe 

# in bagging we have all the predictors, only different training sets
# Bagging (Bootstrap Aggregating)

library(randomForest)

bagging = randomForest(formula = odpowiednie.nawodnienie~., data = train_set, mtry = 22,method = "class")
# mtry - ile zmiennych, gdyby bylo mniej niz 22 to byloby random forest, ale dajemy wszystkie mozliwe predyktory, czyli jest bagging
# bo bagging ma wszystkie predyktory, tylko inne zbiory uczace
# random forest ma inne zbiory uczace i ograniczona liczbe predyktorow

# jak robimy bagging, to nie da sie zrobic wykresu drzewa
# jest poprawa dokladnosci i klasyfikacji
# placi sie za to mniejsza mozliwoscia interpretacji

bag.pred <- predict(bagging,test_set)

wyn=table(test_set$odpowiednie.nawodnienie, bag.pred)
wyn.bag=ocena(macierz.bledow=wyn,nazwa.klasyfikatora = "Bagging") 

########### Ensemble Method 2 - Random Forest #######################

randomfor = randomForest(formula = odpowiednie.nawodnienie~., data = train_set, ntree = 100,method = "class")
ranfor.pred <- predict(randomfor,test_set)
wyn=table(test_set$odpowiednie.nawodnienie, ranfor.pred)
wyn.ranfor=ocena(macierz.bledow=wyn,nazwa.klasyfikatora = "Random forest") 


# ############# Ensemble Method 3 - Boosting ########################
# 
# in Boosting trees are grown sequentially
# each tree is grown using information from previously grown tree

# Ada Boost 
# po pierwszym zbudowaniu drzewa wyszukuje sie zle sklasyfikowane przypadki albo z duzymi rezyduami
# tym przypadkom nadaje sie wieksza wage i trenuje ponownie drzewo i tak N-1 razy

library(adabag)
adaboost <-boosting(odpowiednie.nawodnienie~., data=train_set, boos = TRUE, mfinal = 100) # mfinal = 100 by default, czyli ile drzew

ada.pred <- predict(adaboost,test_set)
wyn=table(test_set$odpowiednie.nawodnienie,ada.pred$class)
wyn.ada=ocena(macierz.bledow=wyn,nazwa.klasyfikatora = "ADA boost") 

t1 <- adaboost$trees[[1]]
plot(t1)
text(t1, pretty = 100)

############################# SVM ####################

# !! SVM is scale-sensitive, the variables MUST be scaled (m=0, s=1)

library(e1071)

# funkcja to svm z biblioteki e1071
# svm dziala dla klasyfikacji i regresji, to zalezy tylko od tego, czy zmienna zalezna jest numeryczna, czy faktorowa

svmfit = svm(odpowiednie.nawodnienie~., data = train_set, kernel = "linear", cost = 1, scale = TRUE)
# kernel = "linear' odpowiada Support Vector Classification
summary(svmfit)

## Predicting on test set
svm.lin.pred = predict(svmfit, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.lin.pred)
wyn.svm.lin=ocena(macierz.bledow = wyn,nazwa.klasyfikatora = "SVM Linear cost 1")

## To check the support vectors
svmfit$index

## Finding the best value of C / Tuning the hyperparameter
tune.out = tune(svm, odpowiednie.nawodnienie~., data= train_set, kernel = "linear", ranges = list(cost=c(0.001,0.01,0.1,1,10,100)))
# tuning is performed using 10-fold cross validation
# mozna dodac parametr cross = ?? i podac inna wartosc

bestmod = tune.out$best.model
summary(bestmod)

svm.lin.pred.100 = predict(bestmod, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.lin.pred.100)
wyn.svm.lin.100=ocena(wyn,"SVM Linear cost 100")


## Polynomial Kernel

svmfitP = svm(odpowiednie.nawodnienie~., data = train_set, kernel = "polynomial", cost = 1, degree = 2)
summary(svmfitP)

svm.pol.pred.1 = predict(svmfitP, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.pol.pred.1)
wyn.svm.pol.1=ocena(wyn,"SVM Polynomial cost 1")


# Hyperparameter Tuning

tune.outP = tune(svm, odpowiednie.nawodnienie~., data = train_set, cross = 4, kernel = "polynomial", ranges = list(cost = c(0.01,0.1,1,5,10),degree = c(0.5,1,2,3,5)))
# cross zmniejszony z 10 do 4, bo i tak liczymy 25 razy i to wszystko razy 4 zamiast razy 10
bestmodP = tune.outP$best.model
summary(bestmodP)
svm.pol.pred.10 = predict(bestmodP, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.pol.pred.10)
wyn.svm.pol.10=ocena(wyn,"SVM Polynomial cost 10")


## Radial Kernel

svmfitR = svm(odpowiednie.nawodnienie~., data = train_set, kernel = "radial", gamma = 1, cost = 1)
summary(svmfitR)
svm.rad.pred.1 = predict(svmfitR, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.rad.pred.1)
wyn.svm.rad.1=ocena(wyn,"SVM Radial cost 1")

tune.outR = tune(svm, odpowiednie.nawodnienie~., data = train_set, kernel = "radial",
                 ranges = list(cost = c(0.001,0.01,0.1,1,10,100,1000),gamma = c(0.01,0.1,0.5,1,2,3,10,50), cross=4))
summary(tune.outR)
bestmodR = tune.outR$best.model
summary(bestmodR)
svm.rad.pred.10 = predict(bestmodR, test_set)
wyn=table(test_set$odpowiednie.nawodnienie, svm.rad.pred.10)
wyn.svm.rad.10=ocena(wyn,"SVM Radial cost 10")


