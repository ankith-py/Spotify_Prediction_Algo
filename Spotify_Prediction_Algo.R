#Load and clean data
dataspot<-read.csv('D:/Spotify_Songs_2000_to_2019.csv')
dataspot[,c(1,2,4,9,11,18)]<-lapply(dataspot[,c(1,2,4,9,11,18)],factor)
str(dataspot)

dataspot$genre[dataspot$genre == 'rock, Folk/Acoustic, easy listening']<-'pop, Dance/Electronic'
dataspot$genre[dataspot$genre == 'rock, pop, metal']<-'pop, rock, metal'
dataspot$genre[dataspot$genre == 'Folk/Acoustic, rock, pop']<-'Alternative'
dataspot$genre[dataspot$genre == 'hip hop, country']<-'hip hop, R&B'
dataspot$genre[dataspot$genre == 'Folk/Acoustic, pop']<-'pop'
dataspot$genre[dataspot$genre == 'hip hop, pop, R&B, latin']<-'hip hop'
dataspot$genre[dataspot$genre == 'hip hop, pop, R&B, Dance/Electronic']<-'R&B'
dataspot$genre[dataspot$genre == 'hip hop, R&B']<-'R&B'
dataspot$genre[dataspot$genre == 'rock, blues']<-'rock'
dataspot$genre[dataspot$genre == 'metal']<-'rock, metal'
dataspot$genre[dataspot$genre == 'pop, R&B, Dance/Electronic']<-'pop, Dance/Electronic'
dataspot$genre[dataspot$genre == 'rock, pop, Dance/Electronic']<-'Dance/Electronic'
dataspot$genre[dataspot$genre == 'pop, country']<-'country'
dataspot$genre[dataspot$genre == 'hip hop, pop, rock']<-'hip hop, pop'

dataspot2<-droplevels(dataspot)
str(dataspot2)

#Prepare the count column
ggg<-sort(table(dataspot2$artist),decreasing = TRUE)
counts<-data.frame(ggg) #create a new column that will work as a factor for the data frame
countsong2<-matrix(NA,nrow = 1916,ncol = 1)
for (i in 1:1916){
  sproww<-as.numeric(rownames(counts[counts$Var1 == dataspot2[i,1],]))
  countsong2[i,]<-counts[sproww,2]
}
dataspot2$songcount<-countsong2

dataspot2<-dataspot2[!dataspot2$year == 1998,]
dataspot2<-dataspot2[!dataspot2$year == 1999,]
dataspot2<-dataspot2[!dataspot2$year == 2020,]

#Changing milliseconds to seconds for simplicity
dataspot2$duration_ms<-dataspot2$duration_ms/1000
names(dataspot2)[names(dataspot2)=='duration_ms']<-'duration_sec'
str(dataspot2)

#Remove artist and song. Data is cleaned and prepared for analysis
dataspot2<-dataspot2[,3:19]


#-------------------------------------------------------------------------------


#Check correlation among categorical predictors for multicollinearity. Highly correlated variables
str(dataspot2)
chisq.test(dataspot2$explicit, dataspot2$key)[3]
chisq.test(dataspot2$explicit, dataspot2$mode)[3]
chisq.test(dataspot2$explicit, dataspot2$genre)[3]

chisq.test(dataspot2$key, dataspot2$mode)[3]
chisq.test(dataspot2$key, dataspot2$genre)[3]

chisq.test(dataspot2$mode, dataspot2$genre)[3]

#Correlation matrix to check for correlation among numerical predictors. Correlation not above or equal to
#0.7, so no multicollinearity and can individually check predictors against popularity
library(fields)
x <- dataspot2[,c(1,3,5,6,8,10:15,17)]
image.plot(cor(x), main = 'Correlation Matrix Heatmap', xlab = 'Predictors', ylab = 'Predictors')

#Analysis of each numerical predictor individually on popularity to determine common trends
plot(dataspot2$duration_sec, dataspot2$popularity)
plot(dataspot2$year, dataspot2$popularity)
plot(dataspot2$danceability, dataspot2$popularity)
plot(dataspot2$energy, dataspot2$popularity)
plot(dataspot2$loudness, dataspot2$popularity)
plot(dataspot2$speechiness, dataspot2$popularity)
plot(dataspot2$acousticness, dataspot2$popularity)
plot(dataspot2$instrumentalness, dataspot2$popularity)
plot(dataspot2$liveness, dataspot2$popularity)
plot(dataspot2$valence, dataspot2$popularity)
plot(dataspot2$tempo, dataspot2$popularity)
plot(dataspot2$songcount, dataspot2$popularity)


#-------------------------------------------------------------------------------


#Split into training and testing data for analysis
set.seed(11)
raindex<-sample(1:nrow(dataspot2),nrow(dataspot2),replace = F)
randomdata<-dataspot2[raindex,]
traindex<-sample(nrow(randomdata),1409)
train<-randomdata[traindex,]
test<-randomdata[-traindex,]


#GLM prediction
glmMod <- glm(popularity ~., data = train)
glmPred <- predict(glmMod, newdata = test)
(glm_MSE <- mean((test$popularity - glmPred)^2))


#Regression tree prediction
library(tree)
treeMod <- tree(popularity ~., data = train)

treePred = predict(treeMod, newdata = test)
(tree_MSE <- mean((test$popularity - treePred)^2))


# #Use CV for tree mod to find optimal size (lowest deviance) for pruning. Then predict on final, pruned tree
# cv.treeMod = cv.tree(treeMod)
# plot(cv.treeMod$size,cv.treeMod$dev,type='b',col = 'red',xlab = 'CV Tree Size',ylab = 'CV Tree Deviance')
# 
# prune.treeMod = prune.tree(treeMod, best = 1)
# prunePred <- predict(prune.treeMod, newdata = test)
# (prune_MSE <- mean((test$popularity - prunePred)^2))



#Random Forest with different mtry values
library(randomForest)
RF_MSE_mat <- matrix(NA, 16, 1)
for (m in 1:16) {
  print(m)
  fitRF = randomForest(popularity ~ ., data = train, mtry=m)
  predictRF = predict(fitRF, newdata=test)
  RF_MSE_mat[m,1] = mean((test$popularity - predictRF)^2)
  
}
plot(RF_MSE_mat, xlab ='# of Variables Selected for each Split', ylab = 'MSE', 
     main ='Random Forest', col = 'red')
lines(RF_MSE_mat, col = 'red')
which(RF_MSE_mat == min(RF_MSE_mat))
min(RF_MSE_mat)


best_fitRF = randomForest(popularity ~ ., data = train, mtry=which(RF_MSE_mat == min(RF_MSE_mat)))

## Variable importance plot
varImpPlot(best_fitRF, main="Variable importance measure", pch=15)


#-------------------------------------------------------------------------------


#Let's see why our MSE is large...there are many observations that have popularity 0, which affects
#he predictive performance of our models.
hist(dataspot2$popularity, xlab = 'Popularity', main = 'Distribution of Popularity', col = 'light blue')

sum(dataspot2$popularity <= 10)/nrow(dataspot2)
  
#Classification will have lower errors, so let's create a new outcome variable by splitting popularity into
#3 categories: low, medium, and high
dataspot3 <- dataspot2
dataspot3$pop_cat <- cut(dataspot3$popularity, breaks = c(0, 39, 74, 100), 
                            labels = c("low", "medium", "high"), include.lowest = T)

#Removing popularity continuous variable
dataspot3 <- dataspot3[,-4]
library(MASS)
library(ISLR)
library(e1071)


err_mat <- matrix(NA, 100, 5)
nSim <- 100
for (ni in 1:nSim) {
  print(ni)
  raindex<-sample(1:nrow(dataspot3),nrow(dataspot3),replace = F)
  randomdata<-dataspot3[raindex,]
  traindex<-sample(nrow(randomdata),1409)
  train3 <-randomdata[traindex,]
  test3 <-randomdata[-traindex,]
  
  #LDA
  modLDA = lda(pop_cat ~., data = train3)
  testPredLDA = as.numeric(predict(modLDA, newdata = test3)$class)
  (LDA_err <- mean(testPredLDA != (as.numeric(test3$pop_cat))))
  err_mat[ni,1] <- LDA_err
  
  #Ordinal Classification
  ordMod <- polr(pop_cat ~., data = train3)
  ordPred <- predict(ordMod, newdata = test3)
  (ord_err <- mean(ordPred != test3$pop_cat))
  err_mat[ni,2] <- ord_err
  
  #Tree Classification
  treeMod <- tree(pop_cat ~., data = train3)
  
  treePred <- predict(treeMod, newdata = test3)
  new_treePred <- matrix(NA, nrow(treePred), 1)
  for (i in 1:nrow(treePred)) {
    if (treePred[i,1] > treePred[i,2] & treePred[i,1] > treePred[i,3]) {
      new_treePred[i] <- 'low'
    }
    if (treePred[i,2] > treePred[i,1] & treePred[i,2] > treePred[i,3]) {
      new_treePred[i] <- 'medium'
    }
    if (treePred[i,3] > treePred[i,1] & treePred[i,3] > treePred[i,2]) {
      new_treePred[i] <- 'high'
    }
  }
  tree_err <- mean(new_treePred != test3$pop_cat)
  err_mat[ni,3] <- tree_err
  
  
  #Pruned Tree Classification
  cv.treeMod = cv.tree(treeMod)
  prune.treeMod = prune.tree(treeMod, best = cv.treeMod$size[which.min(cv.treeMod$dev)])
  prunePred <- predict(prune.treeMod, newdata = test3)
  new_prunePred <- matrix(NA, nrow(prunePred), 1)
  
  for (z in 1:nrow(treePred)) {
    if (prunePred[z,1] > prunePred[i,2] & prunePred[i,1] > prunePred[i,3]) {
      new_prunePred[z] <- 'low'
    }
    if (prunePred[z,2] > prunePred[i,1] & prunePred[i,2] > prunePred[i,3]) {
      new_prunePred[z] <- 'medium'
    }
    if (prunePred[z,3] > prunePred[i,1] & prunePred[i,3] > prunePred[i,2]) {
      new_prunePred[z] <- 'high'
    }
  }
  prune_err <- mean(new_prunePred != test3$pop_cat)
  err_mat[ni,4] <- prune_err
  
  #SVM
  oh_train <- model.matrix(pop_cat ~., data = train3)[,-1]
  x <- oh_train
  y <- train3$pop_cat
  xtest <- model.matrix(pop_cat ~., data = test3)[,-1]
  
  svm_mod <- svm(x, y, type = 'C-classification')
  svm_pred <- predict(svm_mod, newdata = xtest)
  svm_err <- mean(svm_pred != test3$pop_cat)
  err_mat[ni,5] <- svm_err
  
}
#Display boxplot
colnames(err_mat) <- c('LDA', 'Ordinal', 'Tree', 'Pruned Tree', 'SVM')
boxplot(err_mat, main = 'Multiclass Classification for 100 Simulations',xlab = 'Method', ylab = 'Error Rate', col = 'light blue')

#Code will take a while to run. Number of simulations can be decreased further to reduce runtime.
#Beep sound to signal that the script has finished running.
library(beepr)
beep(1)

