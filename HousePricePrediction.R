library(Amelia)
library(corrplot)
library(caret)
library(plotly)
library(dplyr)
library(ggplot2)
library(GGally)
library(purrr)
library(tidyr)
library(pastecs)
library("psych")
require(openxlsx)
library(rpart)
library(rpart.plot)
library(factoextra)
require(pls)
library(randomForest)



house <- read.csv("HousingValuation.csv")
#separating numerical and categorical variables.
house_numerical <- house[,c(4,5,16,17,18,21,24,25,26,27,28,29,30,32,33,34,36,38,39,41,42,44)]
drop <- names(house_numerical)
house_cate <- house[,!(names(house) %in% drop)]
#separating continuous variables from numerical once.
house_continous <- house_numerical[,c(1,2,6,7,8,9,10,19,22)]

#Removing ID from categorical variables.
house_cate <- house_cate[,-1]


#Transforming Ordinal columns.
house_final <- house_numerical
house_final$LotShape <- factor(house_cate$LotShape, levels=c("Reg","IR1", "IR2", "IR3"), labels=c(4,3,2,1))
house_final$LotShape <- as.numeric(as.character(house_final$LotShape))

house_final$HeatingQC <- factor(house_cate$HeatingQC, levels = c("Ex", "Gd", "TA", "Fa", "Po"), labels = c(5,4,3,2,1))
house_final$HeatingQC <- as.numeric(as.character(house_final$HeatingQC))

house_final$KitchenQuality <- factor(house_cate$KitchenQuality, levels = c("Ex", "Gd", "TA", "Fa", "Po"), labels = c(5,4,3,2,1))
house_final$KitchenQuality <- as.numeric(as.character(house_final$KitchenQuality))


#Transforming Nominal columns.
house_final$DwellSubClass20 <- as.numeric(house_cate$DwellSubClass=="20")
house_final$DwellSubClass60 <- as.numeric(house_cate$DwellSubClass=="60")
house_final$DwellSubClass80 <- as.numeric(house_cate$DwellSubClass=="80")
house_final$DwellSubClass90 <- as.numeric(house_cate$DwellSubClass=="90")

house_final$LotConfigCorner <- as.numeric(house_cate$LotConfig=="Corner")
house_final$LotConfigCulDSac <- as.numeric(house_cate$LotConfig=="CulDSac")
house_final$LotConfigFR2 <- as.numeric(house_cate$LotConfig=="FR2")
house_final$LotConfigFR3 <- as.numeric(house_cate$LotConfig=="FR3")
house_final$LotConfigInside <- as.numeric(house_cate$LotConfig=="Inside")

house_cate$GarageType[is.na(house_cate$GarageType)] <- "NoGarage"
house_final$GarageNA <- as.numeric(house_cate$GarageType=="NoGarage")
house_final$GarageDetchd <- as.numeric(house_cate$GarageType=="Detchd")
house_final$GarageAttchd <- as.numeric(house_cate$GarageType=="Attchd")



#summary statistics for every continuous variables.
summary(house_continous)


#sd of continuous variables.
sapply(house_continous, function(x) sd(x,na.rm = TRUE))


#Count of every categorical variables.
lapply(house_cate, table)
house_continous %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot()
table(house_continous$LowQualFinSF)
house_final <- house_final[,-9]


#histogram for each continuous variable.
house_continous %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
hist(log(house_continous$Frontage))
hist(log(house_continous$SalePrice))
hist(log(house_continous$LivingArea))
house_final$Frontage <- log(house_final$Frontage)
house_final$SalePrice <- log(house_final$SalePrice)
house_final$LivingArea <- log(house_final$LivingArea)



#Replace NAs with mean.
house_final_mean <- house_final
house_final_mean$Frontage[is.na(house_final_mean$Frontage)] <- mean(house_final_mean$Frontage, na.rm = TRUE)
mean(house_final_mean$Frontage, na.rm = T)
mean(house_final$Frontage,na.rm = T)
plot(density(house_final_mean$Frontage), main="frontage original and transformed") 
lines(density(house_final$Frontage, na.rm = TRUE), col="blue")

house_final$Frontage <- house_final_mean$Frontage
write.xlsx(house_final, file = "alldata.xlsx")


#Removing highly correlated variables.
house_final_target_remove <- subset(house_final, select = -c(SalePrice))
M <- data.matrix(house_final_target_remove)
corrM <- cor(M)
highlyCorrM <- findCorrelation(corrM, cutoff=0.5)
name <- names(house_final_target_remove)[highlyCorrM]
house_final_target_remove <- house_final_target_remove[,!(names(house_final_target_remove) %in% name)]
house_final_target_remove$SalePrice <- house_final$SalePrice
house_final <- house_final_target_remove

#ploting scatter plot of saleprice vs all other variables.
house_final %>%
  keep(is.numeric) %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x=value, y=SalePrice)) +
  geom_point() +
  facet_wrap(.~ var, scales = "free")

write.xlsx(house_final, file = "finaldata.xlsx")

house <- read.xlsx("finaldata.xlsx")
house_all <- read.xlsx("alldata.xlsx")



################ Model - 1 #################

#Data Partition
smp_size <- floor(2/3 * nrow(house))
set.seed(2) #to Reproduce randomaly produced sample result
house <- house[sample(nrow(house)),]
house.train1 <- house[1:smp_size,]
house.test1 <- house[(smp_size+1):nrow(house),]


#Predictive Model
formula = SalePrice ~.
model1 <- lm(formula = formula, data = house.train1)

as.formula(
  paste0("y ~ ", round(coefficients(model1)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(model1)[-1],
                       names(coefficients(model1)[-1])),
               collapse = " + ")
  )
)
summary(model1)$coefficient

#Evaluation of predictive model
house.train1$predicted.SalePrice <- predict(model1, house.train1)
house.test1$predicted.SalePrice <- predict(model1, house.test1)
pl1 <- house.test1 %>%
  ggplot(aes(SalePrice,predicted.SalePrice)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(color='red')) +
  xlab('Actual saleprice') +
  ylab('Predicted saleprice') +
  theme_bw()
ggplotly(pl1)


#R-Squared
summary(model1)$r.squared
summary(model1)$adj.r.squared

#RMSE
error1 <- exp(house.test1$SalePrice)- exp(house.test1$predicted.SalePrice)
rmse1 <- sqrt(mean(error1^2))
print(rmse1)


##################################
###############2-Model############
##################################

#Automatic Feature Selection
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(house_all[,-c(21)], house_all[,21],sizes = c(1:8), rfeControl = control)
print(results)

house_rfe <- house_all[,c("LivingArea","OverallQuality","OverallCondition","TotalBSF","LotArea","SalePrice")]
target <- house_rfe[,c("SalePrice")]
house_rfe <- house_rfe[,-6]
dev.off()
ggcorr(house_rfe, label = TRUE)
house_rfe$SalePrice <- target
#Data Partition
smp_size <- floor(2/3 * nrow(house_rfe))
set.seed(3) #to Reproduce randomaly produced sample result
house_rfe <- house_rfe[sample(nrow(house_rfe)),]
house_rfe.train <- house_rfe[1:smp_size,]
house_rfe.test <- house_rfe[(smp_size+1):nrow(house_rfe),]


#Predictive Model
formula = SalePrice ~.
model2 <- lm(formula = formula, data = house_rfe.train)
summary(model2)$coefficient

#Evaluation of predictive model
house_rfe.train$predicted.SalePrice <- predict(model2, house_rfe.train)
house_rfe.test$predicted.Saleprice <- predict(model2, house_rfe.test)
as.formula(
  paste0("SalePrice ~ ", round(coefficients(model2)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(model2)[-1],
                       names(coefficients(model2)[-1])),
               collapse = " + ")
  )
)
pl1 <- house_rfe.test %>%
  ggplot(aes(SalePrice,predicted.Saleprice)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(color='red')) +
  xlab('Actual saleprice') +
  ylab('Predicted saleprice') +
  theme_bw()
ggplotly(pl1)

#R-Squared
summary(model2)$r.squared
summary(model2)$adj.r.squared

#RMSE
error2 <- exp(house_rfe.test[,6])-exp(house_rfe.test[,7])
rmse2 <- sqrt(mean(error2^2))
print(rmse2)




######## 3rd model #############
library(leaps)

regfit_fwd = regsubsets(SalePrice~., data = house_all, nvmax = 19, method = "forward")
summary(regfit_fwd)
coef(regfit_fwd,10)
house_forward <- house_all[,c("SalePrice","OverallQuality","OverallCondition","YearBuilt","TotalBSF","LivingArea","Fireplaces","GarageCars","Frontage","KitchenQuality","LotArea")]
target <- house_forward[,c("SalePrice")]
house_forward <- house_forward[,-1]
dev.off()
ggcorr(house_forward, label = TRUE)
house_forward$SalePrice <- target
smp_size <- floor(2/3 * nrow(house_forward))
set.seed(400) #to Reproduce randomaly produced sample result
house_forward <- house_forward[sample(nrow(house_forward)),]
house_forward.train <- house_forward[1:smp_size,]
house_forward.test <- house_forward[(smp_size+1):nrow(house_forward),]
formula = SalePrice ~.
model3 <- lm(formula = formula, data = house_forward.train)
summary(model3)$coefficient

#Evaluation of predictive model
house_forward.train$predicted.SalePrice <- predict(model3, house_forward.train)
house_forward.test$predicted.Saleprice <- predict(model3, house_forward.test)

as.formula(
  paste0("SalePrice ~ ", round(coefficients(model3)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(model3)[-1],
                       names(coefficients(model3)[-1])),
               collapse = " + ")
  )
)

summary(model3)$r.squared
summary(model3)$adj.r.squared
residual <- house_forward.test[,1]-house_forward.test[,12]
house_forward.test$residual <- residual
error3 <- exp(house_forward.test[,1])-exp(house_forward.test[,12])
rmse3 <- sqrt(mean(error3^2))
rmse3
pl1 <- house_forward.test %>%
  ggplot(aes(SalePrice,predicted.Saleprice)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(color='red')) +
  xlab('Actual saleprice') +
  ylab('Predicted saleprice') +
  theme_bw()
ggplotly(pl1)

ggplot(house_forward.test, aes(x=predicted.Saleprice, y=residual)) +
  geom_point() +
  geom_hline(yintercept = 0, col="blue")

rmse_final <- sqrt(mean(residual^2))
rmse_final

###################################
##########               ##########
########## Decision tree ##########
##########               ##########
###################################


######################################
############# model - 1 ##############
######################################

smp_size <- floor(2/3 * nrow(house_all))
set.seed(20)

house_all_selected <- house_all[sample(nrow(house_all)),]
house.train <- house_all_selected[1:smp_size,]
house.test <- house_all_selected[(smp_size+1):nrow(house_all_selected),]

formula = SalePrice ~.
dtree <- rpart(formula, data = house.train, method = "anova")
dtree$variable.importance
rpart.plot(dtree, type = 4, fallen.leaves = FALSE)

#Predictions and Assessment
house.test$predicted.SalePrice <- predict(dtree, house.test)

error4 <- house.test$SalePrice - house.test$predicted.SalePrice
rmse4 <- sqrt(mean(error4^2))
print(paste("Root mean square error is: ", rmse4))


#Fine Tuning the model
printcp(dtree)
#Below command to easily find best CP value
dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]
prunned_tree <- prune(dtree, cp = 0.01)
rpart.plot(prunned_tree, type = 4, fallen.leaves = FALSE)
house.test$predicted.Saleprice <- predict(prunned_tree, house.test)
error4_new <- house.test$SalePrice - house.test$predicted.SalePrice
rmse4_new <- sqrt(mean(error4_new^2))
rmse4_new
#rmse does not changed after the prunning so we conclude that initial tree was not overfitted.


#Random Forest
rf <- randomForest(formula = formula, data = house.train)
print(rf)
house.test$predicted_rf.SalePrice <- predict(rf,house.test)

error_rf <- house.test$SalePrice - house.test$predicted_rf.SalePrice
rmse_rf <- sqrt(mean(error_rf^2))
rmse_rf
plot(rf)

##################################################
############# Gini Index ##############
##################################################

set.seed(3333)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit_gini <- train(SalePrice ~., data = house.train, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini



##################################################
########## Information Gain###########
##################################################

set.seed(3333)
dtree_fit <- train(SalePrice ~., data = house.train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit



#################################################
############ Boosting ###########################
#################################################

require(gbm)
boost.dtree = gbm(SalePrice~., data = house.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 8)
summary(boost.dtree)
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.dtree, newdata = house.test, n.trees = n.trees)
dim(predmat)
error <- house.test$SalePrice - predmat
rmse_boost <- sqrt(mean(error^2))
rmse_boost
house.test$predmat <- predmat


######################################
############# model - 2 ##############
######################################

house_rfe <- house_all[,c("LivingArea","OverallQuality","OverallCondition","TotalBSF","LotArea","SalePrice")]
smp_size <- floor(2/3 * nrow(house_rfe))
set.seed(3) #to Reproduce randomaly produced sample result
house_rfe <- house_rfe[sample(nrow(house_rfe)),]
house_rfe.train <- house_rfe[1:smp_size,]
house_rfe.test <- house_rfe[(smp_size+1):nrow(house_rfe),]
formula = SalePrice ~.
dtree <- rpart(formula, data = house_rfe.train, method = "anova")
dtree$variable.importance
rpart.plot(dtree, type = 4, fallen.leaves = FALSE)


#Predictions and Assessment
house_rfe.test$predicted.Saleprice <- predict(dtree, house_rfe.test)

error4 <- house_rfe.test$SalePrice - house_rfe.test$predicted.Saleprice
rmse4 <- sqrt(mean(error4^2))
print(paste("Root mean square error is: ", rmse4))


#Fine Tuning the model
printcp(dtree)
#Below command to easily find best CP value
dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]
prunned_tree <- prune(dtree, cp = 0.01)
rpart.plot(prunned_tree, type = 4, fallen.leaves = FALSE)
house_rfe.test$predicted_prunned.Saleprice <- predict(prunned_tree, house_rfe.test)
error4_new <- house_rfe.test$SalePrice - house_rfe.test$predicted_prunned.Saleprice
rmse4_new <- sqrt(mean(error4_new^2))
rmse4_new
#rmse does not changed after the prunning so we conclude that initial tree was not overfitted.


#Random Forest
rf <- randomForest(formula = formula, data = house_rfe.train)
print(rf)
house_rfe.test$predicted_rf.SalePrice <- predict(rf,house_rfe.test)

error_rf <- house_rfe.test$SalePrice - house_rfe.test$predicted_rf.SalePrice
rmse_rf <- sqrt(mean(error_rf^2))
rmse_rf
plot(rf)

##################################################
############# Gini Index ##############
##################################################

set.seed(333)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit_gini <- train(SalePrice ~., data = house_rfe.train, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini



##################################################
########## Information Gain###########
##################################################

set.seed(333)
dtree_fit <- train(SalePrice ~., data = house_rfe.train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit



#################################################
############ Boosting ###########################
#################################################


require(gbm)
boost.dtree = gbm(SalePrice~., data = house_rfe.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 8)
summary(boost.dtree)
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.dtree, newdata = house_rfe.test, n.trees = n.trees)
dim(predmat)
error <- house_rfe.test$SalePrice - predmat
rmse_boost <- sqrt(mean(error^2))
rmse_boost
house_rfe.test$predmat <- predmat

######################################
############# model - 3 ##############
######################################
house_forward <- house_all[,c("SalePrice","OverallQuality","OverallCondition","YearBuilt","TotalBSF","LivingArea","Fireplaces","GarageCars","Frontage","KitchenQuality","LotArea")]
smp_size <- floor(2/3 * nrow(house_forward))
set.seed(400) #to Reproduce randomaly produced sample result
house_forward <- house_forward[sample(nrow(house_forward)),]
house_forward.train <- house_forward[1:smp_size,]
house_forward.test <- house_forward[(smp_size+1):nrow(house_forward),]
formula = SalePrice ~.
dtree <- rpart(formula, data = house_forward.train, method = "anova")
dtree$variable.importance
rpart.plot(dtree, type = 4, fallen.leaves = FALSE)


#Predictions and Assessment
house_forward.test$predicted.Saleprice <- predict(dtree, house_forward.test)

error4 <- house_forward.test$SalePrice - house_forward.test$predicted.Saleprice
rmse4 <- sqrt(mean(error4^2))
print(paste("Root mean square error is: ", rmse4))


#Fine Tuning the model
printcp(dtree)
#Below command to easily find best CP value
dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]
prunned_tree <- prune(dtree, cp = 0.01)
rpart.plot(prunned_tree, type = 4, fallen.leaves = FALSE)
house_forward.test$predicted_prunned.Saleprice <- predict(prunned_tree, house_forward.test)
error4_new <- house_forward.test$SalePrice - house_forward.test$predicted_prunned.Saleprice
rmse4_new <- sqrt(mean(error4_new^2))
rmse4_new
#rmse does not changed after the prunning so we conclude that initial tree was not overfitted.


#Random Forest
rf <- randomForest(formula = formula, data = house_forward.train)
print(rf)
house_forward.test$predicted_rf.SalePrice <- predict(rf,house_forward.test)

error_rf <- exp(house_forward.test$SalePrice) - exp(house_forward.test$predicted_rf.SalePrice)
rmse_rf <- sqrt(mean(error_rf^2))
rmse_rf
plot(rf)

##################################################
############# Gini Index ##############
##################################################

set.seed(333)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit_gini <- train(SalePrice ~., data = house_forward.train, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini



##################################################
########## Information Gain###########
##################################################

set.seed(333)
dtree_fit <- train(SalePrice ~., data = house_forward.train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit



#################################################
############ Boosting ###########################
#################################################


require(gbm)
boost.dtree = gbm(SalePrice~., data = house_forward.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 8)
summary(boost.dtree)
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.dtree, newdata = house_forward.test, n.trees = n.trees)
dim(predmat)
error <- house_forward.test$SalePrice - predmat
rmse_boost <- sqrt(mean(error^2))
rmse_boost