# Installing Packages important for this " Advanced regression Techniques "
install.packages("corrplot")
install.packages("boot")
install.packages("vars")
install.packages("QuantPsyc")
install.packages("lmtest")
install.packages("nortest")
install.packages("ROCR")
install.packages("pROC")
install.packages("car")
install.packages("sandwich")
install.packages("MASS")
install.packages("caTools")
install.packages("dplyr")

library(corrplot)                    # For visualizing correlation matrices and confidence intervals
library(boot)                        # Provides extensive facilities for bootstrapping and related resampling methods
library(vars)                        # Modelling Estimation, lag selection, diagnostic testing, forecasting etc
library(QuantPsyc)                   # Quantitative Psychology Tools which Contains functions useful for data screening, testing moderation, mediation and estimating power
library(lmtest)                      # Testing Linear Regression Models
library(nortest)                     # Tests for Normality
library(ROCR)                        # ROCR is a flexible tool for creating cutoff-parameterized 2D performance curves by freely combining two from over 25 performance measures.
library(pROC)                        # It proposes multiple statistical tests to compare ROC curves
library(car)                         # Companion to Applied Regression
library(sandwich)                    # Covariance Matrix Estimators
library(MASS)                        # Support Functions and Datasets for Venables
library(caTools)                     # Moving window statistics, GIF, Base64, ROC AUC, etc. Contains several basic utility functions including: moving (rolling, running) window statistic functions, read/write for GIF and ENVI binary files, fast calculation of AUC, LogitBoost classifier etc.
library(dplyr)                       # For data manipulation

# Set Path

setwd("C:\\Users\\Priyanshi\\Desktop\\Priyanshi\\Projects\\House Price")
getwd()

# Read Train File 
read.csv("train.csv", header = TRUE)->Train_House
House_Data <- Train_House
View(House_Data)
str(House_Data)

## feature Engineering including feature selection
## separating continuous and categorical column
## using corrplot to find the correlation between dependent and independent variable


# Finding N/A Values & Removing it by their mean values

data.frame(colSums(is.na(House_Data)))

# Columns having N/A values are - LotFrontage , Alley , BsmtQual , BsmtCond , BsmtExposure , BsmtFinType1 , BsmtFinType2
# Electrical, MasVnrType ,MasVnrArea , FireplaceQu , Garagetype ,GarageYrBlt , GarageFinish , GarageQual , GarageCond , PoolQC , Fence , MiscFeature.

levels(House_Data$LotFrontage) <- c(levels(House_Data$LotFrontage), "No-LotFrontage-Access")
House_Data$LotFrontage[is.na(House_Data$LotFrontage)]<-"No-LotFrontage-Access"

levels(House_Data$Alley) <- c(levels(House_Data$Alley), "No-Alley-Access")
House_Data$Alley[is.na(House_Data$Alley)]<-"No-Alley-Access"

levels(House_Data$BsmtQual) <- c(levels(House_Data$BsmtQual), "No-BsmtQual-Access")
House_Data$BsmtQual[is.na(House_Data$BsmtQual)]<-"No-BsmtQual-Access"

levels(House_Data$BsmtCond) <- c(levels(House_Data$BsmtCond), "No-BsmtCond-Access")
House_Data$BsmtCond[is.na(House_Data$BsmtCond)]<-"No-BsmtCond-Access"

levels(House_Data$BsmtExposure) <- c(levels(House_Data$BsmtExposure), "No-BsmtExposure-Access")
House_Data$BsmtExposure[is.na(House_Data$BsmtExposure)]<-"No-BsmtExposure-Access"

levels(House_Data$BsmtFinType1) <- c(levels(House_Data$BsmtFinType1), "No-BsmtFinType1-Access")
House_Data$BsmtFinType1[is.na(House_Data$BsmtFinType1)]<-"No-BsmtFinType1-Access"

levels(House_Data$BsmtFinType2) <- c(levels(House_Data$BsmtFinType2), "No-BsmtFinType2-Access")
House_Data$BsmtFinType2[is.na(House_Data$BsmtFinType2)]<-"No-BsmtFinType2-Access"

levels(House_Data$Electrical) <- c(levels(House_Data$Electrical), "No-Electrical-Access")
House_Data$Electrical[is.na(House_Data$Electrical)]<-"No-Electrical-Access"

levels(House_Data$MasVnrType) <- c(levels(House_Data$MasVnrType), "No-MasVnrType-Access")
House_Data$MasVnrType[is.na(House_Data$MasVnrType)]<-"No-MasVnrType-Access"

levels(House_Data$MasVnrArea) <- c(levels(House_Data$MasVnrArea), "No-MasVnrArea-Access")
House_Data$MasVnrArea[is.na(House_Data$MasVnrArea)]<-"No-MasVnrArea-Access"

levels(House_Data$FireplaceQu) <- c(levels(House_Data$FireplaceQu), "No-FireplaceQu-Access")
House_Data$FireplaceQu[is.na(House_Data$FireplaceQu)]<-"No-FireplaceQu-Access"

levels(House_Data$GarageType) <- c(levels(House_Data$GarageType), "No-GarageType-Access")
House_Data$GarageType[is.na(House_Data$GarageType)]<-"No-GarageType-Access"

levels(House_Data$GarageYrBlt) <- c(levels(House_Data$GarageYrBlt), "No-GarageYrBlt-Access")
House_Data$GarageYrBlt[is.na(House_Data$GarageYrBlt)]<-"No-GarageYrBlt-Access"

levels(House_Data$GarageFinish) <- c(levels(House_Data$GarageFinish), "No-GarageFinish-Access")
House_Data$GarageFinish[is.na(House_Data$GarageFinish)]<-"No-GarageFinish-Access"

levels(House_Data$GarageQual) <- c(levels(House_Data$GarageQual), "No-GarageQual-Access")
House_Data$GarageQual[is.na(House_Data$GarageQual)]<-"No-GarageQual-Access"

levels(House_Data$GarageCond) <- c(levels(House_Data$GarageCond), "No-GarageCond-Access")
House_Data$GarageCond[is.na(House_Data$GarageCond)]<-"No-GarageCond-Access"

levels(House_Data$PoolQC) <- c(levels(House_Data$PoolQC), "No-PoolQC-Access")
House_Data$PoolQC[is.na(House_Data$PoolQC)]<-"No-PoolQC-Access"

levels(House_Data$Fence) <- c(levels(House_Data$Fence), "No-Fence-Access")
House_Data$Fence[is.na(House_Data$Fence)]<-"No-Fence-Access"

levels(House_Data$MiscFeature) <- c(levels(House_Data$MiscFeature), "No-MiscFeature-Access")
House_Data$MiscFeature[is.na(House_Data$MiscFeature)]<-"No-MiscFeature-Access"

data.frame(colSums(is.na(House_Data)))
View(House_Data)

# Finding Independent Variable in the Dataset
hist(House_Data$SalePrice)
hist(House_Data$LotArea)

# SalePrice Variable is an Independent Variable.
max(House_Data$SalePrice) ; min(House_Data$SalePrice)

# Checking Quantiles

quantile(House_Data$SalePrice , c(0,0.05,0.1, 0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1.0))
House_Data_1 <- House_Data[House_Data$SalePrice < 450000 , ]
nrow(House_Data)
nrow(House_Data)-nrow(House_Data_1)

quantile(House_Data_1$SalePrice , c(0,0.05,0.1, 0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,1.0))
House_Data_2 <- House_Data_1[House_Data_1$SalePrice < 320000 , ]
nrow(House_Data_1)-nrow(House_Data_2)

quantile(House_Data_2$SalePrice , c(0,0.05,0.1, 0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0))
nrow(House_Data_2)

# Here a simple question arises that " Why do we check Quantiles ? " 
# In addition to the mean and variation, you also can take a look at the quantiles in R.
# A quantile, or percentile, tells you how much of your data lies below a certain value.

# House_Data_2 is Final_Model
House_Data_2 -> Final_House_Data
dim(Final_House_Data)
View(Final_House_Data)

# Removing those unnecessary columns variables which aren't affecting the dataset.

Final_House_Data <- Final_House_Data[c(-1)]
Final_House_Data <- Final_House_Data[c(-5:-16)]
Final_House_Data <- Final_House_Data[c(-8,-9)]
Final_House_Data <- Final_House_Data[c(-9 : -11)]
Final_House_Data <- Final_House_Data[c(-41 : -47)]
Final_House_Data <- Final_House_Data[c(-30 : -41)]
Final_House_Data <- Final_House_Data[c(-13 : -21)]
Final_House_Data <- Final_House_Data[c(-13 : -20)]
View(Final_House_Data)

Final_Data <- Final_House_Data

# Fitted Models

House_Model_1 <- lm(SalePrice ~ . , data = Final_Data)
summary(House_Model_1)

House_Model_2 <- lm(SalePrice ~ MSSubClass + MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + MasVnrArea + ExterQual + ExterCond +Foundation + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold  + YrSold  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_2)

House_Model_3 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + MasVnrArea + ExterQual + ExterCond +Foundation + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold  + YrSold  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_3)

House_Model_4 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + ExterQual + ExterCond +Foundation + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold  + YrSold  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_4)

House_Model_5 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold  + YrSold  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_5)

House_Model_6 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch + MoSold  + YrSold  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_6)

House_Model_7 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch  + SaleType  + SaleCondition , data = Final_Data)
summary(House_Model_7)

House_Model_8 <- lm(SalePrice ~ MSZoning + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch + I(SaleCondition == "Normal") , data = Final_Data)
summary(House_Model_8)

House_Model_9 <- lm(SalePrice ~ I(MSZoning == "FV") + I(MSZoning == "RL") + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch , data = Final_Data)
summary(House_Model_9)

House_Model_10 <- lm(SalePrice ~ I(MSZoning == "RL") + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch , data = Final_Data)
summary(House_Model_10)

House_Model_11 <- lm(SalePrice ~ I(MSZoning == "RL") + LotArea  + OverallQual + OverallCond + YearBuilt + RoofMatl + I(ExterQual == "TA") + WoodDeckSF + OpenPorchSF + ScreenPorch , data = Final_Data)
summary(House_Model_11)
vif(House_Model_11)

# Our Best Fitted Model is " House_Model_11 " 


fitted(House_Model_11)-> X
View(X)
View(Final_Data$SalePrice)
par(mfrow = c(2,2))
plot(House_Model_11)
pred<-predict(House_Model_11, type = "response")
write.csv(pred, "Train Prediction.csv")

attach(Final_Data)
Final_Data$pred <- fitted(House_Model_11)
write.csv(Final_Data,"train_mape.csv")->Write_Train

## Calculated MAPE VALUE
MAPE<-(sum((abs(SalePrice-pred))/SalePrice)/nrow(Final_Data))
MAPE

############ Residual Analysis ############################################################################

res <- Final_Data

res$stu_res <- studres(House_Model_11) ##Studentized residuals
res$cooks_dis <- cooks.distance(House_Model_11) ## Cook's distance

write.csv(res,"res.csv")->residual
residual


### Durbin-Watson Test

durbinWatsonTest(House_Model_11)->DurbinTest
DurbinTest
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)

# Checking multicollinearity
vif(House_Model_11)     # Multicollinearity is approachable or sustainable.

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(House_Model_11)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)


#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05

resids1 <- House_Model_11$residuals
resids1

ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
lillie.test(resids1) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids1) #get Pearson chi-square test for normaility 
sf.test(resids1) #get Shapiro-Francia test for normaility 

qqnorm(resids1)

###########################################################################################################################
############## Testing the model on test data ############################################################################
###########################################################################################################################

read.csv("test.csv", header = TRUE)-> Test_house
Test_house -> House_Test
View(Test_house)
str(House_Test)

# Finding N/A Values & Removing it by their mean values

data.frame(colSums(is.na(House_Test)))

# Columns having N/A values are - MSZoning , LotFrontage , Alley ,Utilities , BsmtQual , BsmtCond , BsmtExposure , BsmtFinType1 , BsmtFinType2 ,BsmtFinSF1 , BsmtFinSF2 , BsmtUnfSF , TotalBsmtSF ,
# ,BsmtFullBath, BsmtHalfBath,KitchenQual,Functional , Exterior1st , Exterior2nd , MasVnrType ,MasVnrArea , FireplaceQu , Garagetype ,GarageYrBlt , GarageFinish , GarageQual , GarageCars , GarageArea , GarageCond , PoolQC , Fence , MiscFeature , SaleType.

levels(House_Test$MSZoning) <- c(levels(House_Test$MSZoning), "No-MSZoning-Access")
House_Test$MSZoning[is.na(House_Test$MSZoning)]<-"No-MSZoning-Access"

levels(House_Test$LotFrontage) <- c(levels(House_Test$LotFrontage), "No-LotFrontage-Access")
House_Test$LotFrontage[is.na(House_Test$LotFrontage)]<-"No-LotFrontage-Access"

levels(House_Test$Alley) <- c(levels(House_Test$Alley), "No-Alley-Access")
House_Test$Alley[is.na(House_Test$Alley)]<-"No-Alley-Access"

levels(House_Test$Utilities) <- c(levels(House_Test$Utilities), "No-Utilities-Access")
House_Test$Utilities[is.na(House_Test$Utilities)]<-"No-Utilities-Access"

levels(House_Test$BsmtQual) <- c(levels(House_Test$BsmtQual), "No-BsmtQual-Access")
House_Test$BsmtQual[is.na(House_Test$BsmtQual)]<-"No-BsmtQual-Access"

levels(House_Test$BsmtCond) <- c(levels(House_Test$BsmtCond), "No-BsmtCond-Access")
House_Test$BsmtCond[is.na(House_Test$BsmtCond)]<-"No-BsmtCond-Access"

levels(House_Test$BsmtExposure) <- c(levels(House_Test$BsmtExposure), "No-BsmtExposure-Access")
House_Test$BsmtExposure[is.na(House_Test$BsmtExposure)]<-"No-BsmtExposure-Access"

levels(House_Test$BsmtFinType1) <- c(levels(House_Test$BsmtFinType1), "No-BsmtFinType1-Access")
House_Test$BsmtFinType1[is.na(House_Test$BsmtFinType1)]<-"No-BsmtFinType1-Access"

levels(House_Test$BsmtFinType2) <- c(levels(House_Test$BsmtFinType2), "No-BsmtFinType2-Access")
House_Test$BsmtFinType2[is.na(House_Test$BsmtFinType2)]<-"No-BsmtFinType2-Access"

levels(House_Test$BsmtFinSF1) <- c(levels(House_Test$BsmtFinSF1), "No-BsmtFinSF1-Access")
House_Test$BsmtFinSF1[is.na(House_Test$BsmtFinSF1)]<-"No-BsmtFinSF1-Access"

levels(House_Test$BsmtFinSF2) <- c(levels(House_Test$BsmtFinSF2), "No-BsmtFinSF2-Access")
House_Test$BsmtFinSF2[is.na(House_Test$BsmtFinSF2)]<-"No-BsmtFinSF2-Access"

levels(House_Test$BsmtUnfSF) <- c(levels(House_Test$BsmtUnfSF), "No-BsmtUnfSF-Access")
House_Test$BsmtUnfSF[is.na(House_Test$BsmtUnfSF)]<-"No-BsmtUnfSF-Access"

levels(House_Test$TotalBsmtSF) <- c(levels(House_Test$TotalBsmtSF), "No-TotalBsmtSF-Access")
House_Test$TotalBsmtSF[is.na(House_Test$TotalBsmtSF)]<-"No-TotalBsmtSF-Access"

levels(House_Test$BsmtFullBath) <- c(levels(House_Test$BsmtFullBath), "No-BsmtFullBath-Access")
House_Test$BsmtFullBath[is.na(House_Test$BsmtFullBath)]<-"No-BsmtFullBath-Access"

levels(House_Test$BsmtHalfBath) <- c(levels(House_Test$BsmtHalfBath), "No-BsmtHalfBath-Access")
House_Test$BsmtHalfBath[is.na(House_Test$BsmtHalfBath)]<-"No-BsmtHalfBath-Access"

levels(House_Test$MasVnrType) <- c(levels(House_Test$MasVnrType), "No-MasVnrType-Access")
House_Test$MasVnrType[is.na(House_Test$MasVnrType)]<-"No-MasVnrType-Access"

levels(House_Test$MasVnrArea) <- c(levels(House_Test$MasVnrArea), "No-MasVnrArea-Access")
House_Test$MasVnrArea[is.na(House_Test$MasVnrArea)]<-"No-MasVnrArea-Access"

levels(House_Test$KitchenQual) <- c(levels(House_Test$KitchenQual), "No-KitchenQual-Access")
House_Test$KitchenQual[is.na(House_Test$KitchenQual)]<-"No-KitchenQual-Access"

levels(House_Test$Functional) <- c(levels(House_Test$Functional), "No-Functional-Access")
House_Test$Functional[is.na(House_Test$Functional)]<-"No-Functional-Access"

levels(House_Test$Exterior1st) <- c(levels(House_Test$Exterior1st), "No-Exterior1st-Access")
House_Test$Exterior1st[is.na(House_Test$Exterior1st)]<-"No-Exterior1st-Access"

levels(House_Test$Exterior2nd) <- c(levels(House_Test$Exterior2nd), "No-Exterior2nd-Access")
House_Test$Exterior2nd[is.na(House_Test$Exterior2nd)]<-"No-Exterior2nd-Access"

levels(House_Test$FireplaceQu) <- c(levels(House_Test$FireplaceQu), "No-FireplaceQu-Access")
House_Test$FireplaceQu[is.na(House_Test$FireplaceQu)]<-"No-FireplaceQu-Access"

levels(House_Test$GarageType) <- c(levels(House_Test$GarageType), "No-GarageType-Access")
House_Test$GarageType[is.na(House_Test$GarageType)]<-"No-GarageType-Access"

levels(House_Test$GarageYrBlt) <- c(levels(House_Test$GarageYrBlt), "No-GarageYrBlt-Access")
House_Test$GarageYrBlt[is.na(House_Test$GarageYrBlt)]<-"No-GarageYrBlt-Access"

levels(House_Test$GarageFinish) <- c(levels(House_Test$GarageFinish), "No-GarageFinish-Access")
House_Test$GarageFinish[is.na(House_Test$GarageFinish)]<-"No-GarageFinish-Access"

levels(House_Test$GarageQual) <- c(levels(House_Test$GarageQual), "No-GarageQual-Access")
House_Test$GarageQual[is.na(House_Test$GarageQual)]<-"No-GarageQual-Access"

levels(House_Test$GarageCond) <- c(levels(House_Test$GarageCond), "No-GarageCond-Access")
House_Test$GarageCond[is.na(House_Test$GarageCond)]<-"No-GarageCond-Access"

levels(House_Test$GarageCars) <- c(levels(House_Test$GarageCars), "No-GarageCars-Access")
House_Test$GarageCars[is.na(House_Test$GarageCars)]<-"No-GarageCars-Access"

levels(House_Test$GarageArea) <- c(levels(House_Test$GarageArea), "No-GarageArea-Access")
House_Test$GarageArea[is.na(House_Test$GarageArea)]<-"No-GarageArea-Access"

levels(House_Test$PoolQC) <- c(levels(House_Test$PoolQC), "No-PoolQC-Access")
House_Test$PoolQC[is.na(House_Test$PoolQC)]<-"No-PoolQC-Access"

levels(House_Test$Fence) <- c(levels(House_Test$Fence), "No-Fence-Access")
House_Test$Fence[is.na(House_Test$Fence)]<-"No-Fence-Access"

levels(House_Test$MiscFeature) <- c(levels(House_Test$MiscFeature), "No-MiscFeature-Access")
House_Test$MiscFeature[is.na(House_Test$MiscFeature)]<-"No-MiscFeature-Access"

levels(House_Test$SaleType) <- c(levels(House_Test$SaleType), "No-SaleType-Access")
House_Test$SaleType[is.na(House_Test$SaleType)]<-"No-SaleType-Access"

data.frame(colSums(is.na(House_Test)))
View(House_Test)
str(House_Test)

# using DPLYR Package function
select(House_Test, c(MSSubClass , MSZoning , LotFrontage,LotArea,OverallQual,OverallCond,YearBuilt , RoofMatl,MasVnrArea,ExterCond,ExterQual , Foundation,WoodDeckSF,OpenPorchSF,EnclosedPorch,X3SsnPorch,ScreenPorch,PoolArea,PoolQC,Fence,MiscFeature,MiscVal,MoSold,YrSold,SaleType,SaleCondition))-> House_Test
dim(House_Test)


# SalePrice Variable is an Independent Variable.

par(mfrow = c(2,2))
#plot(House_Model_11)
pred_Test<-predict(House_Model_11, newdata = House_Test)
write.csv(pred_Test , "Test_Prediction.csv")
attach(House_Test)
#House_Test$pred_Test <- fitted(House_Model_11)      # it gives error dont understand why?
write.csv(House_Test,"mape.csv")->Write_Test

## Calculated MAPE VALUE
MAPE_test<-(sum((abs(SalePrice-pred_Test))/SalePrice)/nrow(House_Test))
MAPE_test
