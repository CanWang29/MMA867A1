if (!require(ggplot2)) install.packages("ggplot2")
if (!require(VIM)) install.packages("VIM")
if (!require(mice)) install.packages("mice")
if (!require(missForest)) install.packages("missForest")
if (!require(glmnet)) install.packages("glmnet")
if (!require(caret)) install.packages("caret")
if (!require(moments)) install.packages("moments")
if (!require(MASS)) install.packages("MASS")
if (!require(Metrics)) install.packages("Metrics")



#import both train and test dataset into dataframes
house.train <- read.csv("C:\\Users\\wangc\\Desktop\\house-prices-advanced-regression-techniques\\train.csv", header = TRUE, sep = ',')
house.test <- read.csv("C:\\Users\\wangc\\Desktop\\house-prices-advanced-regression-techniques\\test.csv", header = TRUE, sep = ',')
str(house.train)
str(house.test)

#Save the "Id" column as I will drop it for prediction but add it back for final submission
house.train.Id <- subset(house.train, select = Id)
house.test.Id <- subset(house.test, select = Id)

#Drop the "Id" column as it is not useful for prediction
house.train <- subset(house.train, select = -c(Id))
house.test <- subset(house.test, select = -c(Id))


#basic graph to check outliers, and remove them
graph.GrLivArea.SalePrice <- ggplot(house.train, aes(x=GrLivArea, y=SalePrice)) +
  geom_point() +
  labs(x = "GrLivArea", y = "SalePrice")
graph.GrLivArea.SalePrice
house.train <- subset(house.train, house.train$GrLivArea <= 4500)


graph.LotArea.SalePrice <- ggplot(house.train, aes(x=LotArea, y=SalePrice)) +
  geom_point() +
  labs(x = "LotArea", y = "SalePrice")
graph.LotArea.SalePrice
house.train <- subset(house.train, house.train$LotArea <= 100000)


#Add SalePrice into test dataframe to combine it with train
house.test$SalePrice <- NA


#Combine two dataframes for replacing missing data if there are any
house <- rbind(house.train, house.test)
str(house)
head(house, 10)


#Check the distribution of SalePrice
d.SalePrice <- density(house.train$SalePrice)
plot(d.SalePrice, main = "Kernel Density of Sale Price")
polygon(d.SalePrice, col = "red", border = "blue")


#distribution with the logarithm
d.SalePrice.log <- density(log(house.train$SalePrice + 1))
plot(d.SalePrice.log, main = "Kernel Density of log Sale Price")
polygon(d.SalePrice.log, col = "red", border = "blue")
#more closed to normal distribution, good


#So we replace saleprice by the logarithm
house$SalePrice <- log(house$SalePrice + 1)


#check missing values
summary(house)
aggr(house, prop = F, numbers = T)
colSums(is.na(house))


#For some variables, NA means "None" but not values are missing
replace.NA.None <- function(column){
  factor(ifelse(is.na(house[[column]]) == TRUE, "None", paste(house[[column]])))
}

house$Alley <- replace.NA.None("Alley")
house$BsmtQual <- replace.NA.None("BsmtQual")
house$BsmtCond <- replace.NA.None("BsmtCond")
house$BsmtExposure <- replace.NA.None("BsmtExposure")
house$BsmtFinType1 <- replace.NA.None("BsmtFinType1")
house$BsmtFinType2 <- replace.NA.None("BsmtFinType2")
house$FireplaceQu <- replace.NA.None("FireplaceQu")
house$GarageType <- replace.NA.None("GarageType")
house$GarageFinish <- replace.NA.None("GarageFinish")
house$GarageQual <- replace.NA.None("GarageQual")
house$GarageCond <- replace.NA.None("GarageCond")
house$PoolQC <- replace.NA.None("PoolQC")
house$Fence <- replace.NA.None("Fence")
house$MiscFeature <- replace.NA.None("MiscFeature")
house$MasVnrType <- replace.NA.None("MasVnrType")


#For LotFrontage, it's related to neighborhood, so I replace missing data with the median per neighborhood
#calculate the median of LotFrontage per neighborhood first
LotFrontage.Neighborhood <- aggregate(LotFrontage ~ Neighborhood, data = house, median)
imputed.LotFrontage <- c(house$Neighborhood[is.na(house$LotFrontage)])
house$LotFrontage[is.na(house$LotFrontage)] <- LotFrontage.Neighborhood[imputed.LotFrontage, 2]


#Some NA means 0, but not the data missing, so replace them by 0
replace.NA.0 <- function(column){
  as.integer(ifelse(is.na(house[[column]]) == TRUE, 0, paste(house[[column]])))
}

house$BsmtFinSF1 <- replace.NA.0("BsmtFinSF1")
house$BsmtFinSF2  <- replace.NA.0("BsmtFinSF2")
house$BsmtUnfSF <- replace.NA.0("BsmtUnfSF")
house$TotalBsmtSF <- replace.NA.0("TotalBsmtSF")
house$GarageCars <- replace.NA.0("GarageCars")
house$GarageArea <- replace.NA.0("GarageArea")
house$GarageYrBlt <- replace.NA.0("GarageYrBlt")
house$BsmtFullBath <- replace.NA.0("BsmtFullBath")
house$BsmtHalfBath <- replace.NA.0("BsmtHalfBath")
house$GarageYrBlt <- replace.NA.0("GarageYrBlt")
house$MasVnrArea <- replace.NA.0("MasVnrArea")


#Use missForest to impute other missing values
#house.imp <- missForest(house)
#But looks like missForest replace all missing categorical data to NA, I decide not to do this


#So for missing categorical data, I replace them by most frequent value
house$MSZoning <- factor(ifelse(is.na(house$MSZoning) == TRUE, "RL", paste(house$MSZoning)))
house$Utilities  <- factor(ifelse(is.na(house$Utilities) == TRUE, "AllPub", paste(house$Utilities)))
house$Exterior1st  <- factor(ifelse(is.na(house$Exterior1st) == TRUE, "VinylSd", paste(house$Exterior1st)))
house$Exterior2nd  <- factor(ifelse(is.na(house$Exterior2nd) == TRUE, "VinylSd", paste(house$Exterior2nd)))
house$SaleType  <- factor(ifelse(is.na(house$SaleType) == TRUE, "WD", paste(house$SaleType)))
house$Functional  <- factor(ifelse(is.na(house$Functional) == TRUE, "Typ", paste(house$Functional)))
house$KitchenQual  <- factor(ifelse(is.na(house$KitchenQual) == TRUE, "TA", paste(house$KitchenQual)))
house$Electrical  <- factor(ifelse(is.na(house$Electrical) == TRUE, "SBrkr", paste(house$Electrical)))


#Double check if there are still any missing values
colSums(is.na(house))
sum(is.na(house))
#[1] 1459
#Only saleprice left, good


#More features engeneering
str(house)


#MSSubClass should be factor
house$MSSubClass <- factor(house$MSSubClass)


#OverallQual & OverallCond should be factor
house$OverallQual <- factor(house$OverallQual)
house$OverallCond <- factor(house$OverallCond)


#MoSold can be factor
house$MoSold  <- factor(house$MoSold)


#the Utilities in test dataframe only has one level, so I can remove it safely
house <- subset(house, select = -c(Utilities))


#Add more variables
house$TotalSF <- house$TotalBsmtSF + house$GrLivArea
house$TotBathrooms <- house$FullBath + house$HalfBath*0.5 + house$BsmtFullBath + house$BsmtHalfBath*0.5


#Change years and month to factor?? No, because if I do that, there will be some missing levels
#completed.house$YrSold <- factor(completed.house$YrSold)
#completed.house$GarageYrBlt <- factor(completed.house$GarageYrBlt)
#completed.house$YearBuilt <- factor(completed.house$YearBuilt)
#completed.house$YearRemodAdd <- factor(completed.house$YearRemodAdd)


#If the categorical variables is ranking, they need to be converted to integers to represent rank
category.rank <- c("MSSubClass", "Street", "Alley", "Functional", "LandSlope", "LotShape", "BsmtQual", "BsmtCond",
                   "BsmtFinType1", "BsmtFinType2", "BsmtExposure", "GarageFinish", "GarageQual", "GarageCond",
                   "ExterQual", "ExterCond", "HeatingQC", "KitchenQual", "PavedDrive", "PoolQC", "Fence", "FireplaceQu",
                   "OverallCond", "CentralAir", "MoSold", "YrSold")

MSSubClass <- c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", "90", "120", "150", "160", "180", "190")
Street <- c("Grvl", "Pave")
Alley <- c("None", "Grvl", "Pave")
Functional <- c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
LandSlope <- c("Sev", "Mod", "Gtl")
LotShape <- c("IR3", "IR2", "IR1", "Reg")
BsmtQual <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtCond <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtExposure <- c("None", "No", "Mn", "Av", "Gd")
GarageFinish <- c("None", "Unf", "RFn", "Fin")
GarageQual <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual <- c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond <- c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC <- c("Po", "Fa", "TA", "Gd", "Ex")
KitchenQual <- c("Po", "Fa", "TA", "Gd", "Ex")
PavedDrive <- c("N", "P", "Y")
PoolQC <- c("None", "Fa", "TA", "Gd", "Ex")
Fence <- c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
FireplaceQu <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
OverallCond <- NA
CentralAir <- NA
MoSold <- NA
YrSold <- NA

category.levels <- list(MSSubClass, Street, Alley, Functional, LandSlope, LotShape, BsmtQual, BsmtCond,
                   BsmtFinType1, BsmtFinType2, BsmtExposure, GarageFinish, GarageQual, GarageCond,
                   ExterQual, ExterCond, HeatingQC, KitchenQual, PavedDrive, PoolQC, Fence, FireplaceQu,
                   OverallCond, CentralAir, MoSold, YrSold)

#replace 1-n to represent the rank
i <- 1
for (c in category.rank) {
  if (c == "OverallCond" | c == "CentralAir" | c == "MoSold" | c == "YrSold") {
    house[, c] = as.numeric(factor(house[, c]))
  } else house[, c] = as.numeric(factor(house[, c], levels = category.levels[[i]]))
  i = i + 1
}


#Split dataframe to categorical variables and non-categorical variables
#get the data type of all variables
data.type.house <- sapply(names(house), function(x) {
  class(house[[x]])
})


#select all variables which data type is not factor (categorical variables)
non.factor.variables <- names(data.type.house[data.type.house != "factor"])


#select all variables which data type is factor
factor.variables <- names(data.type.house[data.type.house == "factor"])


#use dummyVars function to change categorical variables to dummy variables (for each level)
dummies <- dummyVars(~., house[factor.variables])
categorical.dummy <- predict(dummies, house[factor.variables])


#create new dataframe by combining numeric and categorical variables
house <- cbind(house[non.factor.variables], categorical.dummy)


#Separate dataframe into train, test, and prediction
house.train.full <- house[1:1454, ]
house.prediction <- house[1455:2913, ]
set.seed(1000)
house.train.split <- createDataPartition(y = house.train.full$SalePrice, p = 0.75, list = FALSE)
house.train.new <- house.train.full[house.train.split, ]
house.test.new <- house.train.full[-house.train.split, ]


#build cross-validation
set.seed(1001)
cv.lasso <- cv.glmnet(as.matrix(house.train.new[, -58]), house.train.new[, 58])


#make predictions of new test dataframe based on our model and check the RMSE
prediction.test <- predict(cv.lasso, newx = as.matrix(house.test.new[, -58]), s = "lambda.min")
rmse(house.test.new$SalePrice, prediction.test)
#[1] 0.1015648


#use full training dataframe to make final model
set.seed(1001)
cv.lasso <- cv.glmnet(as.matrix(house.train.full[, -58]), house.train.full[, 58])


#make predictions of our prediction dataframe
predictions <- data.frame(exp(predict(cv.lasso, newx = as.matrix(house.prediction[, -58]), s = "lambda.min")) - 1)


#create final submission dataframe and export to csv
predictions <- cbind(house.test.Id, predictions)
names(predictions)[2] <- "SalePrice"
write.csv(predictions, "SalePrice_Prediction_Final.csv", row.names = FALSE)


