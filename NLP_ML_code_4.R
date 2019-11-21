
# README!
# This R script runs the loading of the "What's Cooking" dataset from Kaggle, performs an Exploratory Data Analysis,
# performs some NLP pre-processing steps, plots wordclouds, performs data cleaning, builds both the Decision Tree
# and Random Forest models, and plots their respective confusion matrices

# ======================= Loading the library ============================

library(jsonlite)
library(dplyr)
library(ggplot2)
library(tm) # For NLP; creating bag-of-words
library(caret)
library(rpart)
library(rpart.plot)
library(rjson)
library(magrittr)
library(randomForest)
library(caret)

# ======================= Loading dataset from Kaggle =====================

train <- fromJSON(file = "train.json")

# ======================= Exploratory Data Analysis (EDA) =================

#Basic analysis of the files 
size <- file.size(c("train.json"))
size <- round((size/1024)/1000)   # convert to MB
lines <- sapply(list(train), length)
characters <- sapply(list(train), function(x){sum(nchar(x))})
summary <- data.frame(files =c("train.json"),size,lines,characters)
summary

#Plot of histogram showing the number of recipes of each cuisine
traindf <- c()
for (i in 1:39774){
  traindf <- append(traindf, train[[i]]$cuisine)
  }
traindf <- as.data.frame(traindf)

ggplot(data = traindf, aes(x = traindf)) + 
  geom_histogram(stat= "count") +
  labs(title = "Cuisines", x = "Cuisine", y = "Number of Recipes")

#Create corpus from the list of ingredients
ingredients <- list()
for (i in 1:39774){
  ingre_list <- list(train[[i]]$ingredients)
  ingredients <- append(ingredients, ingre_list)
}

ingredients <- Corpus(VectorSource(ingredients))

#Preprocessing steps
ingredients <- tm_map(ingredients, tolower)
ingredients <- tm_map(ingredients, removePunctuation)
ingredients <- tm_map(ingredients, removeNumbers)
ingredients <- tm_map(ingredients, stripWhitespace)
ingredients <- tm_map(ingredients, stemDocument)

ingredients

#Create a Document Term Matrix
ingredientsDTM <- DocumentTermMatrix(ingredients)
ingredientsDTM

#Remove sparse terms 
sparse_removal <- removeSparseTerms(ingredientsDTM, 0.95)
colnames(sparse_removal) <- make.names(colnames(sparse_removal))
ingredientsDTM <- as.data.frame(as.matrix(sparse_removal))

# Add the dependent variable to the data.frame

ingredientsDTM$cuisine <- as.factor(traindf$traindf)

ingredientsDTM_2 <- ingredientsDTM

# ==================== Undersampling of abundant classes (cuisines) ========================

new_italian <- sample(which(ingredientsDTM_2$cuisine=="italian"),2000,replace=FALSE)
new_mexican <- sample(which(ingredientsDTM_2$cuisine=="mexican"),2000,replace=FALSE)
new_southern_us <- sample(which(ingredientsDTM_2$cuisine=="southern_us"),2000,replace=FALSE)
new_indian <- sample(which(ingredientsDTM_2$cuisine=="indian"),2000,replace=FALSE)
new_chinese <- sample(which(ingredientsDTM_2$cuisine=="chinese"),2000,replace=FALSE)
new_french <- sample(which(ingredientsDTM_2$cuisine=="french"),2000,replace=FALSE)
cajun_creole <- which(ingredientsDTM_2$cuisine=="cajun_creole")
japanese <- which(ingredientsDTM_2$cuisine=="japanese")
greek <- which(ingredientsDTM_2$cuisine=="greek")
spanish <- which(ingredientsDTM_2$cuisine=="spanish")
korean <- which(ingredientsDTM_2$cuisine=="korean")
vietnamese <- which(ingredientsDTM_2$cuisine=="vietnamese")
moroccan <- which(ingredientsDTM_2$cuisine=="moroccan")
british <- which(ingredientsDTM_2$cuisine=="british")
filipino <- which(ingredientsDTM_2$cuisine=="filipino")
irish <- which(ingredientsDTM_2$cuisine=="irish")
jamaican <- which(ingredientsDTM_2$cuisine=="jamaican")
russian <- which(ingredientsDTM_2$cuisine=="russian")
brazilian <- which(ingredientsDTM_2$cuisine=="brazilian")
thai <- which(ingredientsDTM_2$cuisine == "thai")
ingredientsDTM_3 <- ingredientsDTM_2[c(new_italian,new_mexican,new_southern_us,new_indian,new_chinese,new_french,cajun_creole,japanese,greek,spanish,korean,vietnamese,moroccan,british,filipino,irish,jamaican,russian,brazilian,thai),]

ingredientsDTM_3 <- na.omit(ingredientsDTM_3)

# ================== Create Decision Trees model (CART model) ===============

cartModelFit <- rpart(cuisine ~ ., data =ingredientsDTM_3, method = "class")
plot(cartModelFit, uniform = TRUE)
 
plotcp(cartModelFit)
rpart2 <- prune(cartModelFit, cp = 0.01)

#Plot the decision tree
plot(rpart2, uniform = TRUE)
text(rpart2, use.n = FALSE, cex = 0.75)

# ===================== Splitting of training and testing set ========================

inTrain <- createDataPartition(y = ingredientsDTM_3$cuisine, p = 0.8, list = FALSE)

training_DT <- ingredientsDTM_3[inTrain,]
training <- ingredientsDTM_3[inTrain,]

testing_DT <- ingredientsDTM_3[-inTrain,]
testing <- ingredientsDTM_3[-inTrain,]

# ================ Cleaning of testing and training set ==============================
# cleaning of items that are not ingredients (testing set)

testing <- subset(testing, select = -c(2, 13, 14, 20, 21, 25,26,29,30,37,38,40,42,46,51,52,57,61,65,69,72,74,76,78,80,81,82,84))

names(testing)[names(testing) == "chees"] <- "cheese"
names(testing)[names(testing) == "oliv"] <- "olive"
names(testing)[names(testing) == "season"] <- "seasoning"
names(testing)[names(testing) == "veget"] <- "vegetable"
names(testing)[names(testing) == "sauc"] <- "sauce"
names(testing)[names(testing) == "juic"] <- "juice"
names(testing)[names(testing) == "sesam"] <- "sesame"
names(testing)[names(testing) == "leav"] <- "leaves"
names(testing)[names(testing) == "coriand"] <- "coriander"

# cleaning of items that are not ingredients (training set)

training <- subset(training, select = -c(2, 13, 14, 20, 21, 25,26,29,30,37,38,40,42,46,51,52,57,61,65,69,72,74,76,78,80,81,82,84))

names(training)[names(training) == "chees"] <- "cheese"
names(training)[names(training) == "oliv"] <- "olive"
names(training)[names(training) == "season"] <- "seasoning"
names(training)[names(training) == "veget"] <- "vegetable"
names(training)[names(training) == "sauc"] <- "sauce"
names(training)[names(training) == "juic"] <- "juice"
names(training)[names(training) == "sesam"] <- "sesame"
names(training)[names(training) == "leav"] <- "leaves"
names(training)[names(training) == "coriand"] <- "coriander"

# ====================== Plotting DT Confusion Matrix ======================================

# building decision trees confusion matrix
cartPredict <- predict(rpart2, testing_DT, type = "class")
cartCM <- confusionMatrix(cartPredict, testing_DT$cuisine)
cartCM

cartCMtable <- as.data.frame(cartCM$table)

#Plot the multiclass confusion matrix 

ggplot(cartCMtable, aes(x=Reference, y=Prediction, fill=Freq)) + geom_tile()

# ============================== Building RF Model ===========================

# building random forest model

RF_classifier <- randomForest(x = training, y = training$cuisine, ntree= 100)

# ======================Predict and plot RF Confusion Matrix =============================

c_pred <- predict(RF_classifier, newdata = testing)
cart_RF <- confusionMatrix(c_pred, testing$cuisine)
cart_RF

cartRFtable <- as.data.frame(cart_RF$table)

ggplot(cartRFtable, aes(x=Reference, y=Prediction, fill=Freq)) + geom_tile()

# ====================== Saving variables as files ====================================

saveRDS(training, file = "training_4.rds")
saveRDS(testing, file = "testing_4.rds")
saveRDS(RF_classifier, file = "RF_classifier_4.rds")

