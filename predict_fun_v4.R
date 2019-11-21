# This script highlight the building of a function to produce a prediction of cuisine based on ingredient input.
# This script was used to debug code and was implemented fully in the R shiny dashboard

# ======================= Loading of packages ======================
library(caret)
library(randomForest)
library(anchors)
library(ROCR)

# ======================= Reading of files ==========================

testing_4 <- readRDS("./testing_4.rds")
RF_classifier_4 <- readRDS("./RF_classifier_4.rds") # random forests model
# rpart2 <- readRDS("./rpart2.rds") # decision trees model

# ======================= Initialising input ingredients dataframe ===

testing_4[,1:58][testing_4[,1:58]==1] <- 0
testing_4[,1:58][testing_4[,1:58]==2] <- 0
testing_4[,1:58][testing_4[,1:58]==3] <- 0

input_df <- testing_4[1,]

# ======================== To pick 10 random ingredients: =============

random_ingred <- sample(colnames(input_df), 10, replace = T)

ingred_1 <- random_ingred[1]
ingred_2 <- random_ingred[2]
ingred_3 <- random_ingred[3]
ingred_4 <- random_ingred[4]
ingred_5 <- random_ingred[5]
ingred_6 <- random_ingred[6]
ingred_7 <- random_ingred[7]
ingred_8 <- random_ingred[8]
ingred_9 <- random_ingred[9]
ingred_10 <- random_ingred[10]

# ================ Function that forms dataframe of input ingredients to be predicted by the RF model ============

cook_up_fun <- function(a, b, c, d, e, f, g, h, i, j){
  
  
  if (any(a == colnames(input_df))){
    input_df[1,which(ingred_1 == colnames(input_df))] <- 1
  }
  
  if (any(b == colnames(input_df))){
    input_df[1,which(ingred_2 == colnames(input_df))] <- 1
  } 
  
  if (any(c == colnames(input_df))){
    input_df[1,which(ingred_3 == colnames(input_df))] <- 1
  } 
  
  if (any(d == colnames(input_df))){
    input_df[1,which(ingred_4 == colnames(input_df))] <- 1
  } 
  
  if (any(e == colnames(input_df))){
    input_df[1,which(ingred_5 == colnames(input_df))] <- 1
  } 
  
  if (any(f == colnames(input_df))){
    input_df[1,which(ingred_6 == colnames(input_df))] <- 1
  } 

  if (any(g == colnames(input_df))){
    input_df[1,which(ingred_7 == colnames(input_df))] <- 1
  } 
  
  if (any(h == colnames(input_df))){
    input_df[1,which(ingred_8 == colnames(input_df))] <- 1
  } 
  
  if (any(i == colnames(input_df))){
    input_df[1,which(ingred_9 == colnames(input_df))] <- 1
  } 
  
  if (any(j == colnames(input_df))){
    input_df[1,which(ingred_10 == colnames(input_df))] <- 1
  } 
  
  pred <- predict(RF_classifier_4, input_df)
  pred_cuisine <- names(which(table(pred) == 1))
  if (class(pred_cuisine) == "character"){
    cuisine_output <- paste("The predicted cusine for the ingredients chosen is:", pred_cuisine, sep=" ")
  }
  return(cuisine_output)
}


# ================================= Testing of code =========================================================

cook_up_fun(ingred_1, ingred_2, ingred_3, ingred_4, ingred_5, ingred_6, ingred_7, ingred_8, ingred_9, ingred_10)

