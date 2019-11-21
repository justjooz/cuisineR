# This R Shiny dashboard predicts cuisines based on 10 selected ingredients. Developed by Justin Chia.

# ============================ Loading packages ===========================

library(shinydashboard)
library(shiny)
library(caret)
library(randomForest)
library(anchors)

# ============================ Reading testing files =======================

testing_4 <- readRDS("./testing_4.rds") # read testing data
RF_classifier_4 <- readRDS("./RF_classifier_4.rds") # random forests model
# rpart2 <- readRDS("./rpart2.rds") # CART model

# ============================= Data pre-processing =========================

testing_4[,1:58][testing_4[,1:58]==1] <- 0
testing_4[,1:58][testing_4[,1:58]==2] <- 0
testing_4[,1:58][testing_4[,1:58]==3] <- 0

input_df <- testing_4[1,]

ingredient_dropdown_4 <- colnames(input_df)

# ================================== UI ======================================

ui <- dashboardPage(
  dashboardHeader(title = "Welcome to cuisineR!"),
  dashboardSidebar(
    
    titlePanel(h2("Select 10 Ingredients", align = "center")),
    
                   selectInput("ingred1", "Ingredient 1:", 
                               choices=ingredient_dropdown_4),
                   selectInput("ingred2", "Ingredient 2:", 
                               choices=ingredient_dropdown_4),
                   selectInput("ingred3", "Ingredient 3:", 
                               choices=ingredient_dropdown_4),
                   selectInput("ingred4", "Ingredient 4:", 
                               choices=ingredient_dropdown_4),
                   selectInput("ingred5", "Ingredient 5:", 
                               choices=ingredient_dropdown_4),
                   selectInput("ingred6", "Ingredient 6:", 
                               choices=ingredient_dropdown_4),   
                   selectInput("ingred7", "Ingredient 7:", 
                              choices=ingredient_dropdown_4),
                   selectInput("ingred8", "Ingredient 8:", 
                              choices=ingredient_dropdown_4),
                   selectInput("ingred9", "Ingredient 9:", 
                              choices=ingredient_dropdown_4),
                   selectInput("ingred10", "Ingredient 10:", 
                              choices=ingredient_dropdown_4),
                   submitButton("Submit"),
                   hr(),
                   helpText("Model built from data from KAGGLE's What's Cooking Dataset."),
                   hr(),
                   helpText("This R shiny web app was made in fulfilments of requirements of a project in BS3033: Data Science for Biologists."),
                   helpText("Machine Learning pipeline built by Kai Jin, Justin, Shu Chen, and Hannah. R Shiny Dashboard developed by Justin.")
                   ),
  dashboardBody(
    valueBox("PREDICTED CUISINE:",textOutput("final"))
  )
)

# ================================== SERVER ===================================

server <- function(input, output) {
  
  
  output$final <- renderText({
    
    cook_up_fun <- function(a, b, c, d, e, f, g, h, i, j){ # this function adds 1 binary value of 1 to a data frame (input_df)
                                                           # input_df will then be compared to the RF model for prediction)
      
      if (any(a == colnames(input_df))){
        input_df[1,which(a == colnames(input_df))] <- 1
      }
      
      if (any(b == colnames(input_df))){
        input_df[1,which(b == colnames(input_df))] <- 1
      } 
      
      if (any(c == colnames(input_df))){
        input_df[1,which(c == colnames(input_df))] <- 1
      } 
      
      if (any(d == colnames(input_df))){
        input_df[1,which(d == colnames(input_df))] <- 1
      } 
      
      if (any(e == colnames(input_df))){
        input_df[1,which(e == colnames(input_df))] <- 1
      } 
      
      if (any(f == colnames(input_df))){
        input_df[1,which(f == colnames(input_df))] <- 1
      } 
      
      if (any(g == colnames(input_df))){
        input_df[1,which(g == colnames(input_df))] <- 1
      } 
      
      if (any(h == colnames(input_df))){
        input_df[1,which(h == colnames(input_df))] <- 1
      } 
      
      if (any(i == colnames(input_df))){
        input_df[1,which(i == colnames(input_df))] <- 1
      } 
      
      if (any(j == colnames(input_df))){
        input_df[1,which(j == colnames(input_df))] <- 1
      } 
      
      pred <- predict(RF_classifier_4, input_df)
      pred_cuisine <- toupper(names(which(table(pred) == 1)))
      return(pred_cuisine)
      
    }
    
    final <- cook_up_fun(input$ingred1, input$ingred2, input$ingred3, input$ingred4, input$ingred5, input$ingred6, input$ingred7, input$ingred8, input$ingred9, input$ingred10)
  })
  
}


# =================================RUN SHINY APP===================================

shinyApp(ui, server)
