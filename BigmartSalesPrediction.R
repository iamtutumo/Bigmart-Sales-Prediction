              ###########################################
              #######Exploratory Data Analysis###########
              ###########################################


#working directory
path <- "C:/Users/yudhisthirs/Desktop/DataCamp_Material/10.Projects/4. Bigmart Sales Data Set"

#set working directory
setwd(path)


#loading train and test
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#checking dimension 
dim(train)
dim(test)

#structure of train 
str(train)


#check if this data has missing values
sum(is.na(train))
sum(is.na(test))


#check the variables in which these values are missing
colSums(is.na(train))
colSums(is.na(test))

#summary 
summary(train)
summary(test)



                ###########################################
                ##Graphical Representation of Variables####
                ###########################################



library(ggplot2)
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size =2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") +   ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(train, aes(x= Item_Weight, y = Item_Outlet_Sales)) + geom_point(size =2.5, color="navy") + xlab("Item Weight") + ylab("Item Outlet Sales") +   ggtitle("Item Wt vs Item Outlet Sales")

ggplot(train, aes(x= Item_MRP, y = Item_Outlet_Sales)) + geom_point(size =2.5, color="navy") + xlab("Item MRP") + ylab("Item Outlet Sales") +   ggtitle("Item MRP vs Item Outlet Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity")+theme(axis.text.x = element_text(angle =90, vjust = 0.1, color = "navy")) +  xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

#running for outlier - Item_Type Vs Item_MRP
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") +  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) +  xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")





              ###########################################
              ########Dealing with missing obs###########
              ###########################################




test$Item_Outlet_Sales <- 1
combi <- rbind(train, test)

#replacing missing with median values of Item_weight
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
sum(is.na(combi))


#Trouble with Continuous Variables & Categorical Variables
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,median(combi$Item_Visibility), combi$Item_Visibility)

levels(combi$Outlet_Size)[1] <- "Other"

library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)



              ###########################################
              #########Data Manipulation#################
              ###########################################



library(dplyr)

a <- combi%%   group_by(Outlet_Identifier)%%   tally()
names(a)[2] <- "Outlet_Count"
combi <- full_join(a, combi, by = "Outlet_Identifier")

b <- combi%%   group_by(Item_Identifier)%%   tally()
names(b)[2] <- "Item_Count"
combi <- merge(b, combi, by = "Item_Identifier")


combi$Outlet_Year <- (2013 - combi$Outlet_Establishment_Year)

q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)
combi$Item_Type_New <- q


#Label Encoding and One Hot Encoding

combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)


library(dummies)
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'), sep='_')
str (combi)



              ###########################################
              #######Multiple Linear Regression model######
              ###########################################

#Predictive Modeling using Machine Learning
#Finally, we'll drop the columns which have either been converted using other variables or are identifier variables.
#This can be accomplished using select from dplyr package.

combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content,Outlet_Establishment_Year,Item_Type))
str(combi)


#Splitting train and test back
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]


#Let's now build out first regression model on this data set.
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)


#correlated predictor variables brings down the model accuracy
corr_mat <- data.frame(cor(new_train))
write.csv(corr_mat, file = "corr_mat.csv")


#Let's try to create a more robust regression model. This time, I'll be using a building a simple model without
#encoding and new features. Below is the entire code:



#load data
 train <- read.csv("train.csv")
 test <- read.csv("test.csv")
#create a new variable in test file
 test$Item_Outlet_Sales <- 1
#combine train and test data
 combi <- rbind(train, test)
#impute missing value in Item_Weight
 combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm =
                                                          TRUE)
#impute 0 in item_visibility
 combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                  median(combi$Item_Visibility), combi$Item_Visibility)
#rename level in Outlet_Size
 levels(combi$Outlet_Size)[1] <- "Other"
#rename levels of Item_Fat_Content
 library(plyr)
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg"
                                                             = "Regular"))
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
#create a new column 2013 - Year
23/30
 combi$Year <- 2013 - combi$Outlet_Establishment_Year
#drop variables not required in modeling
 library(dplyr)
 combi <- select(combi, -c(Item_Identifier, Outlet_Identifier,
                            Outlet_Establishment_Year))
#divide data set
 new_train <- combi[1:nrow(train),]
 new_test <- combi[-(1:nrow(train)),]
#linear regression
 linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
 summary(linear_model)
 
 
 #Let's check out regression plot to find out more ways to improve this model.
 par(mfrow=c(2,2))
 plot(linear_model)
 
 #Fitted values are the predicted
 #values. If you see carefully, you'll discover it as a funnel shape graph (from right to left ). The shape of this graph
 #suggests that our model is suffering from heteroskedasticity (unequal variance in error terms). Had there been
 #24/30
 #constant variance, there would be no pattern visible in this graph.
 #A common practice to tackle heteroskedasticity is by taking the log of response variable. Let's do it and check if we
 #can get further improvement.
 
 linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
 summary(linear_model)
 
 
#let's check our RMSE
 library(Metrics)
 rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))
 
 
 
 
 
               ###########################################
               #######  Decision Trees model##############
               ###########################################
 
 
 #loading required libraries
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)
 
 #setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
 
 #decision tree
tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart",trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)


#The final value for cp = 0.01. You can also check the table populated in console for more information. The model
#with cp = 0.01 has the least RMSE. Let's now build a decision tree with 0.01 as complexity parameter.


main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train, control =rpart.control(cp=0.01))
prp(main_tree)


pre_score <- predict(main_tree, type = "vector")
rmse(new_train$Item_Outlet_Sales, pre_score)


              ###########################################
              #######  Random Forest       ##############
              ###########################################




#load randomForest library
library(randomForest)

#set tuning parameters
control <- trainControl(method = "cv", number = 5)

#random forest model
rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "parRF", trControl = control, prox = TRUE, allowParallel = TRUE)
print(rf_model)


#Now we've got the optimal value of mtry = 15. Let's use 1000 trees for computation.
#random forest model
forest_model <- randomForest(Item_Outlet_Sales ~ ., data = new_train, mtry = 15,ntree = 1000)
print(forest_model)
varImpPlot(forest_model)
