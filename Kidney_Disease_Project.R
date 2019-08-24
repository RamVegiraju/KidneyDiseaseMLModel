#Objective: Utilize Random Forest ML techniques off of a single decision tree to see how accuratelt kidney disease(ckd) is classified by model
#Reading in from UCI ML Repository
df <- read.csv('kidney_disease.csv')

#check format of data
head(df)
str(df) #93, 45, 47 levels for cell volume, white blood cell, and red blood cell counts

#three numeric variables being read as categorical with factors, need to convert to numeric
df$pcv <- as.numeric(as.character(df$pcv))
str(df)
df$wc <- as.numeric(as.character(df$wc))
str(df)
df$rc <- as.numeric(as.character(df$rc))
str(df) #these three factor variables are now numeric

#Albumin and sugar are factor variables being read as numeric, must be converted to factors
df$al <- as.factor(as.numeric(df$al))
str(df)
df$su <- as.factor(as.numeric(df$su))
str(df)

#there are missing NA values detected in the conversion to account for 
library(Amelia)
missmap(df, main="Kidney Disease Missing Values", 
        col=c("yellow", "black"), legend=FALSE)

#Large amounts of data missing in the first few columns specifically and about 20% off other columns
#IMPUTATION OF DATA USING KNN, dataset is relatively small and not highly dimensional 
install.packages('mice')
install.packages('missForest')
install.packages('VIM')
library(mice)
library(missForest)
library(VIM)

summary(df) #check for amount of NA's in each variable
imputed_KNN <- kNN(df, variable = c("age","bp","sg","al","su","bgr","bu","sc","sod","pot","hemo", "pcv",
                                    "wc","rc"))
summary(imputed_KNN) #no NA's present in summary anymore

missmap(imputed_KNN, main="Kidney Disease Missing Values", 
        col=c("yellow", "black"), legend=FALSE) #No missing values on map anymore

#KNN adds columns with _imp for its algorithm, remove these columns to get a clean final set before EDA and splitting
train_data <- subset(imputed_KNN, select = id:classification)
summary(train_data)

#Exploratory Data Analysis
library(ggplot2)
library(ggplot2movies)
install.packages('DataExplorer')
library(DataExplorer)

#Using Data Explorer functions, let us get an overview of what our data looks like right now visually
plot_str(train_data)
plot_missing(train_data) #confirm no missing values

#Quantitative Variables using ggplot2
#Relationship between age and potential kidney disease
ggplot(train_data,aes(age,bp)) + geom_point(aes(color=classification))
ggplot(train_data,aes(age)) + geom_histogram(aes(fill=classification),color='black',bins=50) #Slightly positive relationship
#Relationship between blood pressure and potential kidney disease
ggplot(train_data,aes(bp)) + geom_histogram(aes(fill=classification),color='black',bins=50) #Strong relationship, split in Random Forest clear
#Relationship between specific gravity and potential kidney disease
ggplot(train_data,aes(sg)) + geom_histogram(aes(fill=classification),color='black',bins=50) #Lower specific gravity leads to ckd/potential kidney disease

#Used Data Explorer Function for qualitative variables, QQ plots, and a general EDA overview, generated report and attached link
create_report(train_data)

#Splitting data
library(caTools)

sample <- sample.split(train_data$classification, SplitRatio = .7)
train = subset(train_data, sample == TRUE)
test = subset(train_data, sample == FALSE)

#Creating a singular Decision Tree before comparing to Random Forest Method
library(rpart)
decision_tree <- rpart(classification ~., method = 'class', data = train)

#Predict data using singular decision tree
decision_treepreds <- predict(decision_tree, test)
head(decision_treepreds) #returns three probabilities, we want to be able to compare in a confusion matrix to the test data set for the classification column
head(test) #develop a function for decision_treepreds to match output to classification column of test data set

decision_treepreds <- as.data.frame(decision_treepreds)#Confirm a df, before proceeding to function

joiner <- function(x){
  if (x>.5){
    return('ckd')
  }else{
    return('notckd')
  }
}

decision_treepreds$classification <- sapply(decision_treepreds$ckd,joiner)
head(decision_treepreds) #now have a new column identifying as ckd or not

#Confusion Matrix for Single Decision Tree
table(decision_treepreds$class,test$classification) #88% accuracy

#Plot tree model
library(rpart.plot)
prp(decision_tree)

#Random Forest Implementation
library(randomForest)

#train random forest model
rf.model <- randomForest(classification ~ . , data = train,importance = TRUE)

#confusion matrix
rf.model$confusion #96% accuracy rate
rf.model$importance #utilize gini index to identify factors with highest mean decrease gini and low gini value, age and sg are highly prevalent
randomf_pred <- predict(rf.model,test)
table(randomf_pred,test$Private)


