##################################################
# Title: FINAL EXAM
##################################################
##################################################
# Written by {MOHAMMED ADEEN, SHAIK}
# Student ID: 8969152
##################################################

# Load Libraries:
library(ggplot2)
library("lattice")
library("pastecs")
library("corrgram")

# Set work directory
setwd("C:/Users/Adeen/Desktop")

# Load the data from the file
train_data<- read.csv(file = "PROG8435-24W-Final_train.csv", header = TRUE)
test_data<- read.csv(file = "PROG8435-24W-Final_test.csv", header = TRUE)

train_data <- as.data.frame(unclass(train_data), stringsAsFactors = TRUE)
test_data <- as.data.frame(unclass(test_data), stringsAsFactors = TRUE)

# Overview: Display the first few rows of the data
head(train_data,3)

#Data Preparation:

#Dimensionality Reduction

summary(train_data)

# No null Value detected
# ID is useful to identify unique records but don't hold value in our analysis, so we remove them

train_data<- train_data[-c(1)] 

stat.desc(train_data)


train_data$Surgical.Medical <- as.factor(train_data$Surgical.Medical)
train_data$Type <- as.factor(train_data$Type)
train_data$Marital <- as.factor(train_data$Marital)
train_data$FSA <- as.factor(train_data$FSA)

test_data$Surgical.Medical <- as.factor(test_data$Surgical.Medical)
test_data$Type <- as.factor(test_data$Type)
test_data$Marital <- as.factor(test_data$Marital)
test_data$FSA <- as.factor(test_data$FSA)

# Convert factor columns to numeric
train_data <- train_data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)) - 1))


test_data$Type <- as.numeric(as.character(test_data$Type))


# Preliminary Analysis:



# Histogram for Age
ggplot(train_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +  # Adjust binwidth accordingly
  labs(title = "Distribution of Age", x = "Age (Years)", y = "Frequency")

# Calculate the mean age excluding the invalid value (-50)
mean_age <- mean(train_data$Age[train_data$Age > 0], na.rm = TRUE)

# Replace any invalid age (-50) with the calculated mean age
train_data$Age[train_data$Age == -50] <- mean_age


# Histogram for Severity
ggplot(train_data, aes(x = Serverity)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +  # Adjust binwidth accordingly
  labs(title = "Distribution of Serverity", x = "Serverity ", y = "Frequency")

# Histogram for Anxiety
ggplot(train_data, aes(x = Anxiety)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +  # Adjust binwidth accordingly
  labs(title = "Distribution of Anxiety", x = "Anxiety ", y = "Frequency")

# Boxplots

# Loops through each variable to plot boxplots, focusing on numeric variables to identify outliers visually.
# Outliers can significantly affect regression model performance and may need to be addressed.
for (i in 1:ncol(train_data)) {
  if (is.numeric(train_data[,i])) {
    boxplot(train_data[,i], main=names(train_data)[i], xlab="", horizontal=TRUE)
  }
}


# Correlations

numeric_data <- train_data[sapply(train_data, is.numeric)]
data_cr <- cor(numeric_data, method="spearman")
round(data_cr, 2)

# Model Building:


# 1. Simple Linear Regression with `Pol` as dependent and `score` as independent
model1 <- lm(Satisfaction ~ Age , data = train_data)
summary(model1)

# Scatter plot with regression line for model1
plot1 <- ggplot(train_data, aes(x = Age, y = Satisfaction)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot with Regression Line (Satisfaction ~ Age)",
       x = "Age", y = "Satisfaction")
print(plot1)

# 2. Simple Linear Regression with `Pol` as dependent and `scr` as independent
model2 <- lm(Satisfaction ~ Serverity, data = train_data)
summary(model2)

# Scatter plot with regression line for model2
plot2 <- ggplot(train_data, aes(x = Serverity , y = Satisfaction)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Regression Line (Satisfaction ~ Serverity)",
       x = "Serverity", y = "Satisfaction")
print(plot2)

# 3. Compare the models
summary(model1)
summary(model2)

# Multivariate regression


# Full model ( Creates a linear model using all available predictors, serving as a baseline for comparison.)
full.model = lm(Satisfaction ~ . , data=train_data, na.action=na.omit)
summary(full.model)

# Predicts using the full model and calculates RMSE, a measure of prediction accuracy, for the data.
# Lower RMSE values indicate better model performance.
pred <- predict(full.model, newdata=train_data)
RMSE_trn_full <- sqrt(mean((train_data$Satisfaction - pred)^2))

# Calculate and display percentage error for better context of the RMSE value.
percent_error_full <- (RMSE_trn_full / mean(train_data$Satisfaction)) * 100
round(RMSE_trn_full,2)
round(percent_error_full,2)


# back model (Builds a model using backward elimination to remove predictors that do not significantly contribute to the model)
back.model = step(full.model, direction="backward", details=TRUE)
summary(back.model)

# Predicts and calculates RMSE for the backward model.
pred <- predict(back.model, newdata=train_data)
RMSE_trn_back <- sqrt(mean((train_data$Satisfaction - pred)^2))

# Calculate and display percentage error for the backward model.
percent_error_back <- (RMSE_trn_back / mean(train_data$Satisfaction)) * 100
round(RMSE_trn_back,2)
round(percent_error_back,2)

#model Comparison
summary(full.model)
summary(back.model)

# Evaluating models:


# 1.mean of zero

# Storing residuals from each model allows for further analysis of model fit,
# including checking the normality of residuals, which is a key assumption in linear regression.

full.res <- residuals(full.model)
back.res <- residuals(back.model)

mean(full.res)
mean(back.res)

# 2. Constant Variance

plot(fitted(full.model), residuals(full.model), xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(back.model), residuals(full.model), xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

# 3. normal Distribution


# Q-Q Plot
qqnorm(full.res)
qqline(full.res)

qqnorm(back.res)
qqline(back.res)

# Shapiro Test
shapiro.test(full.res)
shapiro.test(back.res)


# FINAL PREDICTION

# Predict satisfaction using the backward model on the test data
pred <- predict(back.model, newdata=test_data)
test_data$pred_Satisfaction_back <- predict(back.model, newdata=test_data)

test_fin <- cbind(test_data,pred)
write.csv(test_fin," PROG8435-24W-Final-MS.txt ")
