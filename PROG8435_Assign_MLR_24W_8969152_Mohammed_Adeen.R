##################################################
# Title: Multivariate Linear Regression (MLR)
##################################################
##################################################
# Written by {MOHAMMED ADEEN, SHAIK}
##################################################

# Load Libraries:
library(ggplot2)
library("lattice")
library("pastecs")
library("corrgram")

#set work directory
setwd("C:/Users/Adeen/Desktop/R scripts/")

# Load the data from the file
data <- read.csv(file = "Pol_test.csv", header = TRUE)

# Overview: Display the first few rows of the data
stat.desc(data)

# Task 1: Dimensionality Reduction

head(data,3)
data<- data[-c(1)] # ID is useful to identify unique records but don't hold value in our analysis

# 1. Remove columns with missing values
summary(data) # Summarize the data to inspect missing values
data <- data[, !(names(data) %in% c("time2"))]
head(data,3) # Display the first 3 rows to check the dataset after removal

# 2. Low variance Filter
stat.desc(data)
numeric_cols <- sapply(data, is.numeric)  # Identify numeric columns
iqr_values <- apply(data[, numeric_cols], 2, IQR)  # Compute IQR

# Print the IQR values
print(iqr_values)

# remove the column with the lowest Variance (housing)
data <- data[, !(names(data) %in% c("housing"))]

# 3. High Correlation filter

numeric_data <- data[sapply(data, is.numeric)]
cor(numeric_data, method="spearman")
data <- data[, !(names(data) %in% c("time3"))]

# Task 2: Data Transformation

# Performs and displays a descriptive statistical summary of the dataset,
# providing insights into the central tendency, dispersion, and shape of each variable's distribution.
round(stat.desc(data),2)

# Sets up a graphical layout to plot histograms in a grid, enabling visual inspection of each variable's distribution.
# This helps identify skewness, outliers, and the overall distribution shape, guiding preprocessing decisions.
par(mfrow=c(3,2))    

# Loops through each variable in the dataset, plotting histograms for numeric variables.
# This visual analysis is essential for understanding the distribution of each variable.
for (i in 1:ncol(data)) {
  if (is.numeric(data[,i])) {
    hist(data[,i], main=names(data)[i], xlab="")
  }
}

# Resets plotting layout to default.
par(mfrow=c(1,1))

##################################################
# Task 3: Outliers
##################################################

# Configures the graphical layout for boxplot visualization, useful for detecting outliers.
# Boxplots provide a visual summary of the central tendency, dispersion, and outliers.
par(mfrow=c(3,4))

# Loops through each variable to plot boxplots, focusing on numeric variables to identify outliers visually.
# Outliers can significantly affect regression model performance and may need to be addressed.
for (i in 1:ncol(data)) {
  if (is.numeric(data[,i])) {
    boxplot(data[,i], main=names(data)[i], xlab="", horizontal=TRUE)
  }
}

# Resets plotting layout to default.
par(mfrow=c(1,1))

# Assuming 'other' is a numeric vector in the dataset
upper_cap <- quantile(data$other, 0.95)  # 95th percentile

data$other <- ifelse(data$other > upper_cap, upper_cap, data$other)

# Set the capping thresholds
lower_cap_score <- quantile(data$score, 0.05)  # 5th percentile for lower cap
upper_cap_score <- quantile(data$score, 0.95)  # 95th percentile for upper cap

lower_cap_scr <- quantile(data$scr, 0.05)  # 5th percentile for lower cap
upper_cap_scr <- quantile(data$scr, 0.95)  # 95th percentile for upper cap

# Apply capping for the 'score' column
data$score <- ifelse(data$score < lower_cap_score, lower_cap_score, data$score)
data$score <- ifelse(data$score > upper_cap_score, upper_cap_score, data$score)

# Apply capping for the 'scr' column
data$scr <- ifelse(data$scr < lower_cap_scr, lower_cap_scr, data$scr)
data$scr <- ifelse(data$scr > upper_cap_scr, upper_cap_scr, data$scr)

# Define a tolerance level
tolerance <- 10  # You can adjust this value

# Remove rows where n.child is within the range 200 ± tolerance
data <- subset(data, !(n.child >= 200 - tolerance & n.child <= 200 + tolerance))


par(mfrow=c(3,4))

# Loops through each variable to plot boxplots, focusing on numeric variables to identify outliers visually.
# Outliers can significantly affect regression model performance and may need to be addressed.
for (i in 1:ncol(data)) {
  if (is.numeric(data[,i])) {
    boxplot(data[,i], main=names(data)[i], xlab="", horizontal=TRUE)
  }
}

# Resets plotting layout to default.
par(mfrow=c(1,1))


##################################################
# Task 4: Exploratory Analysis  (Correlation)
##################################################

# Visualizes the correlation matrix using corrgram, providing insights into the relationship between variables.
# Identifying highly correlated variables is important for model selection and understanding variable interactions.
corrgram(data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations among Variables")

# Calculates and rounds the Spearman correlation coefficients, offering a numerical view of variable relationships.
# Spearman correlation is used here as it can handle non-linear relationships and is robust to non-normal distributions.
data_cr <- cor(numeric_data, method="spearman")
round(data_cr, 2)

# Task 5: 

# 1. Simple Linear Regression with `Pol` as dependent and `score` as independent
model1 <- lm(Pol ~ score, data = data)
summary(model1)

# Scatter plot with regression line for model1
plot1 <- ggplot(data, aes(x = score, y = Pol)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot with Regression Line (Pol ~ score)",
       x = "score", y = "Pol")
print(plot1)

# 2. Simple Linear Regression with `Pol` as dependent and `scr` as independent
model2 <- lm(Pol ~ scr, data = data)
summary(model2)

# Scatter plot with regression line for model2
plot2 <- ggplot(data, aes(x = scr, y = Pol)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Regression Line (Pol ~ scr)",
       x = "scr", y = "Pol")
print(plot2)

# 3. Compare the models
summary(model1)
summary(model2)

# Compare R-squared values
cat("Model 1 (Pol ~ score) R-squared:", summary(model1)$r.squared, "\n")
cat("Model 2 (Pol ~ scr) R-squared:", summary(model2)$r.squared, "\n")

# Compare Adjusted R-squared values
cat("Model 1 (Pol ~ score) Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("Model 2 (Pol ~ scr) Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")

##################################################
# Task 6: Model Development - Multivariate
##################################################

# Full model ( Creates a linear model using all available predictors, serving as a baseline for comparison.)
full.model = lm(Pol ~ . , data=data, na.action=na.omit)
summary(full.model)

# Predicts using the full model and calculates RMSE, a measure of prediction accuracy, for the data.
# Lower RMSE values indicate better model performance.
pred <- predict(full.model, newdata=data)
RMSE_trn_full <- sqrt(mean((data$Pol - pred)^2))

# Calculate and display percentage error for better context of the RMSE value.
percent_error_full <- (RMSE_trn_full / mean(data$Pol)) * 100
round(RMSE_trn_full,2)
round(percent_error_full,2)

# back model (Builds a model using backward elimination to remove predictors that do not significantly contribute to the model)
back.model = step(full.model, direction="backward", details=TRUE)
summary(back.model)

# Predicts and calculates RMSE for the backward model.
pred <- predict(back.model, newdata=data)
RMSE_trn_back <- sqrt(mean((data$Pol - pred)^2))
# Calculate and display percentage error for the backward model.
percent_error_back <- (RMSE_trn_back / mean(data$Pol)) * 100
round(RMSE_trn_back,2)
round(percent_error_back,2)


summary(full.model)
summary(back.model)

# Task 7 Model Evaluation:

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

##############################    THANK YOU      ###############################