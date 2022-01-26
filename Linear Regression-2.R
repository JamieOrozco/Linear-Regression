# Package forecast is required to evaluate performance
# If it is not installed you also need to use the command:
install.packages("forecast")
install.packages("Metrics")

library(forecast)
library(Metrics)

# Load file
empData.df <- read.csv("emp_data.csv", fileEncoding = "UTF-8-BOM") 
# Note: fileEncoding needed for Windows based computers

# Structure of the data frame
str(empData.df) 

# Convert jobcat to a factor and assign labels
empData.df$jobcat <- factor(empData.df$jobcat, levels = c(1, 2, 3), 
                            labels = c("Low", "Moderate", "High"))

# Create a new feature that is 1 if gender is 'f' and zero otherwise
empData.df$female <- factor(ifelse(empData.df$gender == "f", 1, 0))

str(empData.df) 


# Extracts year from bdate and places it in a new column called byear
byear <- as.Date(empData.df$bdate, '%m/%d/%Y')
empData.df$byear <- as.numeric(format (byear, '%Y'))

set.seed(123)  # set seed for reproducing the partition

# Partition data into training (70%) and validating (30%)
# sample(range, number)
# sample(c(1:474), 332)

dim(empData.df)[1]  #Number of rows
dim(empData.df)[1]*0.7 #Number of rows in the training dataframe

train.index <- sample(c(1:dim(empData.df)[1]), dim(empData.df)[1]*0.7) 
train.df <- empData.df[train.index, 1:12]
valid.df <- empData.df[-train.index, 1:12]

# Linear Regression Model 
empData.lm <- lm(salary ~ female + educ + byear + 
                 jobcat + salbegin, data = train.df)

options(scipen = 999, digits = 3)

# Summary statistics of the LM model
summary(empData.lm)

# Coefficients and their confidence intervals
coef <- coef(empData.lm)
confint <- confint(empData.lm)
data.frame(Coefficients = coef, CI = confint)

# Produces predictions based on the parameter developed in the regression model
pred_t <- predict(empData.lm, train.df) # Predictions based on training data
pred_v <- predict(empData.lm, valid.df) # Predictions based on validating data

# Computes accuracy measures.
# accuracy(predicte_values, actual_values)
accuracy(pred_t, train.df$salary)
accuracy(pred_v, valid.df$salary)

rmse(train.df$salary, pred_t) #You would not normally do this command
rmse(valid.df$salary, pred_v)

# Histogram of the residuals (difference between the predicted and actual data)
residual_t <- train.df$salary - pred_t
hist(residual_t, breaks = 40, xlab = "Residuals", main = "Training Dataset")

residual_v <- valid.df$salary - pred_v
hist(residual_v, breaks = 40, xlab = "Residuals", main = "Validation Dataset")



