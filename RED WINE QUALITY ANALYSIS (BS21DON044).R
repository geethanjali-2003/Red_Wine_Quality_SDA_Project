############################################
##                                        ##
## Name - Geethanjali Dhanish             ##
## SPJ ID - BS21DON044                    ##    
## Subject - Statistical Data Analysis    ##   
## Data Set - red_wine_quality.csv        ##
##                                        ##
############################################

# Reading the data 'red_wine_quality.csv' and assign to 'red_wine' data frame
red_wine = read.csv('red_wine_quality.csv')

# View first 5 rows of red_wine df
head(red_wine)

# Statistical Description of red_wine df
library('psych')
describe(red_wine)

attach(red_wine)

# Create correlation matrix for red_wine df
cor(red_wine)


# library that contains basic utility functions required to analyse data
library(caTools) 

# Split df into training and testing data
set.seed(123)
split = sample.split(red_wine$quality, SplitRatio = 0.70) 

train_data <- subset(red_wine, split==T) # Create training data for analysis (70%)
test_data <- subset(red_wine, split==F) # Create testing data for validation (30%)


# Create Model-1 using all 11 independent vars
mod1 = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
                    residual.sugar + chlorides + free.sulfur.dioxide +
                    total.sulfur.dioxide + density + pH + sulphates + alcohol)

# Summary statistics for Model-1
summary(mod1)

# Create Model-2 after removing all insignificant independent vars
mod2 = lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide +
                    total.sulfur.dioxide + pH + sulphates + alcohol)

# Summary statistics for Model-2
summary(mod2)

# Residual Analysis
pred = fitted(mod2) 
Res = residuals(mod2) 
plot(Res)
abline(0,0)
qqnorm(Res)
qqline(Res)

# Normality Check using Shapiro - Wilk test
shapiro.test(Res) 

# VIF Test for Multicollinearity
install.packages("car")
attach(red_wine)
library(car)
vif(mod1)
vif(mod2)

# Step AIC (Step-wise regression) to remove highly correlated vars
library(MASS) 
step = stepAIC(mod1, direction = "both")
summary(step)

# Four graphs for Model-2 
# 1- Residuals vs Fitted
# 2- Normal Q-Q
# 3- Scale - Location
# 4- Residuals vs Leverage
plot(mod2)

# Create ANOVA table on final model (Model-2) for hypothesis testing
anova(mod2)

# Prediction the values
pred = fitted(step)
res = residuals(step) 
ax = cbind(quality, pred, res)
mse = mean(res^2)
rmse = sqrt(mse)
mse
rmse

##### Exploratory Data Analysis (Understanding the data using Plots) #####

# Bar Plot to measure frequency of dependent var (quality)
counts <- table(red_wine$quality)
barplot(counts, main="Red Wine Quality PLot", ylab="Frequency",
        xlab="Quality of Wine", col = c("yellow","red","orange"))

# Box Plot to measure outliers in red_wine data set
boxplot(red_wine, data=red_wine, main="Red Wine Quality Data",
        col = c("yellow","red","orange"))

# Scatter Plot between positively correlated vars - alcohol and quality
plot(x = red_wine$alcohol, y = red_wine$quality,
     xlab = "alcohol",
     ylab = "quality",
     main = "Alcohol vs Quality")

############################### THE END ######################################

