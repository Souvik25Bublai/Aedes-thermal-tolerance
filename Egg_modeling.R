#HE - Hatched eggs
#UE - Unhatched eggs, 
#DE - Deformed Eggs
#UHE - (Unhatched+Deformed)eggs


#Load Packages and Datasheets
{library(AER)
  library(ecotox)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(Rmisc)
  library(RColorBrewer)
  library(emmeans)
  library(gam)
  library(mgcv)
  library(multcomp)
  library(multcompView)
  library(emmeans)
  library(car)
  library(carData)
  library(lmtest)
  library(zoo)
  library(lattice)
  library(patchwork)
  library(lme4)
  library(patchwork)
  library(corrplot)
  library(scales)
  library(ggpubr)
  library(tidyverse)
}

attach(Egg_modeling)
#add variables
mydata<-Egg_modeling
View(mydata)

mydata$Preference <- ifelse(mydata$Line=="PK10",-0.33436322,ifelse(mydata$Line=="Mindin", -0.477038214,
                                                                   ifelse(mydata$Line=="Kintampo", -0.581702248,
                                                                          ifelse(mydata$Line=="Kedougou",-0.522748506,
                                                                                 ifelse(mydata$Line=="Ngoye",0.693053879,
                                                                                        ifelse(mydata$Line=="Thies",0.396234544, -0.300140692))))))

mydata$Ancestry <- ifelse(mydata$Line=="PK10",0.001113444,ifelse(mydata$Line=="Mindin", 0.017289316,
                                                                 ifelse(mydata$Line=="Kintampo", 0.031685947,
                                                                        ifelse(mydata$Line=="Kedougou",0.008563421,
                                                                               ifelse(mydata$Line=="Ngoye",0.373815800,
                                                                                      ifelse(mydata$Line=="Thies",0.221672909, 	
                                                                                             0.067482211))))))
mydata$Density <- ifelse(mydata$Line=="PK10",12.29111819,ifelse(mydata$Line=="Mindin", 53.60547457,
                                                                ifelse(mydata$Line=="Kintampo", 37.22657519,
                                                                       ifelse(mydata$Line=="Kedougou",12.24971172,
                                                                              ifelse(mydata$Line=="Ngoye",232.6737489,
                                                                                     ifelse(mydata$Line=="Thies",421.1161584, 2264.553235))))))


mydata$bio1 <- ifelse(mydata$Line == "Ngoye", 27.23134388,
                      ifelse(mydata$Line == "Thies", 26.34446173,
                             ifelse(mydata$Line == "Mindin", 27.4308689,
                                    ifelse(mydata$Line == "Kedougou", 28.31359838,
                                           ifelse(mydata$Line == "PK10", 28.32982187,
                                                  ifelse(mydata$Line == "Kumasi", 25.82099722, 26.67891258)))))) 


mydata$bio8 <- ifelse(mydata$Line == "Ngoye", 28.16551315,
                      ifelse(mydata$Line == "Thies", 27.73808739,
                             ifelse(mydata$Line == "Mindin", 27.34876508,
                                    ifelse(mydata$Line == "Kedougou", 26.66543166,
                                           ifelse(mydata$Line == "PK10", 26.6977011,
                                                  ifelse(mydata$Line == "Kumasi", 26.31027781, 25.93740981)))))) 

mydata$bio9 <- ifelse(mydata$Line == "Ngoye", 26.44713228,
                      ifelse(mydata$Line == "Thies", 25.41942082,
                             ifelse(mydata$Line == "Mindin", 27.84519131,
                                    ifelse(mydata$Line == "Kedougou", 27.65728965,
                                           ifelse(mydata$Line == "PK10", 28.05133339,
                                                  ifelse(mydata$Line == "Kumasi", 26.31272221, 27.5328634)))))) 


mydata$bio10 <- ifelse(mydata$Line == "Ngoye", 28.71277243,
                       ifelse(mydata$Line == "Thies", 27.85747534,
                              ifelse(mydata$Line == "Mindin", 29.94342078,
                                     ifelse(mydata$Line == "Kedougou", 32.02756295,
                                            ifelse(mydata$Line == "PK10", 32.0382415,
                                                   ifelse(mydata$Line == "Kumasi", 27.15455554, 28.62548641)))))) 


mydata$bio11 <- ifelse(mydata$Line == "Ngoye", 24.75124868,
                       ifelse(mydata$Line == "Thies", 24.0032459,
                              ifelse(mydata$Line == "Mindin", 24.82581424,
                                     ifelse(mydata$Line == "Kedougou", 26.117388,
                                            ifelse(mydata$Line == "PK10", 26.11555175,
                                                   ifelse(mydata$Line == "Kumasi", 24.36176669, 24.92969395)))))) 



mydata$bio15 <- ifelse(mydata$Line == "Ngoye", 157.7607664,
                       ifelse(mydata$Line == "Thies", 159.7975303,
                              ifelse(mydata$Line == "Mindin", 140.4319175,
                                     ifelse(mydata$Line == "Kedougou", 124.4395322,
                                            ifelse(mydata$Line == "PK10", 124.9359484,
                                                   ifelse(mydata$Line == "Kumasi", 53.98502001, 65.8939298)))))) 


mydata$bio16 <- ifelse(mydata$Line == "Ngoye", 442.0952381,
                       ifelse(mydata$Line == "Thies", 416.5409836,
                              ifelse(mydata$Line == "Mindin", 494.3442623,
                                     ifelse(mydata$Line == "Kedougou", 810.3442623,
                                            ifelse(mydata$Line == "PK10", 803.5517241,
                                                   ifelse(mydata$Line == "Kumasi", 511.8833333, 444.2295082)))))) 

mydata$bio17 <- ifelse(mydata$Line == "Ngoye", 1.206349206,
                       ifelse(mydata$Line == "Thies", 0.786885246,
                              ifelse(mydata$Line == "Mindin", 0.081967213,
                                     ifelse(mydata$Line == "Kedougou", 2.016393443,
                                            ifelse(mydata$Line == "PK10", 2.224137931,
                                                   ifelse(mydata$Line == "Kumasi", 99.01666667, 35.50819672))))))


mydata$bio18 <- ifelse(mydata$Line == "Ngoye", 135.7142857,
                       ifelse(mydata$Line == "Thies", 325.0819672,
                              ifelse(mydata$Line == "Mindin", 59.95081967,
                                     ifelse(mydata$Line == "Kedougou", 54.75409836,
                                            ifelse(mydata$Line == "PK10", 52.5,
                                                   ifelse(mydata$Line == "Kumasi", 332.2833333, 224.4098361)))))) 

mydata$bio19 <- ifelse(mydata$Line == "Ngoye", 3,
                       ifelse(mydata$Line == "Thies", 3.081967213,
                              ifelse(mydata$Line == "Mindin", 2.049180328,
                                     ifelse(mydata$Line == "Kedougou", 7.31147541,
                                            ifelse(mydata$Line == "PK10", 7.344827586,
                                                   ifelse(mydata$Line == "Kumasi", 367.75, 429.9672131))))))
View(mydata)
dm<-mydata
str(dm)

###Looking at the data as binomial-Success / failure
PropSurvival<-HE/(HE+UHE)
plot(PropSurvival~Temperature)
Y<-cbind(HE,UHE)
Y
model <- glm(Y ~ Temperature, family = binomial)
summary(model)
plot(Temperature, PropSurvival, xlab = "Temperature", ylab = "Proportion Survival")
curve(predict(model, data.frame(Temperature = x), type = "response"), add = TRUE, col = "red")
plot(residuals(model, type = "deviance"))

library(ResourceSelection)
hoslem.test(model$y, fitted(model))
predicted <- predict(model, type = "response")
plot(PropSurvival, predicted, xlab = "Observed", ylab = "Predicted")
abline(0, 1, col = "blue")


ecology_data <- data.frame(
  Preference = dm$Preference,
  Ancestry = dm$Ancestry,
  Density = dm$Density,
  bio1 = dm$bio1,
  bio8 = dm$bio8,
  bio9 = dm$bio9,
  bio10 = dm$bio10,
  bio11 = dm$bio11,
  bio15 = dm$bio15,
  bio18 = dm$bio18,
  bio19 = dm$bio19
)
cor_matrix <- cor(ecology_data)
corrplot(cor_matrix, method = "circle")
#circle
#number
#square
#shade
cor(ecology_data)
climate_data <- data.frame(
  bio1 = dm$bio1,
  bio8 = dm$bio8,
  bio9 = dm$bio9,
  bio10 = dm$bio10,
  bio11 = dm$bio11,
  bio15 = dm$bio15,
  bio18 = dm$bio18
)
cor(climate_data)

# Create the data frame
ecology_data2 <- data.frame(
  Preference = dm$Preference,
  Ancestry = dm$Ancestry,
  Density = dm$Density,
  `Annual mean temperature` = dm$bio1,
  `Mean temperature of wettest quarter` = dm$bio8,
  `Mean temperature of driest quarter` = dm$bio9,
  `Mean temperature of warmest quarter` = dm$bio10,
  `Mean temperature of coldest quarter` = dm$bio11,
  `Precipitation seasonality (coefficient of variation)` = dm$bio15,
  `Precipitation of Driest Quarter` = dm$bio17,
  `Precipitation of warmest quarter` = dm$bio18
)

# Compute the correlation matrix
cor_matrix2 <- cor(ecology_data2)

# Custom column and row names (no ".")
clean_names <- c(
  "Preference", "Ancestry", "Density",
  "Annual mean temperature", "Mean temperature of wettest quarter",
  "Mean temperature of driest quarter", "Mean temperature of warmest quarter",
  "Mean temperature of coldest quarter", "Precipitation seasonality",
  "Precipitation of Driest Quarter", "Precipitation of warmest quarter"
)

# Set clean names to the correlation matrix
colnames(cor_matrix2) <- clean_names
rownames(cor_matrix2) <- clean_names

# Plot the correlation matrix with a title
corrplot(cor_matrix2, method = "circle", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
title("Supplementary Figure 4: Correlation matrix of key predictor variables", 
      line = 3, cex.main = 1.2, font.main = 2)



# Select the features 
selected_features <- c("Temperature", "Stress", "Ancestry", "Density", "bio1", "bio9", "bio15")
# Fit the logistic regression model
model <- lm(Y ~ ., data = dm[, selected_features], family = binomial)
summary(model)

# Define the predictors
selected_features <- c("Temperature", "Stress", "Ancestry", "Density", "bio1", "bio9", "bio15")
# Initialize a list to store results
r_squared_values <- list()
# Loop through each predictor
for (feature in selected_features) {
  # Dynamically create the formula Y ~ feature
  formula <- reformulate(feature, response = "UHE")  # Assuming UHE is the response variable
  
  # Fit the linear model
  model <- lm(formula, data = dm)
  
  # Extract R-squared
  r_squared <- summary(model)$r.squared
  
  # Store the R-squared value
  r_squared_values[[feature]] <- r_squared
}
# Convert to a data frame for readability
r_squared_values_df <- data.frame(
  Predictor = names(r_squared_values),
  R_squared = unlist(r_squared_values)
)
# Print the results
print(r_squared_values_df)



xlibrary(ResourceSelection)
hoslem.test(model$y, fitted(model))
predicted <- predict(model, type = "response") > 0.5
confusion_matrix <- table(Predicted = predicted, Actual = Y)
print(confusion_matrix)
library(pROC)
roc_curve <- roc(dm$Y, fitted(model))
plot(roc_curve)
auc(roc_curve)
plot(residuals(model, type = "deviance"))


# Assuming HE and UHE are the columns in your data frame (dm)
y <- cbind(dm$HE, dm$UHE)  # Create a matrix where the first column is HE (success) and the second column is UHE (failure)
library(glmnet)

# Assuming selected_features is defined and corresponds to the predictor columns
x <- as.matrix(dm[, selected_features])

# Fit the Lasso model using cv.glmnet
model_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Plot the results
plot(model_lasso)
optimal_lambda <- model_lasso$lambda.min  # The optimal lambda value
optimal_lambda
final_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = optimal_lambda)
final_model
coefficients(final_model)

# Step 1: Create the 'Y' variable as a two-column matrix (success/failure)
dm$Y <- cbind(dm$HE, dm$UHE)  # Assuming 'HE' is the success and 'UHE' is the failure

# Check if 'Y' was created properly
head(dm$Y)

# Step 2: Split the data into training and test sets
set.seed(123)  # Set a random seed for reproducibility
train_index <- sample(1:nrow(dm), size = 0.7 * nrow(dm))
train_data <- dm[train_index, ]
test_data <- dm[-train_index, ]

# Step 3: Select the features for both training and test sets
selected_features <- c("Temperature", "Stress", "Ancestry", "Density", "bio1", "bio9", "bio15")

# Create the feature matrix and target variable for training
x_train <- as.matrix(train_data[, selected_features])
y_train <- train_data$Y

# Create the feature matrix and target variable for testing
x_test <- as.matrix(test_data[, selected_features])
y_test <- test_data$Y

# Step 4: Fit the final Lasso model using glmnet
library(glmnet)
model_lasso <- glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Step 5: Predict probabilities on the test set (getting the probabilities of the positive class, i.e., class 1)
predicted_probs <- predict(model_lasso, newx = x_test, type = "response")

# Extract the actual class labels from y_test (binary: 0 or 1)
y_test_labels <- ifelse(y_test[, 2] > 0, 1, 0)  # Assuming '1' is the positive class (success)

# Step 6: Generate the ROC curve and calculate AUC
library(pROC)
roc_curve <- roc(y_test_labels, predicted_probs[, 2])  # predicted_probs[, 2] is the probability of class 1
plot(roc_curve)  # Plot the ROC curve
print(auc(roc_curve))  # Print the AUC value


str(dm)

summary(lm(Y ~ Density, data = dm))
