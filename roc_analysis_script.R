
# Load necessary libraries
library(pROC)
library(randomForest)

# Example dataset (using built-in R dataset for illustration)
data(PimaIndiansDiabetes2, package = "mlbench")  # Load example dataset
df <- na.omit(PimaIndiansDiabetes2)  # Remove missing values
df$diabetes_binary <- ifelse(df$diabetes == "pos", 1, 0)  # Convert target to binary

# Fit a logistic regression model
glm_fit <- glm(diabetes_binary ~ glucose, data = df, family = binomial)

# Scatter plot with logistic regression fit
plot(df$glucose, df$diabetes_binary, 
     xlab = "Glucose", 
     ylab = "Diabetes Binary", 
     main = "Logistic Regression Fit", 
     col = "red", 
     pch = 16)

lines(sort(df$glucose), 
      glm_fit$fitted.values[order(df$glucose)], 
      col = "blue", 
      lwd = 2)

# Plot the ROC curve for logistic regression with 1 - specificity
roc_curve_glm <- roc(df$diabetes_binary, glm_fit$fitted.values, 
                     plot = TRUE, 
                     legacy.axes = TRUE,  # 1 - specificity on the x-axis
                     xlab = "False Positive Rate (1 - Specificity)", 
                     ylab = "True Positive Rate (Sensitivity)", 
                     col = "#377eb8", 
                     lwd = 4, 
                     print.auc = TRUE)

# Fit a random forest model for comparison
library(randomForest)
rf_model <- randomForest(factor(diabetes_binary) ~ glucose, data = df)

# Plot the ROC curve for the random forest with 1 - specificity
roc_curve_rf <- roc(df$diabetes_binary, rf_model$votes[,2], 
                    plot = TRUE, 
                    legacy.axes = TRUE,  # 1 - specificity on the x-axis
                    xlab = "False Positive Rate (1 - Specificity)", 
                    ylab = "True Positive Rate (Sensitivity)", 
                    col = "#4daf4a", 
                    lwd = 4, 
                    print.auc = TRUE)

# Overlay both ROC curves for comparison
roc(df$diabetes_binary, glm_fit$fitted.values, 
    plot = TRUE, 
    legacy.axes = TRUE, 
    xlab = "False Positive Rate (1 - Specificity)", 
    ylab = "True Positive Rate (Sensitivity)", 
    col = "#377eb8", 
    lwd = 4, 
    print.auc = TRUE)

plot.roc(df$diabetes_binary, rf_model$votes[,2], 
         percent = FALSE, 
         col = "#4daf4a", 
         lwd = 4, 
         print.auc = TRUE, 
         add = TRUE, 
         print.auc.y = 0.4)

legend("bottomright", 
       legend = c("Logistic Regression", "Random Forest"), 
       col = c("#377eb8", "#4daf4a"), 
       lwd = 4)
