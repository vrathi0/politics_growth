

rm(list=ls())
source("code/functions.R")


# USING THIS TO WORK ON VARIOUS ML ALGO TO SEE IF FEATURES PREDICT DEPUTIZATION

### FUNCTION

rem_na <- function(data, threshold = 0.05) {
  # Calculate the proportion of missing values in each column
  missing_proportion <- colMeans(is.na(data))
  
  # Identify columns where the missing proportion is greater than the threshold
  columns_to_keep <- missing_proportion <= threshold
  
  # Subset the data to retain only those columns
  cleaned_data <- data[, columns_to_keep]
  
  return(cleaned_data)
}



# 1 READING ---------------------------------------------------------------


ID_DF=qread("data/features/ias_deput_features.qs")


# 2 LASSO ---------------------------------------------------------------


# REMOVING THE NA ROWS TO BEGIN WITH

# Filter rows where Y14 and Y15 are not NA
non_na_df1 <- ID_DF %>% filter(!is.na(Y14))
non_na_df2 <- ID_DF %>% filter(!is.na(Y15))

# Dynamically get the last two columns
last_col_index <- ncol(ID_DF)
second_last_col_index <- last_col_index - 1

# Create the matrices excluding the first and last two columns
X1 <- as.matrix(non_na_df1[ , -c(1, second_last_col_index, last_col_index)])  # 66 features
X2 <- as.matrix(non_na_df2[ , -c(1, second_last_col_index, last_col_index)])  # 66 features

# Extract the Y14 and Y15 values
Y14 <- non_na_df1$Y14
Y15 <- non_na_df2$Y15




X_clean=rem_na(X1, threshold = 0.05)
complete_cases <- complete.cases(X_clean)
X_clean <- X_clean[complete_cases, ]
Y_clean <- Y14[complete_cases]




# Ensure the outcome variable is coded as factor
Y_clean<- as.factor(Y_clean)


# LASSO

# Set seed for reproducibility
set.seed(42)

# Fit LASSO using cross-validation to select the best lambda (regularization parameter)
cv.lasso <- cv.glmnet(
  X_clean, Y_clean, 
  family = "binomial",          # Logistic regression for binary outcome
  alpha = 1,                    # LASSO regularization (L1 penalty)
  type.measure = "class",       # Measure classification error
  nfolds = 10                   # 10-fold cross-validation
)

# Plot the cross-validation results to inspect lambda
plot(cv.lasso)

# Best lambda (minimum cross-validation error)
best_lambda <- cv.lasso$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Fit the final LASSO model using the best lambda
lasso_model <- glmnet(
  X_clean, Y_clean, 
  family = "binomial", 
  alpha = 1, 
  lambda = best_lambda
)


## FEATURE IMPORTANCE

# Extract the non-zero coefficients (important features)
coef_lasso <- coef(lasso_model)

# Convert to a data frame and remove intercept
coef_df <- as.data.frame(as.matrix(coef_lasso))
coef_df <- coef_df[coef_df != 0, , drop = FALSE]
coef_df <- tibble::rownames_to_column(coef_df, "Feature")
colnames(coef_df)[2] <- "Coefficient"

# View the top features by absolute value of coefficient
coef_df <- coef_df %>%
  mutate(abs_coef = abs(Coefficient)) %>%
  arrange(desc(abs_coef))

write.xlsx(coef_df, "data/interm/LASSO_coeff_with_budget.xlsx")

print(coef_df)


### Getting some R2 measure

# Assuming you have your glmnet model for LASSO

# Calculate log-likelihoods
null_model <- glm(Y_clean ~ 1, family = binomial)
log_lik_null <- logLik(null_model)
# Step 1: Get the predicted probabilities for class 1 (assuming a binary outcome)
predicted_probabilities <- predict(lasso_model, newx = X_clean, 
                                   type = "response")

# Step 2: Calculate the log-likelihood manually
# For logistic regression, the log-likelihood is the sum of the log of predicted probabilities for the actual outcome
log_lik_model <- sum(as.numeric(Y_clean) * log(predicted_probabilities) + 
                       (1 - as.numeric(Y_clean)) * log(1 - predicted_probabilities), na.rm=T)

print(log_lik_model)

# McFadden's pseudo-R²
pseudo_r2 <- 1 - (log_lik_model / log_lik_null)
print(pseudo_r2) # Its 0.49 which is considered good for logit?


# Calculate AUC
roc_curve <- roc(Y_clean, predicted_probabilities)
auc_value <- auc(roc_curve)
print(auc_value)

## Prediction accuracy

# Confusion matrix and accuracy
# Assuming predicted_probabilities are the probabilities of class 1
# Convert probabilities to class predictions using a threshold of 0.5
class_predictions <- ifelse(predicted_probabilities > 0.8, 1, 0)

# Now use the class predictions in confusionMatrix
confusionMatrix(as.factor(class_predictions), as.factor(Y_clean))







