# Load Required Libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(rpart) #decision trees
library(pROC) #ROC/AUC
library(gains) #lift charts
library(e1071)

# Exploratory Data Analysis (Teammate A)
telco <- read.csv("Telco-Customer-Churn.csv", stringsAsFactors = TRUE)

# Convert TotalCharges to numeric
telco$TotalCharges <- as.numeric(as.character(telco$TotalCharges))
telco <- telco[!is.na(telco$TotalCharges), ]

# Remove customerID
telco <- telco %>% select(-customerID)

# View structure and summary
str(telco)
summary(telco)

# Histograms for numeric variables
numeric_vars <- telco %>% select_if(is.numeric)
for (col in names(numeric_vars)) {
  print(ggplot(telco, aes_string(x = col)) +
          geom_histogram(fill = "skyblue", color = "black", bins = 30) +
          labs(title = paste("Histogram of", col), x = col, y = "Frequency"))
}

# Boxplots for numeric variables
for (col in names(numeric_vars)) {
  print(ggplot(telco, aes_string(y = col)) +
          geom_boxplot(fill = "orange") +
          labs(title = paste("Boxplot of", col), y = col))
}

# Correlation matrix
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "circle", type = "upper")

# Bar plots for categorical variables vs Churn
cat_vars <- telco %>% select_if(is.factor) %>% select(-Churn)
for (col in names(cat_vars)) {
  print(ggplot(telco, aes_string(x = col, fill = "Churn")) +
          geom_bar(position = "fill") +
          scale_y_continuous(labels = scales::percent) +
          labs(title = paste("Churn Rate by", col), y = "Percentage", x = col))
}

# Pairwise plots
gg_pairs <- ggpairs(telco, columns = c("tenure", "MonthlyCharges", "TotalCharges", "Churn"), aes(color = Churn))
print(gg_pairs)

# Data Cleaning & Feature Engineering (Teammate B)
# Convert 'SeniorCitizen' to factor
telco$SeniorCitizen <- factor(telco$SeniorCitizen, labels = c("No", "Yes"))

# Convert Yes/No columns to factors
yn_cols <- c("Partner", "Dependents", "PhoneService", "PaperlessBilling",
             "MultipleLines", "OnlineSecurity", "OnlineBackup", 
             "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")
telco[yn_cols] <- lapply(telco[yn_cols], function(x) as.factor(as.character(x)))

# Convert other categorical fields as factors
telco$Contract <- as.factor(telco$Contract)
telco$PaymentMethod <- as.factor(telco$PaymentMethod)
telco$InternetService <- as.factor(telco$InternetService)

# Remove rows with missing values again (if any)
telco <- na.omit(telco)

# Remove duplicate rows
telco <- telco[!duplicated(telco), ]

# Create tenure groups
telco$TenureGroup <- cut(telco$tenure, breaks = c(0, 12, 24, 48, 72), labels = c("0–12", "13–24", "25–48", "49–72"), right = FALSE)

# Confirm factor levels
str(telco)
sapply(telco, function(x) if(is.factor(x)) levels(x) else NULL)

# Save cleaned data
write.csv(telco, "Telco_clean.csv", row.names = FALSE)

# Data Partitioning (Teammate C)
#Converts Churn into binary: Yes → 1, No → 0.
telco$Churn_Encoded <- ifelse(telco$Churn == "Yes", 1, 0)

set.seed(100)
folds <- list()
yes.idx <- sample(which(telco$Churn_Encoded == 1))
no.idx  <- sample(which(telco$Churn_Encoded == 0))
yes.folds <- split(yes.idx, cut(seq_along(yes.idx), 10, labels = FALSE))
no.folds  <- split(no.idx, cut(seq_along(no.idx), 10, labels = FALSE))
#Separately shuffles churned and non-churned samples and splits them into 10 groups
for (i in 1:10) {
  folds[[i]] <- c(yes.folds[[i]], no.folds[[i]])
}

# Model Training & Evaluation (Teammate D)
# Function to train and evaluate models
train_and_evaluate <- function(data, folds, models = c("Decision Tree", "Logistic Regression"), target_var = "Churn_Encoded") {
  # Empty dataframe to store performance results from 10-fold cross-validation.
  results <- data.frame(
    Fold = integer(),
    Model = character(),
    Accuracy = numeric(),
    Precision = numeric(),
    Recall = numeric(),
    Specificity = numeric(),
    FDR = numeric(),
    FOR = numeric(),
    AUC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # List to store confusion matrices for each fold and model
  all_confusion_matrices <- list()
  
  # Loop Over 10 Folds
  for (i in 1:length(folds)) {
    test.idx <- folds[[i]]
    train.idx <- setdiff(1:nrow(data), test.idx)
    
    train.set <- data[train.idx, ]
    test.set <- data[test.idx, ]
    
    #Decision Tree Model Training
    if ("Decision Tree" %in% models) {
      dt_model <- rpart(as.formula(paste(target_var, "~ . -Churn")), data = train.set, method = "class")
      dt_prob <- predict(dt_model, test.set, type = "prob")[, 2]
      dt_pred <- ifelse(dt_prob >= 0.5, 1, 0)
      
      # Calculate evaluation metrics
      TP <- sum((dt_pred == 1) & (test.set[[target_var]] == 1))
      TN <- sum((dt_pred == 0) & (test.set[[target_var]] == 0))
      FP <- sum((dt_pred == 1) & (test.set[[target_var]] == 0))
      FN <- sum((dt_pred == 0) & (test.set[[target_var]] == 1))
      
      # Confusion Matrix
      dt_confusion_matrix <- table(Actual = test.set[[target_var]], Predicted = dt_pred)
      print(paste("Fold", i, "Decision Tree Confusion Matrix:"))
      print(dt_confusion_matrix)
      
      # Store the confusion matrix in the list
      all_confusion_matrices[[paste("DT_Fold_", i)]] <- dt_confusion_matrix
      
      Accuracy <- (TP + TN) / (TP + TN + FP + FN)
      Precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
      Recall <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
      Specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
      FDR <- ifelse((FP + TP) > 0, FP / (FP + TP), NA)
      FOR <- ifelse((FN + TN) > 0, FN / (FN + TN), NA)
      
      auc_dt <- auc(roc(test.set[[target_var]], dt_prob))
      plot(roc(test.set[[target_var]], dt_prob),
           main = paste("ROC Curve - Fold", i, "Decision Tree"),
           xlab = "False Positive Rate",
           ylab = "True Positive Rate")
      
      results <- rbind(results, data.frame(
        Fold = i,
        Model = "Decision Tree",
        Accuracy = Accuracy,
        Precision = Precision,
        Recall = Recall,
        Specificity = Specificity,
        FDR = FDR,
        FOR = FOR,
        AUC = auc_dt
      ))
    }
    
    #Logistic Regression Model
    if ("Logistic Regression" %in% models) {
      lr_model <- glm(as.formula(paste(target_var, "~ . -Churn")), data = train.set, family = binomial)
      lr_prob <- predict(lr_model, test.set, type = "response")
      lr_pred <- ifelse(lr_prob >= 0.5, 1, 0)
      
      # Calculate evaluation metrics
      TP <- sum((lr_pred == 1) & (test.set[[target_var]] == 1))
      TN <- sum((lr_pred == 0) & (test.set[[target_var]] == 0))
      FP <- sum((lr_pred == 1) & (test.set[[target_var]] == 0))
      FN <- sum((lr_pred == 0) & (test.set[[target_var]] == 1))
      
      # Confusion Matrix
      lr_confusion_matrix <- table(Actual = test.set[[target_var]], Predicted = lr_pred)
      print(paste("Fold", i, "Logistic Regression Confusion Matrix:"))
      print(lr_confusion_matrix)
      
      # Store the confusion matrix in the list
      all_confusion_matrices[[paste("LR_Fold_", i)]] <- lr_confusion_matrix
      
      Accuracy <- (TP + TN) / (TP + TN + FP + FN)
      Precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
      Recall <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
      Specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
      FDR <- ifelse((FP + TP) > 0, FP / (FP + TP), NA)
      FOR <- ifelse((FN + TN) > 0, FN / (FN + TN), NA)
      
      auc_lr <- auc(roc(test.set[[target_var]], lr_prob))
      plot(roc(test.set[[target_var]], lr_prob),
           main = paste("ROC Curve - Fold", i, "Logistic Regression"),
           xlab = "False Positive Rate",
           ylab = "True Positive Rate")
      
      results <- rbind(results, data.frame(
        Fold = i,
        Model = "Logistic Regression",
        Accuracy = Accuracy,
        Precision = Precision,
        Recall = Recall,
        Specificity = Specificity,
        FDR = FDR,
        FOR = FOR,
        AUC = auc_lr
      ))
    }
  }
  
  return(list(results = results, lift_chart_data = list(), confusion_matrices = all_confusion_matrices))
}

model_results <- train_and_evaluate(telco, folds, models = c("Decision Tree", "Logistic Regression"))
results <- model_results$results
confusion_matrices <- model_results$confusion_matrices

# Print the confusion matrices 
print("All Confusion Matrices:")
print(confusion_matrices)

# Summary of Model Performance
summary_results <- results %>%
  group_by(Model) %>%
  summarise(
    Accuracy = round(mean(Accuracy, na.rm = TRUE), 3),
    Precision = round(mean(Precision, na.rm = TRUE), 3),
    Recall = round(mean(Recall, na.rm = TRUE), 3),
    FDR = round(mean(FDR, na.rm = TRUE), 3),
    FOR = round(mean(FOR, na.rm = TRUE), 3),
    AUC = round(mean(AUC, na.rm = TRUE), 3)
  )
print(summary_results)

# Final Logistic Model Deployment
final_model <- glm(Churn_Encoded ~ . -Churn, data = telco, family = "binomial")
final_prob <- predict(final_model, newdata = telco, type = "response")

# Lift Chart
gain <- gains(telco$Churn_Encoded, final_prob, groups = 10)
plot(c(0, gain$cume.pct.of.total * sum(telco$Churn_Encoded)) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative # of Churned Customers",
     main = "Lift Chart", type = "l")
lines(c(0, sum(telco$Churn_Encoded)) ~ c(0, dim(telco)[1]), lty = 2)

# Decile-wise Lift Chart
gain <- gains(telco$Churn_Encoded, final_prob)
heights <- gain$mean.resp / mean(telco$Churn_Encoded)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0, max(heights) + 1),
                     xlab = "Deciles", ylab = "Mean Response",
                     main = "Decile-wise Lift Chart")
text(midpoints, heights + 0.3, labels = round(heights, 2), cex = 0.8)