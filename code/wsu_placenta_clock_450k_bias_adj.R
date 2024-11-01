library(glmnet)
library(doParallel)  # For parallel processing
library(caret)       # For creating stratified folds
library(tidyverse)

# Load your data
load("data/dream_challenge/beta_public_normalized_sc1.Rdata")
ano_challenge <- data.frame(ano_challenge)
rownames(ano_challenge) <- ano_challenge$Sample_ID

# Assuming your data matrix X and response vector y
X <- t(as.matrix(beta_norm_BMIQ))
# X<-X[,c(1:1000)]
y <- ano_challenge[colnames(beta_norm_BMIQ), "GA"]

# Define the range of alpha values for glmnet
alpha_grid <- seq(0.1, 1, by = 0.1)

# Create stratified folds #? (ensuring a balanced distribution of gestational ages)
set.seed(202)  # For reproducibility
folds <- createFolds(y, k = 5, list = TRUE)

# Set up parallel processing to speed up the cross-validation
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 4))
cl <- makeCluster(num_cores, type = "PSOCK")
registerDoParallel(cl)

# Function to perform glmnet fitting and evaluate with custom scoring
fit_and_evaluate <- function(train_idx, test_idx, alpha) {
  train_x <- X[train_idx, , drop = FALSE]
  train_y <- y[train_idx]
  
  test_x <- X[test_idx, , drop = FALSE]
  test_y <- y[test_idx]
  
  # Fit model
  fit <- cv.glmnet(train_x, train_y, 
                   alpha = alpha, 
                   family = "gaussian",
                   nfolds = 10, 
                   parallel = TRUE)
  
  # Predict and evaluate
  predictions <- predict(fit, s = "lambda.min", newx = test_x)
  mse <- mean((test_y - predictions)^2)

  # Calculate score by considering RMSE across gestational age quantiles
  #quantiles <- quantile(test_y, probs = seq(0, 1, by = 0.2))
  groups <- cut(test_y, breaks = c(0,20,37,45), include.lowest = TRUE, labels = FALSE)
  mse_by_group <- tapply((test_y - predictions)^2, groups, mean)
  score <- mean(mse_by_group)  # Lower score is better

  return(list(mse = mse, score = score, model = fit))
}

# Perform cross-validation
cv_results <- lapply(folds, function(fld) {
  lapply(alpha_grid, function(alpha) {
    fit_and_evaluate(train_idx= setdiff(1:length(y),fld), test_idx = fld, alpha)
  })
})

# Aggregate results to find the best alpha
scores <- sapply(cv_results, function(alpha_results) {
  sapply(alpha_results, function(res) res$score)
})
mean_scores <- colMeans(scores)
best_alpha <- alpha_grid[which.min(mean_scores)]

print(paste("Best alpha:", best_alpha))

# Fit final model using the best alpha
best_model <- cv.glmnet(X, y, alpha = best_alpha)


# Stop parallel processing
stopCluster(cl)
registerDoSEQ()


save(best_model,file="data/clocks/wsu_pl_clock_450k_bias_alpha.Rdata")


# Make predictions on the test data set
## load test data and annotation
load("data/prb/beta_complete_test_bmiq.Rdata")
ano<- read_csv("data/prb/anoall.csv") |> 
  data.frame() |> 
  column_to_rownames("Sample")
all(rownames(ano)%in%colnames(beta_norm_BMIQ))
X_test<- t(beta_norm_BMIQ[colnames(X),rownames(ano)])

predictions<- predict(best_model,newx = X_test)
obs<- ano[rownames(predictions),"Del_GA_Calc"]

print(Metrics::rmse(obs,predictions[,1]))
print(Metrics::bias(obs,predictions[,1]))

