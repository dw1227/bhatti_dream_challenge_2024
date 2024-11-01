# name: wsu_placenta_clock_450k.R
#
# author: Gaurav Bhatti
#
# input: data/dream_challenge/Beta_normalized_subchallenge2.csv
#        data/dream_challenge/Sample_annotation_metadata.csv
#        data/dream_challenge/Probe_array.csv
#        
#        
# output: Heatmap Visualization of 850k Challenge Data
#         results/heatmap_sc2.pdf

rm(list = ls())
library(tidyverse)

# library(vroom)
# library(pheatmap)
# library(scico)
# library(ChAMP)
# 
# ## Read the SC1 raw data
# load("data/dream_challenge/Beta_all_oladejo.Rdata") 
# 
# ## Read the annotation for the public data
# ano_challenge <- read_csv("data/dream_challenge/Sample_annotation_metadata.csv") |> 
#   select(Sample_Name, GA,Sample_accession) |> 
#   rename(Sample_ID=Sample_Name) |> 
#   filter(Sample_ID %in% colnames(beta_cue_knn))
# 
# 
# df_train <- beta_cue_knn[, ano_challenge$Sample_ID]
# detp <- detp_combined_all[rownames(df_train), ano_challenge$Sample_ID]
# 
# rm(detp_combined_all, beta_cue_knn)
# 
# # Preprocess the data
# 
# # 1) Filter out probes that fall near SNPs, align to multiple locations, or target locations on X and Y chromosomes
# # Participants may use different filtering criteria as per their strategies
# beta_train_filter <- champ.filter(
#   beta = df_train,
#   pd = ano_challenge,
#   filterDetP = FALSE,
#   detP = NULL,
#   autoimpute = FALSE,
#   filterBeads = FALSE,
#   arraytype = "450K",  # Set this argument to "450K" for sub-challenge 1
#   fixOutlier = FALSE,
#   filterNoCG = TRUE,
#   filterSNPs = TRUE,
#   filterMultiHit = TRUE,
#   filterXY = TRUE
# )$beta
# 
# 
# # 2) Filter probes based on detection p-values
# # Keep probes that are reliably detected in all samples (p-value < 0.01)
# # Participants may choose different detection p-value thresholds or criteria
# p_vals <- apply(detp, 1, function(x) {
#   sum(x < 0.01, na.rm = TRUE) / length(na.omit(x))  # Proportion of samples with p-value < 0.01
# })
# keep <- names(p_vals)[p_vals == 1]  # Keep probes detected in all samples
# beta_train_filter_detp <- beta_train_filter[rownames(beta_train_filter) %in% keep, ]
# 
# 
# 
# rm(detp,df_train,beta_train_filter,
#    hm450.manifest.hg19,keep,multi.hit,p_vals,probe.features)
# 
# save(beta_train_filter_detp,ano_challenge,
#      file="data/dream_challenge/beta_train_filter_detp_sc1.Rdata")
# 
# load("data/dream_challenge/beta_train_filter_detp_sc1.Rdata")
# 
# # Normalize the filtered beta values using the BMIQ method
# # Participants are encouraged to explore different normalization methods
# beta_norm_BMIQ <- champ.norm(
#   beta = beta_train_filter_detp,
#   arraytype = "450K",
#   method = "BMIQ",
#   cores = 6
# )

# save(beta_norm_BMIQ,ano_challenge,
#      file="data/dream_challenge/beta_public_normalized_sc1.Rdata")

# Load required packages
library(glmnet)
library(doParallel)  # For parallel processing
library(limma)
library(randomForest)

## get the data
load("data/dream_challenge/beta_public_normalized_sc1.Rdata")
ano_challenge<-data.frame(ano_challenge)
rownames(ano_challenge)<-ano_challenge$Sample_ID


# Assuming your data is a dataframe `df` where the first column is GA (response) and the rest are CpG probes (features)
X <- t(as.matrix(beta_norm_BMIQ))  
y <- ano_challenge[colnames(beta_norm_BMIQ),"GA"] # The first column is the target variable (GA)



## specific filtering
# design <- model.matrix(~y)
# fit<- lmFit(beta_norm_BMIQ,design)
# fit<- eBayes(fit)
# limma_res<-topTable(fit,coef = "y",number = Inf,adjust.method = "fdr")
# sel_probes<- rownames(limma_res[limma_res$adj.P.Val<0.01,])
# X<- X[,sel_probes]


# Create a grid of alpha values (for Elastic Net) to search
alpha_grid <- c(seq(0.1, 1, by = 0.1))  # Alpha ranges from 0.01 (No Ridge) to 1 (Lasso)

# Set up parallel processing to speed up the grid search
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
cl <- makeCluster(num_cores, type = "PSOCK")
registerDoParallel(cl)

print(paste("Cores used:",num_cores))

set.seed(1984)
# Run cross-validation for each alpha
cv_results <- lapply(alpha_grid, function(alpha_value) {
  print(paste("Fitting model with alpha =", alpha_value))
  cv.glmnet(X, y, alpha = alpha_value, nfolds = 10, parallel = TRUE)
})

# Stop parallel processing after training is done
stopCluster(cl)
registerDoSEQ()

# Extract the best model based on cross-validated lambda (minimum error)
best_model <- NULL
best_alpha <- NULL
min_mse <- Inf  # Initialize to a large value


# Extract min_mse and alpha for each cv_fit in cv_results and combine into a data frame
res <- map_dfr(seq_along(cv_results), ~ {
  cv_fit <- cv_results[[.x]]
  tibble(
    iteration = .x,
    min_mse = min(cv_fit$cvm),
    alpha = alpha_grid[.x]
  )
})



min_alpha<- res |> 
  arrange(min_mse) |> 
  slice(1) 

best_model<- cv_results[[min_alpha$iteration]]

# Print the best alpha and lambda
print(paste("Best alpha:", min_alpha$alpha))
print(paste("Best lambda:", best_model$lambda.1se))



# Save the final model with best alpha and lambda
save(best_model,file="data/clocks/wsu_pl_clock_450k.Rdata")


load("data/clocks/wsu_pl_clock_450k.Rdata")



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



