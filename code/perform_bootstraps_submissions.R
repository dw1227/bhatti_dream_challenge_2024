# name: perform_bootstrap_submissions.R
#
# author: Gaurav Bhatti
#
# input: data/submissions/Test_data_evaluation.csv
#        data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        data/prb/anoall.csv
#        data/submissions/all_predictions.csv
#        
# output: predictions of participants over bootstrap resamples
#         results/Bootstrap_RMSE_Results.csv



rm(list=ls())


library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(Metrics)

# Read the  submission data
df_test <- read_csv("data/submissions/Test_data_evaluation.csv")  |>  
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse,n=1,with_ties = FALSE) |> 
  ungroup() 

df<- read_csv("data/submissions/Job-393694313420778661233189284.csv")
df_select_submissions<-df |> 
  filter(submission_status %in% "SCORED") |> 
  group_by(submitterid,evaluationid) |> 
  mutate(submission_datetime=  as.POSIXlt(createdOn/1000, origin = "1970-01-01")) |> 
  slice(which.max(submission_datetime)) |> 
  ungroup() |> 
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  filter(submission_stamp %in% df_test$submission_stamp) |> 
  as.data.frame()
  
ano<- read_csv("data/processed/ano_all_predictions.csv") 

all_predictions<-  read_csv("data/submissions/all_predictions.csv") |> 
  left_join(ano|>   dplyr::select(Sample,ga_woc), by = join_by(ID==Sample))



clocks<- c("ga_woc","ga_3505704_9615595", "ga_3506332_9615596" ,
           "ga_3511256_9615595", "ga_3512486_9615595", 
           "ga_3449866_9615596" ,"ga_3320980_9615595" ,
           "ga_3453175_9615595" )

res <- NULL
n_bootstrap <- 1000  # Number of bootstrap resamples

# Iterate through submissions
for (i in 1:length(clocks)) {

  # submitter_stamp <- df_select_submissions$submitterid[i]
  # evalqu_stamp <- df_select_submissions$evaluationid[i]
  
    # Read predictions
    pred <- all_predictions |> 
      dplyr::select("ID",
             GA_prediction=clocks[i]) |> 
      as.data.frame()
    rownames(pred) <- pred$ID
  
    
      
  # read the gold standard
  gold <- read_csv("data/prb/anoall.csv") %>%
      as.data.frame()
    rownames(gold) <- gold$Sample
  
  # Bootstrap resampling
  for (b in 1:n_bootstrap) {
    # Resample the gold dataset with replacement
    gold_resample <- gold[sample(nrow(gold), replace = TRUE), ]
    rownames(gold_resample)<-NULL
    
    # Align  predictions with resamples gold standard
    pred_resample <- pred[match(gold_resample$Sample,pred$ID), ]
    

    # Calculate RMSE for the bootstrap sample
    rmse_bootstrap <- Metrics::rmse(gold_resample[["Del_GA_Calc"]], pred_resample[["GA_prediction"]])
    res <- rbind(res, c(clocks[i], b, rmse_bootstrap))
  }
}

# Assign column names and save results
colnames(res) <- c("clock", "bootstrap_iteration", "rmse")
res <- as.data.frame(res)
write_csv(res, "results/Bootstrap_RMSE_Results.csv")



