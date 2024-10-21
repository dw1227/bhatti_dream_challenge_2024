# name: get_all_predictions.R
#
# author: Gaurav Bhatti
#
# input: data/prb/beta_complete_test_bmiq.Rdata
#        data/prb/anoall.csv
#        data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        data/processed/autogluon_predictions_450k.csv
#        data/processed/autogluon_predictions_850k.csv
#        
# output: ano with predictions                                                      
#         data/processed/ano_all_predictions.csv

rm(list=ls())
library(planet)
library(tidyverse)


## load test data and annotation
load("data/prb/beta_complete_test_bmiq.Rdata")

ano<- read_csv("data/prb/anoall.csv") |> 
  data.frame() |> 
  column_to_rownames("Sample")
all(rownames(ano)%in%colnames(beta_norm_BMIQ))
beta_norm_BMIQ<- beta_norm_BMIQ[,rownames(ano)]



## Lee et al 2019 clocks
ano <- ano %>%
  mutate(
    ga_rpc= predictAge(beta_norm_BMIQ, type = "RPC"),
    Sample= rownames(ano),
    ga_cpc = predictAge(beta_norm_BMIQ, type = "CPC"),
    ga_rrpc= predictAge(beta_norm_BMIQ, type = "RRPC")
  )

# autogluon 

ano<- read_csv("data/processed/autogluon_predictions_450k.csv") |> 
  rename(ga_automl_450k=GA,
         Sample=Sample_ID) |> 
  right_join(ano,by=c("Sample")) 


ano<- read_csv("data/processed/autogluon_predictions_850k.csv") |> 
  rename(ga_automl_850k=GA,
         Sample=Sample_ID) |> 
  right_join(ano,by=c("Sample"))
  
  
  


## Mayne et al 2017 clock
# mayne_clock_coef<- read_csv("data/clocks/mayne_clock_coefficients.csv")
# probe_coef<- mayne_clock_coef |> 
#   filter(Probe %in%  rownames(beta_norm_BMIQ))
# beta_matrix<- beta_norm_BMIQ [probe_coef$Probe,]
# 
# ano$ga_mayne<-  colSums(beta_matrix*probe_coef$Coefficient) + 
#   (mayne_clock_coef |> filter(Probe=="(Intercept)") |> pull(Coefficient))
# 
# rmse <- Metrics::rmse(ano[["Del_GA_Calc"]], ano[["ga_mayne"]])




#### extract GA for top performers
df_test <- read_csv("data/submissions/Test_data_evaluation.csv")  |>  
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse,n=1,with_ties = FALSE) |> 
  ungroup() |> 
  mutate(prediction_file = file.path("data/submissions", 
                                     paste0("output_run_", submission_stamp),"predictions.csv")) 

# run this to generate all_predictions
prediction_files<- df_test$prediction_file
names(prediction_files)<- paste0("ga_",df_test$submission_stamp)

all_predictions<- map_dfr(.x=prediction_files,
        .f=read_csv,
        .id= "submission_stamp",
        col_types=c("cd")) |>
  pivot_wider(names_from = submission_stamp,
              values_from = "GA_prediction") 
   
   
### Merge with annotation
ano<- ano |> left_join(all_predictions,by=c("Sample"="ID"))




### Wisdom of crowd 
top_performers <- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(submission_stamp)

names(top_performers)<- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(Submitter)

woc_candidates <-  top_performers

ano$ga_woc<- ano |> 
  select(paste0("ga_",woc_candidates)) |> 
  rowwise() |> 
  mutate(ga_woc = mean(c_across(everything()), na.rm = TRUE)) |> 
  pull(ga_woc)   

write_csv(ano,"data/processed/ano_all_predictions.csv")
 


