
library(planet)
library(tidyverse)
library(Metrics)


ano<- read_csv("data/prb/anoall.csv") |> 
  data.frame() |> 
  column_to_rownames("Sample")



load("data/prb/beta_complete_test_bmiq.Rdata")
load("data/prb/beta_raw.Rdata")

beta_norm_BMIQ<- beta_norm_BMIQ[,rownames(ano)]

beta_test<- beta_test[,rownames(ano)]



all(rownames(ano)==colnames(beta_norm_BMIQ))
all(rownames(ano)==colnames(beta_test))


ano <- ano %>%
  mutate(
    ga_RPC_bmiq = predictAge(beta_norm_BMIQ, type = "RPC"),
    ga_RPC_raw = predictAge(beta_test, type = "RPC")
    
    # ga_CPC = predictAge(beta_norm_BMIQ, type = "CPC"),
    # ga_RRPC = predictAge(beta_norm_BMIQ, type = "RRPC")
  )


mae= paste("Mean absolute error=", round(mae(ano$Del_GA_Calc,ano$ga_RPC_bmiq),2), "weeks")
rho= round(cor(ano$Del_GA_Calc,ano$ga_RPC_bmiq),2)
rho= paste("Pearson correlation=", rho)
rmse= paste("RMSE=", round(rmse(ano$Del_GA_Calc,ano$ga_RPC_bmiq),2), "weeks")



mae= paste("Mean absolute error=", round(mae(ano$Del_GA_Calc,ano$ga_RPC_raw),2), "weeks")
rho= round(cor(ano$Del_GA_Calc,ano$ga_RPC_raw),2)
rho= paste("Pearson correlation=", rho)
rmse= paste("RMSE=", round(rmse(ano$Del_GA_Calc,ano$ga_RPC_raw),2), "weeks")




