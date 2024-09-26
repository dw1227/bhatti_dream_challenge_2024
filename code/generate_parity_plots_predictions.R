# name: generate_parity_plots_predictions.R
#
# author: Gaurav Bhatti
#
# input: data/processed/ano_all_predictions.csv        
#        data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        data/submissions/all_predictions.csv
#        
# output: ano with predictions and predicted vs reported GA for RPC, 
#         top performers and WOC solution
#         results/Parity_plots.pdf
#         data/processed/ano_all_predictions.csv



rm(list=ls())
library(planet)
library(tidyverse)
library(Metrics)
library(viridis)


###  annotation
ano<- read_csv("data/processed/ano_all_predictions.csv")




# wisdom of crowd
df_test <- read_csv("data/submissions/Test_data_evaluation.csv")  |>  
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse,n=1,with_ties = FALSE) |> 
  ungroup()
# df <- read_csv("data/submissions/Job-393694313420778661233189284.csv")
top_performers <- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(submission_stamp)
names(top_performers)<- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(Submitter)




# Define a function to create a plot for each GA column
plot_ga_predictions <- function(ga_column,title) {
  # Calculate rmse, mae, and correlation for each column
  rmse <- Metrics::rmse(ano[["Del_GA_Calc"]], ano[[ga_column]])
  mae <- Metrics::mae(ano[["Del_GA_Calc"]], ano[[ga_column]])
  cor <- cor(ano[["Del_GA_Calc"]], ano[[ga_column]])
  
  # Prepare labels
  rho <- paste("Correlation=", round(cor, 2))
  error <- paste0("RMSE= ", round(rmse, 2), " wks")
  MAE <- paste0("MAE= ", round(mae, 2), " wks")
  
  # Create the scatter plot
  df <- data.frame(gold = ano[["Del_GA_Calc"]], 
                   prediction = ano[[ga_column]],
                   group=ano[["Group2"]]) |> 
    filter(!group%in% "pretermSGA") |> 
    mutate(group=factor(group,levels = c("PTL","PPROM","pretermPE", 
                                        "termPE","termSGA","Controls")))
  levels(df$group)<-c("PTL","PPROM","Preterm PE", 
                      "Term PE","Term SGA","Control")
  g_sc <- df %>% 
    ggplot(aes(x = gold, y = prediction,
               color=group,group=group)) + 
    geom_point(stroke=1,size=3) +
    geom_abline(slope = 1, intercept = 0, colour = "black") +
    #geom_smooth(method = "lm", se = FALSE, 
    #            aes(color = group)) +
    scale_color_manual(values= c("red","green","blue",
                                 "purple",  "orange","black"),
                       breaks=c("PTL","PPROM","Preterm PE", 
                          "Term PE","Term SGA","Control"))+
    labs(x = "Reported GA (weeks)",
         y = "Predicted GA (weeks)",
         title=title,
         color="") +
    lims(x = c(18, 43),
         y=c(18,43)) +
    theme_bw() +
    annotate("text", label = rho, x = 20, y = 40, size = 5, 
             colour = "black",hjust=0) +
    annotate("text", label = error, x = 20, y = 39, size = 5,
             colour = "black",hjust=0)+
    annotate("text", label = MAE, x = 20, y = 38, size = 5, 
             colour = "black",hjust=0)
  
  return(g_sc)
}



columns_to_plot <- c(Mayne="ga_mayne",
                    RPC="ga_rpc", 
                     CPC="ga_cpc",
                     RRPC="ga_rrpc",
                     paste0("ga_", top_performers),"ga_woc")

names(columns_to_plot)<- c("Mayne et al. 2017",
                           "Robust Placental Clock",
                           "Control Placental Clock",
                           "Refined Robust Placental Clock",
                           names(top_performers),
                           "Wisdom of Crowd")

# Use purrr::map to create plots for all GA columns and store them in a list
plots <- map2(columns_to_plot, 
              names(columns_to_plot), 
              plot_ga_predictions)


pdf("results/parity_plots.pdf", width = 8, height = 6)
# Loop over each plot and print it to the PDF
for (plot in plots) {
  print(plot)
}
dev.off()



