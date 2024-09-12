# name: calculate_bayes_factor_plot_rmse.R
#
# author: Gaurav Bhatti
#
# input: results/Bootstrap_RMSE_Results.csv
#        data/submissions/Job-393694313420778661233189284.csv
#        
# output: Bayes factor and distribution of RMSE over bootstrap resamples
#         results/Bayes_factor_rmse_violin.pdf



rm(list=ls())


library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(Metrics)




df<- read_csv("data/submissions/Job-393694313420778661233189284.csv")
df<- df |> select(submitterid,Submitter) |> 
  filter(!duplicated(submitterid)) |> 
  mutate(user= str_extract(Submitter,pattern = "\\(.*\\).*"),
         user= str_replace_all(user, "[()]", ""),
         user=if_else(is.na(user),
                      str_replace(Submitter,pattern = "Team - ",""),
                      paste0("@",user)),
         Submitter=user)

# ---- Bayes Factor Calculation ----
res_df<- read_csv("results/Bootstrap_RMSE_Results.csv")
res_df<- res_df |> left_join(df,by="submitterid")


# Step 1: Calculate the median RMSE for each team
team_rmse_median <- res_df %>%
  group_by(submitterid,Submitter) %>%
  summarize(median_rmse = median(rmse))

# Step 2: Rank teams by their median RMSE (lower RMSE is better)
team_rmse_median <- team_rmse_median %>%
  arrange(median_rmse) %>%
  mutate(rank = row_number())

# Step 3: Initialize empty columns to store Bayes factors
team_rmse_median$bayes_k_k1 <- NA

# Step 4: Calculate Bayes factor for k vs k+1 for each team (except the last team)
for (k in 1:(nrow(team_rmse_median) - 1)) {
  # Subset the RMSE results for team k and team k+1
  rmse_team_k <- res_df %>% filter(submitterid == team_rmse_median$submitterid[k])
  rmse_team_k1 <- res_df %>% filter(submitterid == team_rmse_median$submitterid[k + 1])
  
  # Initialize counters for better and worse comparisons
  num_k_better <- 0
  num_k_worse <- 0
  
  # Compare RMSEs for each bootstrap iteration
  for (b in 1:max(res_df$bootstrap_iteration)) {
    # Get the RMSE for team k and team k+1 for the b-th bootstrap iteration
    rmse_k_b <- rmse_team_k %>% filter(bootstrap_iteration == b) %>% pull(rmse)
    rmse_k1_b <- rmse_team_k1 %>% filter(bootstrap_iteration == b) %>% pull(rmse)
    
    # Check if team k performed better or worse in this iteration
    if (rmse_k_b < rmse_k1_b) {
      num_k_better <- num_k_better + 1
    } else if (rmse_k_b > rmse_k1_b) {
      num_k_worse <- num_k_worse + 1
    }
  }
  
  # Calculate Bayes factor for k vs k+1 (avoid division by zero)
  team_rmse_median$bayes_k_k1[k] <- ifelse(num_k_worse > 0, 
                                           num_k_better / num_k_worse, Inf)
}
  
  
  # Prepare the data for violin plot
  violin_data <- res_df %>%
    inner_join(team_rmse_median, by = "Submitter") %>%
    mutate(Submitter = factor(Submitter,
                                levels = team_rmse_median$Submitter))
  
  team_rmse_median<- team_rmse_median |> 
    mutate(Submitter = factor(Submitter,
                                levels = team_rmse_median$Submitter))
  
  # Plot the violin plot
  g_vp<- ggplot(violin_data, aes(x = Submitter, y = rmse,
                                 fill=Submitter)) +
    geom_violin(trim = FALSE) +                          # Violin plot for RMSE distribution
    stat_summary(fun = median, geom = "point", size = 2) + # Display median RMSE as points
    theme_bw() +
    labs(title = "Violin Plot of RMSE across Bootstrap Resamples",
         x = "Team",
         y = "RMSE") +
    geom_text(data = team_rmse_median, aes(x = Submitter, 
                                           y = 3.6, 
                                           label = round(bayes_k_k1, 2)),
              vjust = -0.5)   +
    # Add the label for Bayes factor
    annotate("text", x = 1, y = 3.8, label = "Bayes factor (k vs k+1)", 
             vjust = 0, hjust = 0, size = 4, fontface = "bold") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                     size = 12,face = "bold"),
          axis.title.x = element_blank(),
          legend.position = "none")
  
  pdf("results/Bayes_factor_rmse_violin.pdf",width=10,height = 8)
  print(g_vp)
  dev.off()
  