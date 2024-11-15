# name: plot_ega_acceleration.R
#
# author: Gaurav Bhatti
#
# input: data/processed/ano_all_predictions.csv        
#        data/submissions/Test_data_evaluation.csv
#       
# output: predicted vs reported GA for RPC, top performers and WOC
#         results/Parity_plots.pdf
#         results/eGA_acceleration_table.csv


library(tidyverse)
library(here)
library(ggpubr)

ano<- read_csv(here("data/processed/ano_all_predictions.csv"))


# test_results <- ano |> 
#   mutate(eGA_acceleration = ga_rpc - Del_GA_Calc) |> 
#   mutate(Group=if_else(Del_GA_Calc<37,"PTB","TB"),
#          Clock="RPC") |> 
#   group_by(Group) |> 
#   summarise(Clock=unique(Clock),
#             t_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$statistic,
#             p_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$p.value,
#             mean_eGA_acceleration = round(mean(eGA_acceleration, na.rm = TRUE),2),
#             sd_eGA_acceleration = sd(eGA_acceleration, na.rm = TRUE),
#             n = n()) |> 
#   bind_rows(ano |> 
#               mutate(eGA_acceleration = ga_3505704_9615595 - Del_GA_Calc) |> 
#               mutate(Group=if_else(Del_GA_Calc<37,"PTB","TB"),
#                      Clock="Top_team") |> 
#               group_by(Group) |> 
#               summarise(Clock=unique(Clock),
#                         t_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$statistic,
#                         p_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$p.value,
#                         mean_eGA_acceleration = round(mean(eGA_acceleration, na.rm = TRUE),2),
#                         sd_eGA_acceleration = sd(eGA_acceleration, na.rm = TRUE),
#                         n = n())) |> 
#   bind_rows(ano |> 
#               mutate(eGA_acceleration = ga_woc - Del_GA_Calc) |> 
#               mutate(Group=if_else(Del_GA_Calc<37,"PTB","TB"),
#                      Clock="WOC") |> 
#               group_by(Group) |> 
#               summarise(Clock=unique(Clock),
#                         t_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$statistic,
#                         p_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$p.value,
#                         mean_eGA_acceleration = round(mean(eGA_acceleration, na.rm = TRUE),2),
#                         sd_eGA_acceleration = sd(eGA_acceleration, na.rm = TRUE),
#                         n = n())) |> 
#   write_csv(file="results/eGA_acceleration_table.csv")

  

ano<- ano |> 
  filter(!Group %in% "pretermSGA")

test_ranking<- read_csv(here("data/submissions/Test_data_evaluation.csv"))



### Calculate,compare, and plot epigenetic gestational age acceleration 

analyze_eGA_acceleration <- function(data, ega_column, chronological_column, 
                                     title=NULL) {
  data <- data |> 
    mutate(eGA_acceleration = !!sym(ega_column) - !!sym(chronological_column)) |> 
    mutate(Group = factor(Group2, levels = c("PTL","PPROM","pretermPE", 
                                             "termPE","termSGA","Controls")))
  levels(data$Group)<- c("PTL","PPROM","Preterm PE", 
                         "Term PE","Term SGA","Control")
  
  # Performing one-sided t-tests to determine if eGA is significantly different from chronological age
  test_results <- data |> 
    group_by(Group) |> 
    summarise(t_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$statistic,
              p_value = t.test(eGA_acceleration, mu = 0, alternative = "two.sided")$p.value,
              mean_eGA_acceleration = round(mean(eGA_acceleration, na.rm = TRUE),2),
              sd_eGA_acceleration = sd(eGA_acceleration, na.rm = TRUE),
              n = n()) 
  
  # Plotting
  max_y <- max(data$eGA_acceleration, na.rm = TRUE)
  p_value_annotation <- mutate(test_results, y.position = max_y * 1.1)

  
  box_plot <- ggboxplot(data, x = "Group", y = "eGA_acceleration",
                        color = "Group", #palette = nature_palette, 
                        add = "jitter",
                        ylab = "eGA Acceleration (epigenetic GA - chronological GA)", 
                        xlab = "Group") +
    labs(title = title,x="") +
    theme_pubr() +
    theme(legend.position = "none") # Remove legend for cleaner plot
  
  # Adding p-value annotations manually
  box_plot<- box_plot +
    geom_text(data = test_results, 
              aes(x = Group, y = max_y * 1.1,
                  label = sprintf("%.1f(p= %.3f)",mean_eGA_acceleration,
                                  p_value)),hjust = 0.5, vjust = 0) +
    geom_hline(yintercept = 0,linetype=2,alpha=0.2)  +   
    annotate("text",x = 1, y = max_y * 1.2,
                  label = "Mean eGA acceleration",size=5
             ,hjust=0.23)+
    scale_color_manual(values= c("red","green","blue",
                             "purple",  "orange","black"),
                   breaks=c("PTL","PPROM","Preterm PE", 
                           "Term PE","Term SGA","Control"))

 
  
  return(box_plot)
}


## Compare eGA acceleration Using different clocks

rpc_results <- analyze_eGA_acceleration(ano, "ga_rpc", "Del_GA_Calc",
                                        title="Robust Placental Clock")


cpc_results <- analyze_eGA_acceleration(ano, "ga_cpc", "Del_GA_Calc",
                                        title="Control Placental Clock")

rrpc_results <- analyze_eGA_acceleration(ano, "ga_rrpc", "Del_GA_Calc",
                                         title="Refined Robust Placental Clock")


top_teams <- test_ranking |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  slice_min(Test_rmse, n = 3, with_ties = FALSE) |> 
  mutate(stamp = paste("ga", submitterid, evaluationid, sep = "_")) |> 
  pull(stamp)

top_team_results_1 <- analyze_eGA_acceleration(ano, top_teams[1],"Del_GA_Calc",
                                               title="Team 1")

top_team_results_2 <- analyze_eGA_acceleration(ano, top_teams[2],"Del_GA_Calc",
                                               title="Team 2")

top_team_results_3 <- analyze_eGA_acceleration(ano, top_teams[3],"Del_GA_Calc",
                                               title="Top Performer #3")

woc_results <- analyze_eGA_acceleration(ano, "ga_woc","Del_GA_Calc",
                                        title="Wisdom of Crowds")

wsu_450_results <- analyze_eGA_acceleration(ano, "wsu_450k","Del_GA_Calc",
                                        title="Wayne state placental clock")


pdf("results/eGA_acceleration_existing_clocks.pdf",width = 8)
rpc_results
cpc_results
rrpc_results
dev.off()

pdf("results/eGA_acceleration_dream_clocks.pdf",width = 8)
top_team_results_1
top_team_results_2
woc_results
wsu_450_results
dev.off()



