# name: generate_parity_plots_predictions.R
#
# author: Gaurav Bhatti
#
# input: data/processed/ano_all_predictions.csv        
#        data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        data/submissions/all_predictions.csv      
#        

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
library(gridExtra)
library(patchwork)
library(ggpubr)
library(cowplot)
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
    geom_point(stroke=1,size=3,alpha=0.6) +
    geom_abline(slope = 1, intercept = 0, colour = "black") +
    #geom_smooth(method = "lm", se = FALSE, 
    #            aes(color = group)) +
    # scale_color_manual(values= c("red","green","blue",
    #                              "purple",  "orange","black"),
    #                    breaks=c("PTL","PPROM","Preterm PE", 
    #                       "Term PE","Term SGA","Control"))+
    scale_color_manual(values= c("#D55E00","#009E73","#0072B2",
                                 "#CC79A7",  "#E69F00","#000000"),
                       breaks=c("PTL","PPROM","Preterm PE", 
                                "Term PE","Term SGA","Control"))+
    
    labs(x = "Reported GA (weeks)",
         y = "Predicted GA (weeks)",
         title=title,
         color="") +
    lims(x = c(18, 43),
         y=c(18,43)) +
    theme_cowplot() +
    annotate("text", label = rho, x = 20, y = 40, size = 4, 
             colour = "black",hjust=0) +
    annotate("text", label = error, x = 20, y = 39, size = 4,
             colour = "black",hjust=0)+
    annotate("text", label = MAE, x = 20, y = 38, size = 4, 
             colour = "black",hjust=0)+
    theme(plot.title = element_text(hjust=0.5))
  
  return(g_sc)
}



columns_to_plot <- c(RPC="ga_rpc", 
                     CPC="ga_cpc",
                     RRPC="ga_rrpc",
                     paste0("ga_", top_performers),
                     "ga_woc",
                     "ga_automl_450k",
                     "ga_automl_850k",
                     "ga_automl_850k_2",
                     "wsu_450k")

names(columns_to_plot)<- c("Robust Placental Clock",
                           "Control Placental Clock",
                           "Refined Robust Placental Clock",
                            "Team 1",
                           "Team 2",
                           "Team 3",
                           "Wisdom of Crowds",
                           "Autogluon",
                           "Autogluon (850K)",
                           "Autogluon (450K-2)",
                           "Post Challenge placental clock")

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


pdf("results/Figure 3.pdf", width = 10, height = 6)
#plots[["Team 1"]] + plots[["Team 2"]] + plots[["Team 3"]] + plot_annotation(tag_levels = 'A')
# ggarrange(plots[["Team 1"]] , plots[["Team 2"]] , plots[["Team 3"]] , labels = c("A", "B", "C"),  
#           ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

# Assuming plots["Team 2"] is your central plot
plots[["Team 2"]] <- plots[["Team 2"]] +
  theme(legend.position = c(0.85, 0.2),  # Center the legend inside the plot
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "lines"),
        legend.justification = c("center", "center"),  # Justify legend center to the coordinate
        legend.background = element_rect(fill = "white", colour = "white"))  # Optional: Add background

# Use ggarrange without common.legend since we moved the legend inside the center plot
arranged_plots<- ggarrange(
  plots[["Team 1"]]+ theme(legend.position = "none"),
  plots[["Team 2"]],
  plots[["Team 3"]]+ theme(legend.position = "none"),
  labels = c("A", "B", "C"),
  ncol = 3, nrow = 1
)

# Adding a common title using annotate_figure
final_plot <- annotate_figure(arranged_plots,
                              top = text_grob("Figure 3", 
                                              size = 14, face = "bold",
                                              hjust=0,x=0))
final_plot
dev.off()






pdf("results/Figure S1.pdf", width = 10, height = 6)
#plots[["Team 1"]] + plots[["Team 2"]] + plots[["Team 3"]] + plot_annotation(tag_levels = 'A')
figs1<- ggarrange(plots[["Robust Placental Clock"]] + 
                    theme(legend.position = "none",
                          plot.title = element_text(size = 11,hjust=0.5)),
          plots[["Control Placental Clock"]] +
            theme(legend.position = c(0.85, 0.2),  # Center the legend inside the plot
                  legend.text = element_text(size = 12),
                  legend.key.size = unit(1, "lines"),
                  
                  legend.justification = c("center", "center"),  # Justify legend center to the coordinate
                  legend.background = element_rect(fill = "white", colour = "white"),
                  plot.title = element_text(size = 11,hjust=0.5)),  # Optional: Add background
       
          plots[["Refined Robust Placental Clock"]] + 
            theme(legend.position = "none",
                  plot.title = element_text(size = 11,hjust=0.5)),
          labels = c("A", "B", "C"),  
          ncol=3, nrow=1)
figs1
dev.off()


pdf("results/Figure 5.pdf", width = 9, height = 6)
#plots[["Team 1"]] + plots[["Team 2"]] + plots[["Team 3"]] + plot_annotation(tag_levels = 'A')
fig5<- ggarrange(plots[["Wisdom of Crowds"]]  + theme(legend.position = "none"), 
          plots[["Autogluon"]]+
            theme(legend.position = c(0.85, 0.2),  # Center the legend inside the plot
                  legend.text = element_text(size = 12), 
                  legend.key.size = unit(1, "lines"),
                  legend.justification = c("center", "center"),  # Justify legend center to the coordinate
                  legend.background = element_rect(fill = "white", colour = "white")), 
          labels = c("A", "B"),  
          ncol=2, nrow=1)
# Adding a common title using annotate_figure
fig5 <- annotate_figure(fig5,
                              top = text_grob("Figure 5", 
                                              size = 14, face = "bold",
                                              hjust=0,x=0))
fig5

dev.off()


pdf("results/Figure 6.pdf", width = 8, height = 6)
#plots[["Team 1"]] + plots[["Team 2"]] + plots[["Team 3"]] + plot_annotation(tag_levels = 'A')
fig6<- plots[["Post Challenge placental clock"]]+
  labs(title="Post Challenge placental clock")+
  theme(legend.position = c(0.85, 0.2),  # Center the legend inside the plot
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, "lines"),
        legend.justification = c("center", "center"),  # Justify legend center to the coordinate
        legend.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(size=14,face="bold"))

# Adding a common title using annotate_figure
fig6 <- annotate_figure(fig6,
                        top = text_grob("Figure 6", 
                                        size = 14, face = "bold",
                                        hjust=0,x=0))
fig6
dev.off()




