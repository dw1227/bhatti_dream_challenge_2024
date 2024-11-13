# name: association_ega_acceleration_envrionmental_exposures.R
#
# author: Gaurav Bhatti
#
# input: data/processed/ano_all_predictions.csv        
#        data/prb/sample_metadata.csv
#        data/submissions/Test_data_evaluation.csv
#       
# output: plot of coefficients derived from the model
#         results/plot_association_ega_acceleration_environmental_exposures.pdf
     


library(tidyverse)
library(here)
library(ggpubr)
library(broom)
library(cowplot)
ano<- read_csv(here("data/processed/ano_all_predictions.csv"))

metadata<- read_csv(here("data/prb/sample_metadata.csv")) |> 
  select("EN_Main_Index","EN_Smoking","EN_smokingtxt","EN_Drugs","EN_Drug_txt")

ano<- read_csv("data/processed/ano_all_predictions.csv")

ano<- ano |> 
  left_join(metadata,by=c("Main_Index"="EN_Main_Index"),keep=F) |> 
  mutate(smoking=factor(if_else(EN_Smoking==3,1,0,NA),levels = c("0","1")),
         drugs=factor(if_else(EN_Drugs==3,1,0,NA),levels=c("0","1")),
         fetal_sex= factor(if_else(Sex=="Female",1,0,NA),levels=c("0","1")),
         weight=Pre_Preg_Wtlb,
         height=HGTinch,
         bmi=(703*weight/(height)^2),
         obesity=factor(if_else(bmi>=30,1,0,NA),levels = c("0","1")),
         advanced_maternal_age=factor(if_else(Age>=35,1,0,NA),
                                      levels = c("0","1")))


test_ranking<- read_csv(here("data/submissions/Test_data_evaluation.csv"))
top_teams <- test_ranking |>
  group_by(submitterid) |>
  slice_min(Test_rmse, n = 1, with_ties = FALSE) |>
  ungroup() |>
  slice_min(Test_rmse, n = 2, with_ties = FALSE) |>
  mutate(stamp = paste("ga", submitterid, evaluationid, sep = "_")) |>
  pull(stamp)







fit_ega_model <- function(clock) {
data=ano
# Calculate EGA acceleration: Epi GA - Chronological GA
data[[paste(clock, "acceleration", sep = "_")]] <- data[[clock]] - data$Del_GA_Calc
# Model formula
formula <- as.formula(paste0(clock, "_acceleration ~ smoking + drugs + fetal_sex + advanced_maternal_age + obesity"))
# Fit linear model
model <- lm(formula, data = data)
summary_df <- tidy(model,conf.int=TRUE)
summary_df$clock <- clock

return(summary_df)
}



clocks <- c("ga_rpc", "ga_cpc", "ga_rrpc",top_teams,"wsu_450k" )  
titles<- c("RPC","CPC","RRPC","Team 1","Team 2","WSPC")
names(titles)<-clocks
# Use map_df to iterate over clocks and bind the results into one dataframe
model_results_df <- map_df(clocks, fit_ega_model) |> 
  mutate(title=titles[clock])



# Define a mapping of original terms to shortened terms
short_names <- c(
  advanced_maternal_age1= "Advanced \nmaternal age",
  fetal_sex1 = "Fetal sex \n(female vs male)",
  smoking1 = "Smoking",
  obesity1 = "Obesity",
  drugs1 = "Drug use"
)

# Assuming model_results_df is your dataframe
model_results_df <- model_results_df |> 
  mutate(short_term = if_else(term %in%  names(short_names), 
                              short_names[term], term),
         short_term= factor(short_term,levels = c("Advanced \nmaternal age","Obesity",
                                      "Smoking","Drug use","Fetal sex \n(female vs male)"))) 



pdf("results/plot_association_ega_acceleration_environmental_exposures.pdf",width=9)
model_results_df %>%
  filter(term != "(Intercept)") |> 
  ggplot(aes(x = short_term, y = estimate, 
             ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size=.5) +
  geom_hline(yintercept = 0,linetype=2,size=0.1)+
  facet_grid(~title, scales = "free_x", space = "free_x", switch = "x") +  # Adjust facets to line up horizontally
  theme_cowplot() +
  labs(x="",
       y="Change in eGA acceleration (weeks)")+
  theme(
    #axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  coord_flip()

dev.off()






