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
ano<- read_csv(here("data/processed/ano_all_predictions.csv"))

metadata<- read_csv(here("data/prb/sample_metadata.csv")) |> 
  select("EN_Main_Index","EN_Smoking","EN_smokingtxt","EN_Drugs","EN_Drug_txt")


test_ranking<- read_csv(here("data/submissions/Test_data_evaluation.csv"))
top_teams <- test_ranking |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  slice_min(Test_rmse, n = 2, with_ties = FALSE) |> 
  mutate(stamp = paste("ga", submitterid, evaluationid, sep = "_")) |> 
  pull(stamp)


ano<- ano |> 
  left_join(metadata,by=c("Main_Index"="EN_Main_Index"),keep=F) |> 
  mutate(smoking=factor(if_else(EN_Smoking==3,1,0,NA),levels = c("0","1")),
         drugs=factor(if_else(EN_Drugs==3,1,0,NA),levels=c("0","1")),
         fetal_sex= factor(if_else(Sex=="Female",1,0,NA),levels=c("0","1")),
         maternal_age=Age,
         weight=Pre_Preg_Wtlb,
         height=HGTinch)




fit_ega_model <- function(clock) {
data=ano
# Calculate EGA acceleration: Epi GA - Chronological GA
data[[paste(clock, "acceleration", sep = "_")]] <- data[[clock]] - data$Del_GA_Calc
# Model formula
formula <- as.formula(paste0(clock, "_acceleration ~ smoking + drugs + fetal_sex + maternal_age + weight + height"))
# Fit linear model
model <- lm(formula, data = data)
summary_df <- tidy(model,conf.int=TRUE)
summary_df$clock <- clock

return(summary_df)
}



clocks <- c("ga_rpc", "ga_cpc", "ga_rrpc",top_teams,"ga_woc" )  
titles<- c("RPC","CPC","RRPC","Top#1","Top#2","WOC")
names(titles)<-clocks
# Use map_df to iterate over clocks and bind the results into one dataframe
model_results_df <- map_df(clocks, fit_ega_model) |> 
  mutate(title=titles[clock])



# Define a mapping of original terms to shortened terms
short_names <- c(
  maternal_age = "Age",
  fetal_sex1 = "Gender",
  smoking1 = "Smoke",
  weight = "Wgt (lb)",
  height = "Hgt (inch)",
  drugs1 = "Drugs"
)

# Assuming model_results_df is your dataframe
model_results_df <- model_results_df |> 
  mutate(short_term = if_else(term %in%  names(short_names), 
                              short_names[term], term),
         short_term= factor(short_term,levels = c("Age","Wgt (lb)","Hgt (inch)",
                                      "Smoke","Drugs","Gender"))) 



pdf("results/plot_association_ega_acceleration_environmental_exposures.pdf",width=9)
model_results_df %>%
  filter(term != "(Intercept)") |> 
  ggplot(aes(x = short_term, y = estimate, 
             ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size=.5) +
  geom_hline(yintercept = 0,linetype=2,size=0.1)+
  facet_grid(~title, scales = "free_x", space = "free_x", switch = "x") +  # Adjust facets to line up horizontally
  theme_minimal() +
  labs(x="Environmental and biological factors",
       y="Change in eGA acceleration per unit or category change")+
  theme(
    #axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  coord_flip()

dev.off()






