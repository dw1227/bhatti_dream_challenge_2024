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
  select("EN_Main_Index","PH_RTVI_C1","PH_RCVI_C2","PH_ISK_C3","PH_VA_C4",
         "PH_IIF_C5","PH_DVH_C7","PH_PMB_D1","PH_MHD_D2","PH_ABP_D3",
         "PH_DVthromb","PH_SAfibrinoidnec","PH_Endovt","PH_RetroPLhema",
         "PH_AVM","PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg",
         "PH_G1_ST11","PH_G2_ST12","PH_G3_ST21","PH_G4_ST22","PH_G5_CPI",
         "PH_H_CD","PH_HistioIV","PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg",
         "PH_VCH_E1","PH_VCH_E2","PH_VCH_E3","PH_VCH_E4","PH_TLF_E5",
         "PH_IFC_E6","PH_FSI_E7","PH_VasEctasia","PH_CMR_A12345","PH_CFR_B1234",
         "PH_DVM")

ano<- ano |> 
  left_join(metadata,by=c("Main_Index"="EN_Main_Index"),keep=F)

myf0=function(x){ifelse(length(x[!is.na(x)])==0,NA,sum(abs(x[!is.na(x)])))}
# ##Maternal vascular malperfusion lesion (MVM)
ano$MaternalVascularMalperfusion=ifelse(apply(ano[, c("PH_RTVI_C1","PH_RCVI_C2","PH_ISK_C3","PH_VA_C4",
                                                             "PH_IIF_C5","PH_DVH_C7","PH_PMB_D1","PH_MHD_D2",
                                                             "PH_ABP_D3","PH_DVthromb","PH_SAfibrinoidnec",
                                                             "PH_Endovt","PH_RetroPLhema","PH_AVM")],1,myf0)>=1,1,0)

#  ##Chronic inflammatory lesions (CI). 
ano$ChronicInflammatory =ifelse(apply(ano[, c("PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg",
                                                      "PH_G1_ST11","PH_G2_ST12","PH_G3_ST21",
                                                      "PH_G4_ST22","PH_G5_CPI","PH_H_CD","PH_HistioIV")],1,myf0)>=1,1,0) 


##sub-categories of Chronic Inflammatory Lesions
ano$ChronicVillitis=ifelse(apply(ano[,c("PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg")],1,myf0)>=1,1,0) 


ano$ChronicChorioamnionitis=ifelse(apply(ano[,c("PH_G1_ST11","PH_G2_ST12","PH_G3_ST21","PH_G4_ST22","PH_G5_CPI")],1,myf0)>=1,1,0)

# 
# dat$HCD=ifelse(dat[, c("H_CD")]==0,0,1) 
# dat$HistiocyticIntervillositis=ifelse(dat[, c("HistioIV")]==0,0,1)
# table(dat$HistiocyticIntervillositis);table(is.na(dat$HistiocyticIntervillositis)) 

# ##Fetal Vascular Malperfusion (FVM)
ano$FetalVascularMalperfusion=ifelse(apply(ano[, c("PH_VCH_E1",
                                                   "PH_VCH_E2",
                                                   "PH_VCH_E3",
                                                   "PH_VCH_E4",
                                                   "PH_TLF_E5",
                                                   "PH_IFC_E6",
                                                   "PH_FSI_E7",
                                                   "PH_VasEctasia")],1,myf0)>=1,1,0)

# ##maternal placental inflammatory lesion (MIR)
ano$AcuteMaternalInflammatory=ifelse(ano$PH_CMR_A12345==0,0,1)  #=acute histologic chorioamnionitis  

# ##fetal placental inflammatory lesion (FIR)
ano$AcuteFetalInflammatory=ifelse(ano$PH_CFR_B1234==0,0,1) #=funisitis at any site

# summary(lm(I(wsu_450k-Del_GA_Calc)~PH_AVM+PH_DVM+Del_GA_Calc,data=ano[ano$Del_GA_Calc<37,]))
# ga_3505704_9615595
# ga_3506332_9615596

ano<- ano |> 
  drop_na(PH_AVM) |> 
  drop_na(PH_DVM) |> 
  select(- c("PH_RTVI_C1","PH_RCVI_C2","PH_ISK_C3","PH_VA_C4",
                  "PH_IIF_C5","PH_DVH_C7","PH_PMB_D1","PH_MHD_D2","PH_ABP_D3",
                  "PH_DVthromb","PH_SAfibrinoidnec","PH_Endovt","PH_RetroPLhema",
                  "PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg",
                  "PH_G1_ST11","PH_G2_ST12","PH_G3_ST21","PH_G4_ST22","PH_G5_CPI",
                  "PH_H_CD","PH_HistioIV","PH_F1_Blg","PH_F2_Bhg","PH_F3_Plg","PH_F4_Phg",
                  "PH_VCH_E1","PH_VCH_E2","PH_VCH_E3","PH_VCH_E4","PH_TLF_E5",
                  "PH_IFC_E6","PH_FSI_E7","PH_VasEctasia","PH_CMR_A12345","PH_CFR_B1234")) 


######################################################################################


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
formula <- as.formula(paste0(clock, "_acceleration ~ PH_DVM + PH_AVM + Del_GA_Calc"))
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
  mutate(title=titles[clock]) |> 
  filter(term!="Del_GA_Calc") 

# Define a mapping of original terms to shortened terms
short_names <- c(
  PH_DVM= "Delayed Villous\nMaturation",
  PH_AVM = "Accelerated Villous\nMaturation"
)

# Assuming model_results_df is your dataframe
model_results_df <- model_results_df |> 
  mutate(short_term = if_else(term %in%  names(short_names), 
                              short_names[term], term),
         short_term= factor(short_term,levels = c("Accelerated Villous\nMaturation","Delayed Villous\nMaturation"))) 



pdf("results/plot_association_ega_acceleration_villous_maturation.pdf",width=9)
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






