# name: association_ega_acceleration_pregnancy_complications_pcpc.R
#
# author: Gaurav Bhatti
#
# input: data/processed/ano_all_predictions.csv        
#        data/prb/sample_metadata.csv
#       
# output:results/Figure 8.pdf



library(tidyverse)
library(here)
library(ggpubr)
library(broom)
library(cowplot)
library(car)
library(MASS)
library(emmeans)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(gridExtra)

#################### get annotation
ano<- read_csv(here("data/processed/ano_all_predictions.csv"))

####################get and merge additional maternal factors and fetal sex
metadata<- read_csv(here("data/prb/sample_metadata.csv")) |> 
  dplyr::select("EN_Main_Index","EN_Smoking","EN_smokingtxt","EN_Drugs","EN_Drug_txt",
                "EN_Pre_Preg_Wtkg","DL_Del_WeightKG","DL_Augmented","DL_Lab_Sp","SD_SGA",
                "NEO_SS_Comments","EN_Protocol_1","EN_Protocol_2","EN_Protocol_3")

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
                                      levels = c("0","1")),
         early_maternal_age=factor(if_else(Age<=18,1,0,NA),
                                   levels = c("0","1")),
         np=factor(if_else(Parity==0,1,0,NA),
                   levels = c("0","1")),
         
         sga=factor(if_else(Percentile<10,1,0),
                    levels = c("0","1")),
         weight_gain=DL_Del_WeightKG- EN_Pre_Preg_Wtkg,
         splabor=factor(ifelse(DL_Lab_Sp==1|DL_Augmented==1,1,0),
                        levels=c("0","1")))|> 
  mutate(PE=factor(ifelse(Group %in% c("pretermPE","termPE"),1,0),
                   levels = c("0","1")))



############### plot bw and bw percetile against eGA acceleration

###############Term 
df=ano[ano$Del_GA_Calc >=37,] 

#### eGA acceleration
df$y <- df$wsu_450k - df$Del_GA_Calc

df$fetal_sex_r=relevel(df$fetal_sex,ref = "1")

# BW Percetile
model <- lm( Percentile~y*(fetal_sex+Age + smoking   
                           + obesity +np+splabor+drugs), 
             data = df)
Anova(model)

df$Percentile_cat<- cut(df$Percentile,100*c(0,0.2,0.4,0.6,0.8,1))
# df |> 
#   filter(fetal_sex==1) |> 
  
# boxplot(y~Percentile_cat,data=df[df$fetal_sex=="0",])
  

selected_model <- lm( Percentile~y*fetal_sex+Age + smoking   
                      + obesity +np+splabor+drugs, 
                      data = df)
Anova(selected_model)

#tab_model(selected_model, transform = NULL, auto.label = FALSE)

selected_model <- lm( Percentile~y*fetal_sex_r+Age + smoking   
                      + obesity +np+splabor+drugs, 
                      data = df)
summary(selected_model)

#tab_model(selected_model, transform = NULL, auto.label = FALSE)

### Make a Plot
cols <- c("1" = "#D55E00", "0" = "#0072B2")
g_bwp<- df |> 
  ggplot(aes(x=y,y=Percentile,color=fetal_sex))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  scale_color_manual(name="Fetal Sex",values=cols,labels=c("Male","Female"))+
  labs(title = "Term placentas (n=171, GA≥ 37 )",
       x="eGA Acceleration",
       y="Birthweight Percentile")+
  theme_cowplot()+
  theme(legend.position = c(0.85, 0.85),  # Center the legend inside the plot
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, "lines"),
        legend.justification = c("center", "center"),  # Justify legend center to the coordinate
        legend.background = element_rect(fill = "white", colour = "white"))





# Preeclampsia
model <- glm(PE~ y*(fetal_sex+Age+ + smoking   
                    + obesity +np+splabor+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)

selected_model<- glm(PE~ y+fetal_sex+Age+ + smoking   
                     + obesity +np+splabor+drugs, 
                     data = df,
                     family=binomial(link="logit"))
Anova(selected_model)
summary(selected_model)
#tab_model(selected_model, transform = NULL, auto.label = FALSE)

##  SGA (n=50)
model <- glm(sga~ y*(fetal_sex+Age+ smoking   
                     + obesity +np+splabor+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)


selected_model<- glm(sga~ y*fetal_sex+Age+ smoking   
                     + obesity +splabor+np+drugs, 
                     data = df,family=binomial(link="logit"))

summary(selected_model)
Anova(selected_model)

#tab_model(selected_model, transform = NULL, auto.label = FALSE)
summary(selected_model)

selected_model<- glm(sga~ y*fetal_sex_r+Age+ smoking   
                     + obesity +splabor+np+drugs, 
                     data = df,family=binomial(link="logit"))
summary(selected_model)
#tab_model(selected_model, transform = NULL, auto.label = FALSE)


###############Preterm 
df=ano[ano$Del_GA_Calc <37,] |> 
  mutate(ptl=factor(if_else(Group=="PTL",1,0),levels = c("0","1")),
         pprom=factor(if_else(Group=="PPROM",1,0),levels = c("0","1")))

#### eGA acceleration
df$y <- df$wsu_450k - df$Del_GA_Calc

df$fetal_sex_r=relevel(df$fetal_sex,ref = "1")


# BW Percetile
model <- lm( Percentile~y*(fetal_sex+Age + smoking   
                           + obesity +np+splabor+drugs), 
             data = df)

Anova(model)

selected_model <- lm( Percentile~y*fetal_sex+Age + smoking   
                      + obesity +np+splabor+drugs, 
                      data = df)
AIC(selected_model)
Anova(selected_model)

summary(selected_model)
#tab_model(selected_model, transform = NULL, auto.label = F)


selected_model <- lm( Percentile~y+fetal_sex_r+Age + smoking   
                      + obesity +np+splabor+drugs, 
                      data = df)
#tab_model(selected_model, transform = NULL, auto.label = F)


cols <- c("1" = "#D55E00", "0" = "#0072B2")
pt_bwp<- df |> 
  ggplot(aes(x=y,y=Percentile,color=fetal_sex))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  scale_color_manual(name="Fetal Sex",values=cols,labels=c("Male","Female"))+
  labs(title = "Preterm placentas (n=213, GA<37)",
       x="eGA Acceleration ",
       y="Birthweight Percentile")+
  theme_cowplot()+
  theme(legend.position = c(0.85, 0.85),  # Center the legend inside the plot
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, "lines"),
        legend.justification = c("center", "center"),  # Justify legend center to the coordinate
        legend.background = element_rect(fill = "white", colour = "white"))


## PE
model <- glm(PE~ y*(fetal_sex+Age + smoking   
                    + obesity +np+splabor+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)
selected_model<- glm(PE~ y+(fetal_sex+Age+  smoking   
                            + obesity +np+splabor+drugs), 
                     data = df,family=binomial(link="logit"))
#tab_model(selected_model, transform = NULL, auto.label = FALSE)

Anova(selected_model)


## Preterm SGA (n=37)
model <- glm(sga~ y*(fetal_sex+Age+ smoking   
                     + obesity +np+splabor+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)
selected_model<- glm(sga~ y+(fetal_sex+Age+ smoking   
                             + obesity +np+splabor+drugs), 
                     data = df,family=binomial(link="logit"))
Anova(selected_model)
summary(selected_model)

#tab_model(selected_model, transform = NULL, auto.label = FALSE)




## Preterm labor (n=)
model <- glm(ptl~ y*(fetal_sex+Age+ smoking   
                     + obesity +np+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)
selected_model<- glm(ptl~ y+(fetal_sex+Age+ smoking   
                             + obesity +np+drugs), 
                     data = df,family=binomial(link="logit"))
Anova(selected_model)
summary(selected_model)

#tab_model(selected_model, transform = NULL, auto.label = FALSE)



## Preterm PPROM (n=75)
model <- glm(pprom~ y*(fetal_sex+Age+ smoking   
                       + obesity +np+splabor+drugs), 
             data = df,family=binomial(link="logit"))

Anova(model)
selected_model<- glm(pprom~ y+(fetal_sex+Age+ smoking   
                               + obesity +np+splabor+drugs), 
                     data = df,family=binomial(link="logit"))
Anova(selected_model)
summary(selected_model)
#tab_model(selected_model, transform = NULL, auto.label = FALSE)






pdf("results/Figure8.pdf",height=8,width=10)
fig8<- ggarrange(g_bwp+theme(legend.position = "none"),pt_bwp, labels = c("A", "B"),  
          ncol=2, nrow=1)
# Adding a common title using annotate_figure
fig8 <- annotate_figure(fig8,
                        top = text_grob("Figure 8", 
                                        size = 14, face = "bold",
                                        hjust=0,x=0))
fig8

dev.off()










