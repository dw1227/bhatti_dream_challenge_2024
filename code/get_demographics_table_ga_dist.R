# name: get_demographics_table_ga_dist.R
#
#
# author: Gaurav Bhatti
#
# input: data/prb/anoall.csv
#        data/prb/sample_metadata.csv
#        
# output: demographics                                                      
#         results/demographics_test.csv


rm(list=ls())
library(tidyverse)
library(here)
library(ggridges)
library(cowplot)

ano<- read_csv(here("data/prb/anoall.csv"))

metadata<- read_csv(here("data/prb/sample_metadata.csv")) |> 
  select("EN_Main_Index","EN_Smoking","EN_smokingtxt","EN_Drugs","EN_Drug_txt",
         "EN_ML_Race")




pdata<- ano |> 
  left_join(metadata,by=c("Main_Index"="EN_Main_Index"),keep=F) |> 
  mutate(smoking=factor(if_else(EN_Smoking==3,1,0,NA),levels = c("0","1")),
         drugs=factor(if_else(EN_Drugs==3,1,0,NA),levels=c("0","1")),
         fetal_sex= factor(if_else(Sex=="Female",1,0,NA),levels=c("0","1")),
         maternal_age=Age,
         weight=Pre_Preg_Wtlb,
         height=HGTinch,
         race=EN_ML_Race,
         Nulliparous=factor(if_else(Parity==0,1,0,NA),levels = c("0","1")) ) |> 
  select(smoking,drugs,fetal_sex,maternal_age,weight,height,BMI,Del_GA_Calc,
         Group=Group2,Baby_Weight,Percentile,race,Nulliparous,smoking,drugs)
  


cv= c("maternal_age","BMI","Del_GA_Calc",
      "Baby_Weight","Percentile")


res=NULL;
for ( i in 1:length(cv))
{
  temp=pdata[,c("Group",cv[i])] |> data.frame()
  y=as.numeric(as.character(temp[,2]))
  x=factor(temp[,1],levels = c("Controls","PTL",
                               "PPROM","pretermPE","pretermSGA",
                               "termPE","termSGA"))
  test=oneway.test(y~x,var.equal = F)
  median= round(tapply(y,x,median,na.rm=T),1)
  ranges= unlist(lapply(tapply(y,x,function(x){round(quantile(x,c(0,1),na.rm=T),1)}),
                        function(x){paste0("(",paste(x,collapse="-"),")")}))
  med=paste0(median,ranges)
  N_obs=sum(!is.na(y))
  N_obs=paste0(N_obs,"(",paste(tapply(y,x,function(x){sum(!is.na(x))}),collapse =";"),")")
  tmp=c(cv[i],N_obs,med,pval=test$p.value)
  res=rbind(res,tmp)
}
colnames(res) = c("Variable","N_observations",
                  names(median),"p")



################Categorical Variables
pdata$AAx=ifelse(pdata$race=="Black or African American",1,0)

variables= c("Nulliparous","AAx","smoking","drugs","fetal_sex")
variables=variables[variables %in% colnames(pdata)]
for ( i in 1:length(variables))
{
  temp=pdata[,c("Group",variables[i])] |> data.frame()
  
  y=factor(as.numeric(as.character(temp[,2])),levels=c("1","0"))
  x=factor(temp[,1],levels = c("Controls","PTL",
                               "PPROM","pretermPE","pretermSGA",
                               "termPE","termSGA"))
  tab= table(y,x)[c("1","0"),]
  counts=paste0(tab[1,],"/",colSums(tab),"(",round(100*prop.table(tab,2)[1,],1),"%)")
  
  
  test=fisher.test(tab,simulate.p.value = T)
  
  
  n= tapply(y,x,function(x){sum(x=="1",na.rm=T)})
  N= tapply(y,x,function(x){sum(!is.na(x))})
  N_obs=sum(!is.na(y))
  N_obs=paste0(N_obs,"(",paste(tapply(y,x,function(x){sum(!is.na(x))}),collapse =";"),")")
  
  tmp=c(variables[i],N_obs,counts,test$p.value)
  res=rbind(res,tmp)
}


res=data.frame(res,stringsAsFactors = F)
res$p=as.numeric(as.character(res$p))
res$p=ifelse(res$p<0.001,"<0.001",round(res$p,3))

rownames(res)=res$Variable
# res=res[c("Age", "Nulliparous","Abortion",
#           "Amnio_GA_Calc","AF_WBC","IL6_.pg.ml",
#           "Csec","birwei","apgar5","cervdil"),]


res=res[c("maternal_age","AAx","Nulliparous","smoking","drugs","BMI","Del_GA_Calc",
          "Baby_Weight","fetal_sex"),]

res$Variable<- c("Age","African American","Nulliparous","Smoking","Drugs",
                 "BMI","Gestational age at delivery","Birthweight","Fetal sex")


write.csv(res,"results/demographics_test.csv")



# Plot the distribution of gestational ages 
ano<- read_csv("data/dream_challenge/Sample_annotation_metadata.csv") |> 
  mutate(split="Public data") |> 
  select(Group=Dataset_ID,GA,split) |> 
  bind_rows(ano |> select(Group=Group2,GA=Del_GA_Calc) |> 
              mutate(split="Test data"))


pdf("results/Test_training_datasets_GA_distribution.pdf",width=9)
ano |>  
  group_by(Group) |> 
  mutate(mean_GA=mean(GA)) |> 
  ungroup() %>%
  mutate(Group=fct_reorder(Group,mean_GA)) |> 
  ggplot(aes(x = GA, y = Group, fill = Group)) +
  geom_density_ridges(
    aes(point_color = Group, point_fill = Group),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  theme_cowplot()+
  labs(x="Gestational Age (weeks)",
       y="",
       title = "Distribution of gestational age")+
  theme(legend.position = "none")+
  facet_wrap(~split,scales = "free")

dev.off()

