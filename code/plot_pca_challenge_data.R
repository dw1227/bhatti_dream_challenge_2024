# name: plot_pca_challenge.R
#
# author: Gaurav Bhatti
#
# input: data/dream_challenge/Leaderboard_beta_subchallenge1.csv
#        data/dream_challenge/Beta_raw_subchallenge1.csv
#        data/prb/beta_raw.Rdata
#        data/dream_challenge/Sample_annotation_metadata.csv
#        
# output: PCA Visualization of Challenge Data
#         results/pca_sc1_leaderboard_test.pdf



rm(list=ls())
library(tidyverse)
library(vroom)
library(irlba)
library(scico)
library(WGCNA)

## read the leaderboard data
beta_leaderboard<-vroom("data/dream_challenge/Leaderboard_beta_subchallenge1.csv",
                        delim=",") |> 
  rename("cpg_name"= `...1`)

## read the SC1 raw data
beta_sc1<-vroom("data/dream_challenge/Beta_raw_subchallenge1.csv") |> 
  rename("cpg_name"= `...1`) |> 
## left join the two data sets
  left_join(beta_leaderboard,by="cpg_name")

## read the test data
load("data/prb/beta_raw.Rdata")
## create the combined challenge dataset
beta_challenge<-data.frame(beta_test) %>% 
  mutate("cpg_name"= rownames(.)) |> 
  as_tibble() |> 
  ## right join with the public dataset
  right_join(beta_sc1,by="cpg_name")


# garbage collection
rm(beta_sc1,beta_leaderboard,beta_test)


## read the annotation for the public data
ano_challenge<- read_csv("data/dream_challenge/Sample_annotation_metadata.csv") |> 
  ## keep sample id, GA, and study id
  select(Sample_ID,Dataset_ID,GA) |> 
  ## merge with test data
  bind_rows(
    ## read the annotation for the test data
    ano |> 
      ## keep sample id, GA, and create study id
      select(Sample_ID=Sample,GA=Del_GA_Calc) |> 
      mutate(Dataset_ID="WSU")) |> 
  remove_rownames() |> 
  column_to_rownames(var="Sample_ID")
  



beta_challenge<- beta_challenge |> 
  select(rownames(ano_challenge)) 

# PCA
p<- prcomp_irlba(t(beta_challenge),n=2)

# Calculate explained variance for axis labels
pca_var <- round(100 * (p$sdev^2) / p$totalvar, 1)  # Explained variance percentages


# Biplot
pdf("results/pca_sc1_leaderboard_test.pdf",height=8)
p$x |>  
  as_tibble() |> 
  mutate(Sample_ID = colnames(beta_challenge)) |>
  left_join(ano_challenge |> rownames_to_column(var = "Sample_ID"),
            by = "Sample_ID") |> 
  mutate(Group = if_else(Dataset_ID == "WSU", "WSU", "Public")) |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Dataset_ID,
                 color=Dataset_ID,
                 shape = Group,
                 alpha=GA,size=GA)) +  
  scale_fill_scico_d(palette = "romaO")+
  scale_color_scico_d(palette = "romaO")+
  scale_shape_manual(values = c("Public" = 21, "WSU" = 24)) +  
  scale_alpha_continuous(range=c(0.2,0.9))+
  scale_size_continuous(range=c(2,6))+
  labs(x = paste0("PC1 (", pca_var[1], "% variance)"),
       y = paste0("PC2 (", pca_var[2], "% variance)"),
       alpha = "Gestational Age",
       size="Gestational Age",
       fill = "",color="",shape="") +
  theme_classic()



dev.off()

 


