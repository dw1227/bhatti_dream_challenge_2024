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

library(tidyverse)
library(glmnet)
library(org.Hs.eg.db)
library(AnnotationDbi)
source("code/ora.R")

## Read the SC1 raw data
load("data/dream_challenge/beta_public_normalized_sc1.Rdata")


## Read the annotation for the public data
ano_challenge <- read_csv("data/dream_challenge/Sample_annotation_metadata.csv") |> 
  select(Sample_ID=Sample_Name, GA) |> 
  filter(Sample_ID %in% colnames(beta_norm_BMIQ))

## Ensure GA is numeric
ano_challenge$GA <- as.numeric(ano_challenge$GA)

## Check GA range
print(summary(ano_challenge$GA))


## Calculate correlation between each CpG probe and gestational age
cor_df <- beta_norm_BMIQ|> 
  data.frame() |> 
  rownames_to_column(var = "cpg_name") |> 
  pivot_longer(-cpg_name, names_to = "Sample_ID", values_to = "Beta") |> 
  left_join(ano_challenge, by = "Sample_ID") |> 
  group_by(cpg_name) |> 
  summarize(correlation = cor(Beta, GA, use = "complete.obs"), .groups = "drop") 


# epic_annotation <- read_csv("data/dream_challenge/Probe_annotation.csv", col_select = -1) |> as.data.frame()
# rownames(epic_annotation) <- epic_annotation$Name
epic_annotation<- read_delim("data/probe_annotation/GSE198627_GEO_Methylation850k_PLATFORMv2.1.txt",
                             skip = 26)



# Wayne state 450k clock
load("data/clocks/wsu_pl_clock_450k.Rdata")
coefficients<-data.frame(data.matrix(coef(best_model,s=best_model$lambda.1se)))

all_cpgs<-rownames(coefficients)[-1]
target_cpgs<- rownames(coefficients |> filter(s1!=0))[-1]





# Load and prepare annotation data (assuming 'annotation_data' is loaded with the EPIC annotation file)
# Filter relevant columns: 'ID', 'Regulatory_Region' (or a similar region column)

annotation_data <- epic_annotation |> 
  select(ID, Regulatory_Region,
         Global_TSS_transcript_type,
         Relation_to_CpG_Island) |> 
  filter(ID %in% all_cpgs) 
  # drop_na(UCSC_RefGene_Group) |> 
  # separate_rows(UCSC_RefGene_Group,sep = ";") |> 
  # distinct()

# Define target_ids and universe_ids (CpG probes)
target_ids <- target_cpgs[target_cpgs%in% annotation_data$ID]     # Vector of target CpG probes
universe_ids <- all_cpgs [all_cpgs%in% annotation_data$ID]       # Vector of all CpG probes


# Create id_sets for ORA based on genomic region annotations
id_sets <- annotation_data |> 
  group_by(Regulatory_Region) |> 
  summarize(probes = list(ID)) |> 
  deframe()

non_coding_sets <- annotation_data |> 
   drop_na(Global_TSS_transcript_type) |> 
   separate_rows(Global_TSS_transcript_type,sep = ";") |> 
   distinct() |> 
  filter(Global_TSS_transcript_type %in% c("eRNA","lncRNA","small-NC")) |> 
  group_by(Global_TSS_transcript_type) |> 
  summarize(probes = list(ID)) |> 
  deframe()


cpg_island_sets <-annotation_data |> 
  drop_na(Relation_to_CpG_Island) |> 
  distinct() |> 
  filter(Relation_to_CpG_Island %in% c("Island","Open.Sea","Shore")) |> 
  group_by(Relation_to_CpG_Island) |> 
  summarize(probes = list(ID)) |> 
  deframe()


id_sets<- c(id_sets,non_coding_sets,cpg_island_sets)
  
# Extract region names for set_names
set_names <- names(id_sets)

# Run ORA with the defined sets
results <- ora(target_ids= cor_df |> 
                 filter(cpg_name %in% target_ids) |> 
                 filter(abs(correlation)>0) |> pull(cpg_name), 
               universe_ids=universe_ids,
               id_sets=id_sets, 
               set_names=set_names)

# View results
print(results[,1:5])




## extract the list of genes associated with genes increasing in methylation
get_entrez_genes<- function(cpgs,epic_annotation)
{
annotation_data <- epic_annotation |>   
  filter(ID %in% cpgs) |> 
  dplyr::select(Global_enhancer_targets_HGNC,
         Global_TSS_associated_transcript_HGNC,
         Global_GeneBody_HGNC) |> 
  unite("genes_combined",c(Global_enhancer_targets_HGNC,
        Global_TSS_associated_transcript_HGNC,
        Global_GeneBody_HGNC),sep=";",na.rm=T,remove = F) |> 
  separate_rows(genes_combined,sep=";") |> 
  filter(genes_combined!="unknown") |> 
  distinct(genes_combined)

entrez_ids<- na.omit(unique(unlist(AnnotationDbi::mapIds(org.Hs.eg.db,
                    keys=annotation_data$genes_combined,
                    column="ENTREZID",
                    keytype="SYMBOL",
                    multiVals="first") )))
return(entrez_ids)

}

cpgs<-  cor_df |> 
  filter(cpg_name %in% target_ids) |> 
  pull(cpg_name)

genes<- get_entrez_genes(cpgs,epic_annotation)




write.csv(genes,file="data/clocks/genes_cpgs_all_wsu.csv")
