# name: plot_heatmap_challenge.R
#
# author: Gaurav Bhatti
#
# input: data/dream_challenge/Beta_normalized_subchallenge2.csv
#        data/dream_challenge/Sample_annotation_metadata.csv
#        data/dream_challenge/Probe_array.csv
#        
#        
# output: Heatmap Visualization of 850k Challenge Data
#         results/heatmap_sc2.pdf

rm(list = ls())
library(tidyverse)
library(vroom)
library(pheatmap)
library(scico)

## Read the SC1 raw data
beta_sc1 <- vroom("data/dream_challenge/Beta_normalized_subchallenge2.csv") |> 
  rename("cpg_name" = `...1`)

## Read the annotation for the public data
ano_challenge <- read_csv("data/dream_challenge/Sample_annotation_metadata.csv") |> 
  select(Sample_ID, GA) |> 
  filter(Sample_ID %in% colnames(beta_sc1))

## Ensure GA is numeric
ano_challenge$GA <- as.numeric(ano_challenge$GA)

## Check GA range
print(summary(ano_challenge$GA))

## Read probe specificity data
probe_data <- read_csv("data/dream_challenge/Probe_array.csv") |> 
  select("Name", "Array_specificity") |> 
  mutate(Array = if_else(Array_specificity == "Common to 450K & 850K", "Shared_probes", "850K_specific"))

## Calculate correlation between each CpG probe and gestational age
cor_df <- beta_sc1 |> 
  pivot_longer(-cpg_name, names_to = "Sample_ID", values_to = "Beta") |> 
  left_join(ano_challenge, by = "Sample_ID") |> 
  group_by(cpg_name) |> 
  summarize(correlation = cor(Beta, GA, use = "complete.obs"), .groups = "drop") |> 
  left_join(probe_data, by = c("cpg_name" = "Name"))

## Select the top 4000 CpG probes by absolute correlation
top_probes <- cor_df |> 
  group_by(Array, sign(correlation)) |> 
  arrange(desc(abs(correlation))) |> 
  slice_head(n = 100) |> 
  ungroup()

## Filter beta_sc1 for only the top probes
beta_sc1_select <- beta_sc1 |> 
  filter(cpg_name %in% top_probes$cpg_name)

## Ensure no missing values in beta_matrix
beta_matrix <- beta_sc1_select |> 
  column_to_rownames(var = "cpg_name") |> 
  select(all_of(ano_challenge$Sample_ID)) |> 
  data.matrix()

# Ensure all values are finite (no Inf, -Inf)
range(beta_matrix)



## Order columns (samples) by gestational age
ordered_samples <- ano_challenge |>  arrange(GA) |> pull(Sample_ID)

## Order rows (probes) by array specificity and correlation
ordered_cpgs <- cor_df |> 
  filter(cpg_name %in% beta_sc1_select$cpg_name) |> 
  arrange(desc(Array), desc(correlation))

beta_matrix <- beta_matrix[ordered_cpgs$cpg_name, ordered_samples]

## Create row annotation: Probe specificity and correlation
row_annotation <- data.frame(
  Array = factor(ordered_cpgs$Array, levels = c("Shared_probes", "850K_specific")),
  Correlation = ordered_cpgs$correlation
)
rownames(row_annotation) <- ordered_cpgs$cpg_name

## Create column annotation: Gestational age
col_annotation <- ano_challenge |> 
  column_to_rownames(var = "Sample_ID")
col_annotation<-data.frame(GA=col_annotation[ordered_samples,])
rownames(col_annotation)<-ordered_samples

## Define colors for annotations
annotation_colors <- list(
  Array = c("Shared_probes" = "blue", "850K_specific" = "green"),
  Correlation = colorRampPalette(c("#008080", "white", "#8B0000"))(100),
  GA = colorRampPalette(c("yellow", "orange", "red"))(100)
)

## Define the custom color palette for beta values
custom_colors <- colorRampPalette(c("blue", "white", "red"))(100)

pdf("results/heatmap_sc2.pdf")
pheatmap(beta_matrix, 
         color = custom_colors, 
         scale = "none",
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         show_rownames = FALSE, 
         show_colnames = FALSE,
         annotation_row = row_annotation, 
         annotation_col = col_annotation, 
         annotation_colors = annotation_colors)
dev.off()

