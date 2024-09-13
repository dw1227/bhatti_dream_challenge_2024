# name: generate_parity_plots_predictions.R
#
# author: Gaurav Bhatti
#
# input: data/prb/beta_complete_test_bmiq.Rdata
#        data/prb/anoall.csv
#        data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        data/submissions/all_predictions.csv
#        
# output: predicted vs reported GA for RPC, top performers and WOC solution
#         results/Parity_plots.pdf




rm(list=ls())
library(planet)
library(tidyverse)
library(Metrics)
library(rvest)


# Define the function to extract username from the webpage
extract_username <- function(id) {
  # Construct URLs for both profile and team
  profile_url <- paste0("https://www.synapse.org/Profile:", id)
  team_url <- paste0("https://www.synapse.org/Team:", id)
  
  # Function to check if a page is a valid profile
  is_valid_profile <- function(webpage) {
    !grepl("Synapse | Sage Bionetworks", webpage %>% html_text())
  }
  
  # Function to fetch and extract username
  fetch_username <- function(url) {
    webpage <- read_html(url)
    username <- webpage %>%
      html_text() %>%
      str_split("\n") %>%
      unlist() %>%
      .[1]
    # str_extract("\\((.*?)\\)") %>%
    # str_remove_all("[()]")
    return(username)
  }
  
  # Try to fetch username from Profile URL
  webpage <- read_html(profile_url)
  if (is_valid_profile(webpage)) {
    return(fetch_username(profile_url))
  }
  
  # If not a valid profile, try the Team URL
  return(fetch_username(team_url))
}


## load test data and annotation
load("data/prb/beta_complete_test_bmiq.Rdata")
# load("data/prb/beta_raw.Rdata")

ano<- read_csv("data/prb/anoall.csv") |> 
  data.frame() |> 
  column_to_rownames("Sample")

beta_norm_BMIQ<- beta_norm_BMIQ[,rownames(ano)]

# beta_test<- beta_test[,rownames(ano)]



all(rownames(ano)==colnames(beta_norm_BMIQ))
# all(rownames(ano)==colnames(beta_test))


ano <- ano %>%
  mutate(
    ga_rpc= predictAge(beta_norm_BMIQ, type = "RPC"),
    Sample= rownames(ano),
    # ga_RPC_raw = predictAge(beta_test, type = "RPC")
    ga_cpc = predictAge(beta_norm_BMIQ, type = "CPC"),
    ga_rrpc= predictAge(beta_norm_BMIQ, type = "RRPC")
  )


#### extract GA for top performers
df_test <- read_csv("data/submissions/Test_data_evaluation.csv")  |>  
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse,n=1,with_ties = FALSE) |> 
  ungroup() |> 
  mutate(prediction_file = file.path("data/submissions", 
                                paste0("output_run_", submission_stamp),"predictions.csv")) |> 
  mutate(Submitter  = map_chr(submitterid, extract_username))


# run this once to generate all_predictions. I may move this later to a 
# separate script.

# prediction_files<- df_test$prediction_file
# names(prediction_files)<- paste0("ga_",df_test$submission_stamp)
# 
#  map_dfr(.x=prediction_files,
#         .f=read_csv,
#         .id= "submission_stamp",
#         col_types=c("cd")) |> 
#   pivot_wider(names_from = submission_stamp,
#               values_from = "GA_prediction") |> 
#    write_csv("data/submissions/all_predictions.csv")
# 

 all_predictions<- read_csv("data/submissions/all_predictions.csv")
### Merge with annotation
ano<- ano |> left_join(all_predictions,by=c("Sample"="ID"))


# wisdom of crowd
df <- read_csv("data/submissions/Job-393694313420778661233189284.csv")
# woc_candidates<- df |>   
#   filter(submission_status == "SCORED") |> 
#   filter(!createdBy %in% c("1420476","3379638","3453428","3503156",
#                            "3504542","3505047","3505276")) |> 
#   group_by(submitterid, evaluationid) |> 
#   mutate(submission_datetime = as.POSIXlt(createdOn/1000, origin = "1970-01-01")) |> 
#   slice(which.max(submission_datetime)) |> 
#   group_by(submitterid) |> 
#   slice_min(rmse,n=1,with_ties = FALSE) |> 
#   ungroup() |> 
#   filter(rmse<2) |> 
#   mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
#   pull(submission_stamp)

top_performers <- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(submission_stamp)
names(top_performers)<- df_test %>%
  slice_min(Test_rmse, n = 3) %>%
  pull(Submitter)
woc_candidates <-  top_performers
ano$ga_woc<- ano |> 
  select(paste0("ga_",woc_candidates)) |> 
  rowwise() |> 
  mutate(ga_woc = mean(c_across(everything()), na.rm = TRUE)) |> 
  pull(ga_woc)



# Define a function to create a plot for each GA column
plot_ga_predictions <- function(ga_column,title) {
  # Calculate rmse, mae, and correlation for each column
  rmse <- Metrics::rmse(ano[["Del_GA_Calc"]], ano[[ga_column]])
  mae <- Metrics::mae(ano[["Del_GA_Calc"]], ano[[ga_column]])
  cor <- cor(ano[["Del_GA_Calc"]], ano[[ga_column]])
  
  # Prepare labels
  rho <- paste("Correlation=", round(cor, 2))
  error <- paste0("RMSE= ", round(rmse, 2), " wks")
  
  # Create the scatter plot
  df <- data.frame(gold = ano[["Del_GA_Calc"]], prediction = ano[[ga_column]])
  g_sc <- df %>% 
    ggplot(aes(x = gold, y = prediction)) + 
    geom_point() +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    labs(x = "Reported GA (weeks)",
         y = "Predicted GA (weeks)",
         title=title) +
    lims(x = c(18, 43),
         y=c(18,43)) +
    theme_bw() +
    annotate("text", label = rho, x = 22, y = 40, size = 5, colour = "black") +
    annotate("text", label = error, x = 22, y = 39, size = 5, colour = "black")
  
  return(g_sc)
}



columns_to_plot <- c(RPC="ga_rpc", 
                     CPC="ga_cpc",
                     RRPC="ga_rrpc",
                     paste0("ga_", top_performers),"ga_woc")
names(columns_to_plot)<- c("Robust Placental Clock",
                           "Control Placental Clock",
                           "Refined Robust Placental Clock",
                           names(top_performers),
                           "Wisdom of Crowd")

# Use purrr::map to create plots for all GA columns and store them in a list
plots <- map2(columns_to_plot, names(columns_to_plot), plot_ga_predictions)


pdf("results/Parity_plots.pdf", width = 8, height = 6)
# Loop over each plot and print it to the PDF
for (plot in plots) {
  print(plot)
}
dev.off()
