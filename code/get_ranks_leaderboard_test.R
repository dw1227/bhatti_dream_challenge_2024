# name: get_ranks_leaderboard_test.R
#
# author: Gaurav Bhatti
#
# input: data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        
# output: power point silde with rankings in leaderboard and test datasets.
#         results/rankings_leaderboard_test.pptx



rm(list=ls())

library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(flextable)
library(officer)


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



# Load data and process
df <- read_csv("data/submissions/Job-393694313420778661233189284.csv")
df_test <- read_csv("data/submissions/Test_data_evaluation.csv") 

# Process submission data and test data
df_select_submissions <- df |> 
  filter(submission_status == "SCORED") |> 
  group_by(submitterid, evaluationid) |> 
  mutate(submission_datetime = as.POSIXlt(createdOn/1000, origin = "1970-01-01")) |> 
  slice(which.max(submission_datetime)) |> 
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_"))

df_test <- df_test |>  
  mutate(submission_stamp = paste(submitterid, evaluationid, sep = "_")) |> 
  group_by(submitterid) |> 
  slice_min(Test_rmse,n=1,with_ties = FALSE) |> 
  ungroup() |> 
  select(submission_stamp,Test_rmse) |> 
  left_join(df_select_submissions, by = "submission_stamp") |> 
  mutate(Leaderboard_Rank = rank(rmse),
         Test_Rank = rank(Test_rmse)) |> 
  select( submitterid, Test_Rank, Test_rmse, Leaderboard_Rank, rmse) |> 
  mutate(Submitter  = map_chr(submitterid, extract_username))
  

# Set table defaults
set_flextable_defaults(digits = 2, decimal.mark = ".", big.mark = " ", na_str = "<na>")

# Create flextables for Test and Leaderboard
ft1 <- df_test |> 
  select(Participant = Submitter, Rank = Test_Rank, RMSE = Test_rmse) |> 
  arrange(Rank) |> 
  qflextable() |> 
  colformat_double(j = "RMSE") |> 
  add_header_row(values = c("", "Test"), colwidths = c(1, 2)) |> 
  align(align = "center", part = "all")

ft2 <- df_test %>%
  select(Participant = Submitter, Rank = Leaderboard_Rank, RMSE = rmse) |> 
  arrange(Rank) |> 
  qflextable() |> 
  colformat_double(j = "RMSE") |> 
  add_header_row(values = c("", "Leaderboard"), colwidths = c(1, 2)) |> 
  align(align = "center", part = "all")

# Generate PowerPoint with the tables
ppt <- read_pptx() |> 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(ft2, location = ph_location_left()) |> 
  ph_with(ft1, location = ph_location_right())

print(ppt, target = "results/rankings_leaderboard_test.pptx")
