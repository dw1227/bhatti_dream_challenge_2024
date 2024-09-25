# name: get_username_teamname.R
#
# author: Gaurav Bhatti
#
# input: data/submissions/Job-393694313420778661233189284.csv
#        data/submissions/Test_data_evaluation.csv
#        
# output: the same files with full user/team names
#         data/submissions/Job-393694313420778661233189284.csv
#         data/submissions/Test_data_evaluation.csv



rm(list=ls())

library(tidyverse)
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



# Load data and process
read_csv("data/submissions/Job-393694313420778661233189284.csv") |> 
  mutate(Submitter  = map_chr(submitterid, extract_username)) |> 
  write_csv("data/submissions/Job-393694313420778661233189284.csv")


read_csv("data/submissions/Test_data_evaluation.csv")  |>  
  mutate(Submitter  = map_chr(submitterid, extract_username)) |> 
  write_csv("data/submissions/Test_data_evaluation.csv")
