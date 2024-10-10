# script for data cleaning for the quayside datasets - North Eastern IFCA
# created: 6/7/2024 by Daisuke Goto (d.goto@bangor.ac.uk)

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in and reformat data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

quayside_data_ind_lobster <- readr::read_csv(file = "data/neifca/raw/quayside_lobster.csv") |> 
  dplyr::glimpse() 
colnames(quayside_data_ind_lobster) <- quayside_data_ind_lobster |> 
  janitor::clean_names() |> 
  colnames() 

# data cleaning
quayside_data_ind_lobster <- quayside_data_ind_lobster |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, m=0, f=1, "M/F"=2, "#M"=0, "M \xa0"=0, "F \xa0"=1, "\xa0M"=0, "\xa0F"=1)) |> # recode sex
  dplyr::mutate(size = as.numeric(size)) 
  
# export output as csv
readr::write_csv(quayside_data_ind_lobster, file = "processed_data/neifca/quayside_data_ind_lobster_neifca_clean.csv") 
