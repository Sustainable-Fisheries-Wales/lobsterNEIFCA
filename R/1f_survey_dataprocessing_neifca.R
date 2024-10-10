# script for data cleaning for the fishery-independent survey datasets. - North Eastern IFCA
# created: 6/8/2024 by Daisuke Goto (d.goto@bangor.ac.uk)

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in and reformat data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
survey_data_lobster <- readr::read_csv(file = "data/neifca/raw/potting.survey_lobster.csv") |> 
  dplyr::rename(carapace_len=CL) |> dplyr::glimpse() 
colnames(survey_data_lobster) <- survey_data_lobster |> 
  janitor::clean_names() |> 
  colnames() 
survey_data_effort <- readr::read_csv(file = "data/neifca/raw/survey_effort.csv") |> dplyr::glimpse()
colnames(survey_data_effort) <- survey_data_effort |> 
  janitor::clean_names() |> 
  colnames() 

# data cleaning
survey_data_lobster <- survey_data_lobster |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> 
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::mutate(month = dplyr::recode(month, MAY=5, JUNE=6, JULY=7, AUGUST=8, SEPTEMBER=9, 
                                      NOVEMBER=11, OCTOBER=10, March=3, June=6, July=7,      
                                      May=5, August=8, September=9)) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0)) 

survey_data_effort <- survey_data_effort |> 
  #dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> 
  dplyr::mutate(month = dplyr::recode(month, MAY=5, JUNE=6, JULY=7, AUGUST=8, SEPTEMBER=9, 
                                      NOVEMBER=11, OCTOBER=10, March=3, June=6, July=7,      
                                      May=5, August=8, September=9)) 

# export output as csv
readr::write_csv(survey_data_lobster, file = "processed_data/neifca/survey_data_lobster_neifca_clean.csv") 
readr::write_csv(survey_data_effort, file = "processed_data/neifca/survey_data_effort_neifca_clean.csv") 
