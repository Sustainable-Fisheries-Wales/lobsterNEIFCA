# script for data cleaning for the observer datasets. - North Eastern IFCA

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in and reformat data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
observer_data_vessel.master22 <- readr::read_csv(file = "data/neifca/raw/observer.vessel.info_master2022.csv") |> 
  dplyr::rename(vessel = Column1) |> 
  dplyr::glimpse() 
colnames(observer_data_vessel.master22) <- observer_data_vessel.master22 |> 
  janitor::clean_names() |> 
  colnames()
observer_data_vessel.master22 <- observer_data_vessel.master22 |> 
  dplyr::select(-ices)
observer_data_vessel.master22$soak_time_days <- observer_data_vessel.master22$dab <- 
  observer_data_vessel.master22$whelk <- observer_data_vessel.master22$wrasse <-  
  observer_data_vessel.master22$sea_urchin <- observer_data_vessel.master22$hermit <- NA
observer_data_vessel.master22 <- observer_data_vessel.master22 |> 
  dplyr::select(date, month, vessel, fleet, pots, soak_time_days, site, lat_deg, lat_min, long_deg, long_min, long_e_w, 
                lobster, edible, sole, dab, codling, velvet, starfish, dogfish, scorpionfish, octopus,  portunus, 
                shore_crab, squatlobster, whelk, wrasse, sea_urchin, hermit, comments)|> 
  dplyr::glimpse() 

observer_data_industry.master23 <- readr::read_csv(file = "data/neifca/raw/observer.industry_master2023.csv") |> 
  dplyr::glimpse()
colnames(observer_data_industry.master23) <- observer_data_industry.master23 |> 
  janitor::clean_names() |> 
  colnames()
observer_data_industry.master23$soak_time_days <- observer_data_industry.master23$site <- 
  observer_data_industry.master23$sole <- observer_data_industry.master23$dab <- 
  observer_data_industry.master23$codling <- observer_data_industry.master23$velvet <- 
  observer_data_industry.master23$starfish <- observer_data_industry.master23$dogfish <- 
  observer_data_industry.master23$scorpionfish <- observer_data_industry.master23$octopus <- 
  observer_data_industry.master23$portunus <- observer_data_industry.master23$shore_crab <- 
  observer_data_industry.master23$squatlobster <- observer_data_industry.master23$whelk <- 
  observer_data_industry.master23$wrasse <- observer_data_industry.master23$sea_urchin <- 
  observer_data_industry.master23$hermit <- observer_data_industry.master23$comments <- NA
observer_data_industry.master23 <- observer_data_industry.master23 |> 
  dplyr::select(date, month, vessel, fleet, pots, soak_time_days, site, lat_deg, lat_min, long_deg, long_min, long_e_w, 
                lobster, edible, sole, dab, codling, velvet, starfish, dogfish, scorpionfish, octopus,  portunus, 
                shore_crab, squatlobster, whelk, wrasse, sea_urchin, hermit, comments)|> 
  dplyr::glimpse() 
observer_data_master23 <- readr::read_csv(file = "data/neifca/raw/neifca_master2023.csv") |> 
  dplyr::glimpse()
colnames(observer_data_master23) <- observer_data_master23 |> 
  janitor::clean_names() |> 
  colnames()
observer_data_master23$site <- NA
observer_data_master23 <- observer_data_master23 |> 
  dplyr::select(date, month, vessel, fleet, pots, soak_time_days, site, lat_deg, lat_min, long_deg, long_min, long_e_w, 
                lobster, edible, sole, dab, codling, velvet, starfish, dogfish, scorpionfish, octopus,  portunus, 
                shore_crab, squatlobster, whelk, wrasse, sea_urchin, hermit, comments)  
observer_data_measure_lobster22 <- readr::read_csv(file = "data/neifca/raw/observer.vessel.info_measure_lobster2022.csv") |>
  dplyr::select(-Column1, -Column2) |> 
  dplyr::rename(Sex = SEX) |> 
  dplyr::glimpse() 
colnames(observer_data_measure_lobster22) <- observer_data_measure_lobster22 |> 
  janitor::clean_names() |> 
  colnames()
observer_data_measure_lobster23 <- readr::read_csv(file = "data/neifca/raw/observer_measure2023_lobster.csv") |>
  dplyr::glimpse()
colnames(observer_data_measure_lobster23) <- observer_data_measure_lobster23 |> 
  janitor::clean_names() |> 
  colnames()

# merge 2022 & 2023 data
observer_data_vessel <- data.table::rbindlist(list(observer_data_vessel.master22, 
                                                   observer_data_master23, observer_data_industry.master23))
observer_data_vessel <- observer_data_vessel |> 
  dplyr::rename(lobsterN=lobster, edibleN=edible, soleN=sole, dabN=dab, codlingN=codling, 
  velvetN=velvet, starfishN=starfish, dogfishN=dogfish, scorpionfishN=scorpionfish,
  octopusN=octopus, portunusN=portunus, shore_crabN=shore_crab, 
  squatlobsterN=squatlobster, whelkN=whelk, wrasseN=wrasse, 
  sea_urchinN=sea_urchin, hermitN = hermit)
observer_data_lobster <- data.table::rbindlist(list(observer_data_measure_lobster22, observer_data_measure_lobster23))

# data cleaning
observer_data_vessel <- observer_data_vessel |> 
  dplyr::mutate(month = dplyr::recode(month, March=3, April=4, May=5, June=6, July=7, August=8, September=9)) |>
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |>  
  dplyr::mutate(year = lubridate::year(date)) |> 
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  tidyr::unite(trip, c(vessel, date), sep = "|", remove = FALSE) |> 
  dplyr::mutate(edibleN = as.numeric(edibleN),
                lat = paste0(lat_deg, ".", gsub("\\..*", "", lat_min)),
                lon = paste0(long_deg, ".", gsub("\\..*", "", long_min))) |>
  # convert coordinates 
  tidyr::unite(lat_lon, c(lat, lon), sep = "|", remove = FALSE) |> 
  tidyr::unite(trip, c(vessel, date), sep = "|", remove = FALSE) |>
  tidyr::unite(lat_lon_trip, c(lat, lon, trip), sep = "|", remove = FALSE) 

observer_data_lobster <- observer_data_lobster |>
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, f=1, m=0)) |> 
  dplyr::mutate(month = dplyr::recode(month, March=3, April=4, May=5, June=6, July=7, August=8, September=9)) |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> 
  dplyr::mutate(year = lubridate::year(date)) |> 
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  tidyr::unite(trip, c(vessel, date), sep = "|", remove = FALSE) 
  
# export output as csv
readr::write_csv(observer_data_vessel, file = "processed_data/neifca/observer_data_vessel_neifca_clean.csv") 
readr::write_csv(observer_data_lobster, file = "processed_data/neifca/observer_data_lobster_neifca_clean.csv")
