# script to reformat the lobster observer & survery size comp data - North Eastern IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", 
              "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing scripts
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "1c_observer_dataprocessing_neifca.R")
setwd("R/")
source(file = "1f_survey_dataprocessing_neifca.R")

# read in data
observer_data_vessel <- readr::read_csv("processed_data/neifca/observer_data_vessel_neifca_clean.csv") |> 
  dplyr::glimpse()
observer_data_lobster <- readr::read_csv("processed_data/neifca/observer_data_lobster_neifca_clean.csv") |> 
  dplyr::glimpse()
observer_data_env <- readr::read_csv("processed_data/neifca/observer_data_env_neifca_clean.csv") |> 
  dplyr::glimpse()
colnames(observer_data_env)[17] <- "cl"
colnames(observer_data_env)[4] <- "fleet"

# cpue 
observer_data_cpue_lobster <- readr::read_csv("processed_data/neifca/observer_data_lobster_nominal.cpue_trip_new.csv") |> 
  dplyr::mutate(vessel = as.character(fleet)) |>
  dplyr::glimpse()

# merge datasets
observer_data_lobster <- observer_data_lobster |>
  dplyr::left_join(observer_data_cpue_lobster, by = c("date", "month", "year", "vessel"))

# survey data
#survey_data_effort <- readr::read_csv("processed_data/neifca/survey_data_effort_neifca_clean.csv") |> dplyr::glimpse()
survey_data_lobster <- readr::read_csv("processed_data/neifca/survey_data_lobster_neifca_clean.csv") |>
  tidyr::unite(month.yr, c(month, year), sep = "-", remove = FALSE) |> 
  dplyr::glimpse()

# cpue 
survey_data_cpue_lobster <- readr::read_csv("processed_data/neifca/survey_data_lobster_nominal.cpue_trip.csv") |> 
  dplyr::glimpse()

# merge datasets
survey_data_lobster <- survey_data_lobster |>
  dplyr::left_join(survey_data_cpue_lobster) |>
  dplyr::filter(month.yr != "5-2018") |>
  dplyr::mutate(year = dplyr::case_when(is.na(year) ~ 2023,  
                                        !is.na(year) ~ year),
                month.yr = dplyr::case_when(month.yr == "11-NA" ~ "11-2023",  
                                        month.yr != "11-NA" ~ month.yr))

# reformat length composition input data (for SS)
# observer data
# lobster
data <- observer_data_lobster |>
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(cl = cl/10)
colnames(data)[8] <- "length" 
size.min <- 1
size.max <- 21
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  #print(data[data$month.yr==i,])
  subdata <- data[data$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$nominal.cpue), na.rm = TRUE)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_m$nominal.cpue), na.rm = TRUE)
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)* sum(unique(subdata_f$nominal.cpue), na.rm = TRUE)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))  
    sum(unique(subdata_f$nominal.cpue), na.rm = TRUE)
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$month.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by month
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "month.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
size.dist_lobster <- size.dist_lobster |> 
  dplyr::filter(is.finite(as.numeric(nsample)))
for (i in c(unique(data$month.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- unique(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample), na.rm = TRUE))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- unique(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample), na.rm = TRUE))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_lobster2 <- dplyr::bind_rows(as.data.frame(size.dist_lobster2), as.data.frame(size.dist))
}

# reformat for ss
colnames(size.dist_lobster2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_lobster_f <- size.dist_lobster2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster_observer <- size.dist_lobster_f |> 
  dplyr::bind_cols(size.dist_lobster_m) |>
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_lobster_observer) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                       paste0("f", 1:n.size), paste0("m", 1:n.size))

# aggregate by year
size.dist_lobster_yr_observer <- size.dist_lobster_observer |> 
  tidyr::gather(sizebin, value, f1:colnames(size.dist_lobster_observer)[length(colnames(size.dist_lobster_observer))], 
                factor_key = TRUE) |>
  dplyr::filter(is.finite(nsample)) |>
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 1,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample, na.rm = TRUE),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value), na.rm = TRUE)/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()


# survey data
# lobster
data <- survey_data_lobster |>
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(carapace_len = carapace_len/10)
colnames(data)[4] <- "length" 
(size.min <- round(min(data$length, na.rm = TRUE))) 
size.max <- 21
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  subdata <- data[data$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$nominal.cpue), na.rm = TRUE)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_m$nominal.cpue), na.rm = TRUE)
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f) * sum(unique(subdata_f$nominal.cpue), na.rm = TRUE)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_f$nominal.cpue), na.rm = TRUE)
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$month.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by month
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "month.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
size.dist_lobster <- size.dist_lobster |> 
  dplyr::filter(is.finite(as.numeric(nsample)))
for (i in c(unique(data$month.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- unique(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample), na.rm = TRUE))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- unique(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample), na.rm = TRUE))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_lobster2 <- dplyr::bind_rows(as.data.frame(size.dist_lobster2), as.data.frame(size.dist))
}

# reformat for ss
colnames(size.dist_lobster2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_lobster_f <- size.dist_lobster2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster_survey <- size.dist_lobster_f |> 
  dplyr::bind_cols(size.dist_lobster_m) |>
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_lobster_survey) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                          paste0("f", 1:n.size), paste0("m", 1:n.size))

# aggregate by year
size.dist_lobster_yr_survey <- size.dist_lobster_survey |> 
  tidyr::gather(sizebin, value, f1:colnames(size.dist_lobster_survey)[length(colnames(size.dist_lobster_survey))], 
                factor_key = TRUE) |>
  dplyr::filter(is.finite(nsample)) |>
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 1,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample, na.rm = TRUE),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value), na.rm = TRUE)/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()


# export data
readr::write_csv(size.dist_lobster_yr_observer, file = "processed_data/neifca/observer.size.comp.data_lobster_neifca_ss.csv") 
readr::write_csv(size.dist_lobster_yr_survey, file = "processed_data/neifca/survey.size.comp.data_lobster_neifca_ss.csv") 
