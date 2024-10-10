# script to reformat size composition data as SS model input for the observer data - North Eastern IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
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

# merge datasets w/ habitat data
observer_data_lobster <- observer_data_env |> 
  dplyr::filter(species == "Lobster")

# survey data
#survey_data_effort <- readr::read_csv("processed_data/neifca/survey_data_effort_neifca_clean.csv") |> dplyr::glimpse()
survey_data_lobster <- readr::read_csv("processed_data/neifca/survey_data_lobster_neifca_clean.csv") |>
  tidyr::unite(month.yr, c(month, year), sep = "-", remove = FALSE) |> 
  dplyr::glimpse()

# reformat length composition input data (for SS)
#_yr month fleet sex part Nsamp datavector(female-male) ***separate males and females*** 
# observer data
data <- observer_data_lobster
colnames(data)[17] <- "length"
data <- data |> dplyr::filter(!is.na(sex))
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 1#round(min(data$length, na.rm = TRUE)) #4
size.max <- 21#round(max(data$length, na.rm = TRUE)) #18
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
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
    size.dist_m[6] <- nrow(subdata_m)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::select(-year, -month, -fleet, -sex, -part, -nsample)
size.dist_lobster_f <- size.dist_lobster |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster_observer <- size.dist_lobster_f |> dplyr::bind_cols(size.dist_lobster_m) 
colnames(size.dist_lobster_observer) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                          paste0("f", 1:n.size), paste0("m", 1:n.size))

# survey data
data <- survey_data_lobster
colnames(data)[4] <- "length"
data <- data |> dplyr::filter(!is.na(sex)) |>
  dplyr::filter(!is.na(year)) 
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
(size.min <- round(min(data$length, na.rm = TRUE))) 
size.max <- 21#round(max(data$length, na.rm = TRUE)) 14
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
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
    size.dist_m[6] <- nrow(subdata_m)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::select(-year, -month, -fleet, -sex, -part, -nsample)
size.dist_lobster_f <- size.dist_lobster |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
plusrow <- as.data.frame(matrix(NA, 1, n.size+6))
colnames(plusrow) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_f <- size.dist_lobster_f |> dplyr::bind_rows((plusrow)) 
size.dist_lobster_survey <- size.dist_lobster_f |> dplyr::bind_cols(size.dist_lobster_m) 
colnames(size.dist_lobster_survey) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                        paste0("f", 1:n.size), paste0("m", 1:n.size))


# export data
readr::write_csv(size.dist_lobster_observer, file = "processed_data/neifca/observer.size.comp.data_lobster_neifca_ss.csv") 
readr::write_csv(size.dist_lobster_survey, file = "processed_data/neifca/survey.size.comp.data_lobster_neifca_ss.csv") 
