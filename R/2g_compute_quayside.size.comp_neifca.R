# script to reformat the lobster quayside size composition data - North Eastern IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "1d_quayside_dataprocessing_neifca.R")

# read in data
quayside_data_lobster <- readr::read_csv("processed_data/neifca/quayside_data_ind_lobster_neifca_clean.csv") |> 
  dplyr::filter(!is.na(date)) |>
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) |>
  tidyr::unite(month.yr, c(month, year), sep = "-", remove = FALSE) |> # create factor month-year
  dplyr::mutate(mass_kg = 4.36E-07*size^3.10753) |> # mm -> kg
  dplyr::group_by(date) |>
  dplyr::mutate(landings_mt = sum(mass_kg)) |>
  dplyr::glimpse()

# total landings
landings_lobster <- quayside_data_lobster |>
  dplyr::group_by(year) |> 
  dplyr::reframe(year = unique(year),
                 quarter = max(lubridate::quarter(date)),
                 fleet = 3,
                 landings = sum((mass_kg), na.rm = TRUE)/1000,
                 catch.se = 0.05) |>
  dplyr::ungroup() |>
  dplyr::mutate(catch.se = sd(log(landings))/mean(log(landings))) |>
  dplyr::glimpse()

readr::write_csv(landings_lobster, file = "processed_data/neifca/inshore_landing.data_lobster_neifca_ss.csv") 

# reformat length composition input data (for SS)
# lobster
data <- quayside_data_lobster |>
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(size = size/10)
colnames(data)[4] <- "length" 
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
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$landings), na.rm = TRUE)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_m$landings), na.rm = TRUE)
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f) * sum(unique(subdata_f$landings), na.rm = TRUE)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_f$landings), na.rm = TRUE)
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
size.dist_lobster_yr_quayside <- size.dist_lobster_f |> 
  dplyr::bind_cols(size.dist_lobster_m) |>
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_lobster_yr_quayside) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                          paste0("f", 1:n.size), paste0("m", 1:n.size))

# aggregate by year
size.dist_lobster_yr_quayside <- size.dist_lobster_yr_quayside |> 
  tidyr::gather(sizebin, value, f1:colnames(size.dist_lobster_yr_quayside)[length(colnames(size.dist_lobster_yr_quayside))], 
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
readr::write_csv(size.dist_lobster_yr_quayside, file = "processed_data/neifca/quayside.size.comp.data_lobster_neifca_ss.csv") 
