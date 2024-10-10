# script for data cleaning after the initial check for observer datasets - North Eastern IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
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

# convert length to mass (the same equations used for the welsh stocks)
observer_data_lobster <- observer_data_lobster |> 
  dplyr::mutate(sample_mass_kg = 4.36E-07*cl^3.10753)

# function to compute nominal catch, landings, cpue, and lpue per fishing trip 
compute_cpue.lpue <- function(data) {
  
  # per fishing trip
  # compute effort (number of pots lifted per trip)
  observer_data_effort <- data |> 
    dplyr::group_by(date, fleet, string, vessel_length) |> 
    dplyr::reframe(date = unique(date),
                   fleet = unique(fleet),
                   string = unique(string),
                   nominal.effort = unique(pots, na.rm = TRUE),
                   vessel_length = unique(vessel_length)) |> 
    dplyr::group_by(date, fleet, vessel_length) |> 
    dplyr::reframe(nominal.effort = sum(nominal.effort, na.rm = TRUE),
                   vessel_length = unique(vessel_length)) 
  
  
  # compute total catch (kg) per trip
  observer_data_catch <- data |> 
    #dplyr::mutate(sample_mass_kg = carapace_width) 
    dplyr::group_by(date, fleet, vessel_length) |> 
    dplyr::reframe(nominal.catch = sum(sample_mass_kg, na.rm = TRUE)) 
  
  # # compute total landings (kg)
  # observer_data_landing <- data |> dplyr::group_by(trip) |> 
  #   dplyr::filter(landed==1) |> 
  #   dplyr::summarise(nominal.landing = sum(mass, na.rm = TRUE)) 
  
  # merge datasets and add vessel info
  observer_data_trip <- observer_data_effort |> 
    list(observer_data_catch) |>
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue = nominal.catch/nominal.effort
                  #, nominal.lpue = nominal.landing/nominal.effort
                  ) |>
    tidyr::unite(trip, c(fleet, date), sep = "|", remove = FALSE) |> # create factor per fishing trip
    dplyr::mutate(date = as.Date(date), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  # observer_data_select_trip <- data |> 
  #   dplyr::group_by(date) |> 
  #   dplyr::reframe(vessel_length = unique(vessel_length), 
  #                  #site = unique(site), 
  #                  lowest_tide = mean(lowest_tide, na.rm = TRUE),
  #                  temp = mean(temp, na.rm = TRUE),
  #                  temp_interpol_dist = mean(temp_interpol_dist, na.rm = TRUE),
  #                  dist_shore = mean(dist_shore, na.rm = TRUE),
  #                  roughness = unique(roughness),
  #                  rough_dist = unique(rough_dist),
  #                  sed_folk16 = unique(sed_folk16),
  #                  sed_folk7 = unique(sed_folk7)) # trip-level info
  # observer_data_trip <- observer_data_trip |> 
  #   dplyr::left_join(observer_data_select_trip, by = c("fleet", "date")) 
  
  
  # per pot set (w/ unique gps)
  # compute total number of pots set in each location in each trip
  observer_data_potset <- data |> 
    dplyr::group_by(date, fleet, string, latitude, longitude) |> 
    dplyr::reframe(date = unique(date),
                   fleet = unique(fleet),
                   string = unique(string),
                   potset = unique(pots, na.rm = FALSE),
                   vessel_length = unique(vessel_length), 
                   lowest_tide = mean(lowest_tide), 
                   temp = unique(temp), 
                   temp_interpol_dist = unique(temp_interpol_dist), 
                   dist_shore = unique(dist_shore), 
                   roughness = unique(roughness), 
                   rough_interpol_dist = unique(rough_dist), 
                   sediment16 = unique(sed_folk16),
                   sediment7 = unique(sed_folk7)) |> 
    dplyr::group_by(date, fleet, latitude, longitude) |> 
    dplyr::reframe(date = unique(date),
                   fleet = unique(fleet),
                   potset = sum(potset, na.rm = FALSE),
                   vessel_length = unique(vessel_length), 
                   lowest_tide = mean(lowest_tide), 
                   temp = unique(temp), 
                   temp_interpol_dist = unique(temp_interpol_dist), 
                   dist_shore = unique(dist_shore), 
                   roughness = unique(roughness), 
                   rough_interpol_dist = unique(rough_interpol_dist), 
                   sediment16 = unique(sediment16),
                   sediment7 = unique(sediment7)) 
  
  # compute total catch (kg) per each location in each trip
  observer_data_catch_potset <- data |> 
    dplyr::group_by(date, fleet, latitude, longitude) |> 
    dplyr::reframe(nominal.catch_potset = sum(sample_mass_kg, na.rm = TRUE)) 
  
  # # compute total landings (kg) per each location in each trip
  # observer_data_landing_potset <- data |> dplyr::group_by(lat_lon, trip) |> 
  #   dplyr::filter(landed==1) |> 
  #   dplyr::summarise(nominal.landing_potset = sum(mass, na.rm = TRUE)) 
  
  # merge all datasets
  observer_data_potset <- observer_data_potset |> 
    list(observer_data_catch_potset) |> 
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue_potset = nominal.catch_potset/potset 
                  #, nominal.lpue_potset = nominal.landing_potset/potset
                  ) |>
    #tidyr::separate_wider_delim(cols = lat_lon, delim = "|", names = c("lat", "lon")) |>
    #tidyr::separate_wider_delim(cols = trip, delim = "|", names = c("vessel", "fleet", "date")) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(#lat = as.numeric(lat), 
                  #lon = as.numeric(lon), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  
  # # add covariate info
  # observer_data_select_potset <- data |> 
  #   dplyr::group_by(fleet, date, latitude, longitude) |> 
  #   dplyr::reframe(vessel_length = unique(vessel_length), 
  #                  #species = unique(species), 
  #                  lowest_tide = unique(lowest_tide), 
  #                  temp = unique(temp), 
  #                  temp_interpol_dist = unique(temp_interpol_dist), 
  #                  dist_shore = unique(dist_shore), 
  #                  roughness = unique(roughness), 
  #                  rough_interpol_dist = unique(rough_dist), 
  #                  sediment16 = unique(sed_folk16),
  #                  sediment16 = unique(sed_folk7)
  #                  ) # potset-level info
  # observer_data_potset <- observer_data_potset |> 
  #   dplyr::left_join(observer_data_select_potset, by = c( "date", "latitude", "longitude")) 
  
  return(list(observer_data_trip, observer_data_potset))
}

# apply the function to each stock
observer_data_lobster_out <- compute_cpue.lpue(observer_data_lobster) # lobster

# apply the function to each stock for under 10m
observer_data_lobster_u10 <- observer_data_lobster |> dplyr::filter(vessel_length <= 10)
observer_data_lobster_out_u10 <- compute_cpue.lpue(observer_data_lobster_u10) # lobster

# export output as rds (as a list)
readr::write_rds(observer_data_lobster_out, file = "processed_data/neifca/observer_data_lobster_nominal.cpue_new.rds") # lobster

# export output as csv
readr::write_csv(observer_data_lobster_out[[1]], file = "processed_data/neifca/observer_data_lobster_nominal.cpue_trip_new.csv") 
readr::write_csv(observer_data_lobster_out[[2]], file = "processed_data/neifca/observer_data_lobster_nominal.cpue_potset_new.csv") 

# compute cpue for survey data
survey_data_lobster <- readr::read_csv("processed_data/neifca/survey_data_lobster_neifca_clean.csv") |> dplyr::glimpse()

# convert length to mass (the same equations used for the welsh stocks)
survey_data_lobster <- survey_data_lobster |>
  dplyr::mutate(sample_mass_kg = 4.36E-07*carapace_len^3.10753)

# function to compute cpue for survey data
compute_cpue_survey <- function(data) {
  # per fishing trip
  # compute total catch (kg) per trip
  survey_data_cpue <- data |>
    #dplyr::mutate(sample_mass_kg = carapace_width)
    dplyr::group_by(year, month) |>
    dplyr::reframe(year = unique(year),
                   month = unique(month),
                   nominal.catch = sum(sample_mass_kg, na.rm = TRUE),
                   nominal.effor = 15,
                   nominal.cpue = nominal.catch/15)
  return(survey_data_cpue)
}

# apply the function to each stock
survey_data_lobster_out <- compute_cpue_survey(survey_data_lobster) # lobster

# export output as csv
readr::write_csv(survey_data_lobster_out, file = "processed_data/neifca/survey_data_lobster_nominal.cpue_trip.csv") 

# plot output
# temporal variation
# select a dataset and a parameter
data <- observer_data_lobster_out[[1]]
response <- data$nominal.cpue
response.name <- "nominal catch rate (kg per number of pots hauled)"

mycolors <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
              RColorBrewer::brewer.pal(name = "Set3", n = 7))
(plot1<- data |> ggplot2::ggplot(ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response, 
                                     group = lubridate::quarter(date, with_year = TRUE))) +
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(size = 2., ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response,
                                               group = lubridate::quarter(date, with_year = TRUE),
                                               color = as.factor(lubridate::month(date))), 
                       alpha = 0.4) +
  ggplot2::xlab("year") +
  ggplot2::ylab(response.name) +
  ggplot2::theme_classic() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))))

# export plot
ggplot2::ggsave(file=paste0("plots/neifca/", response.name, "_observer_trends_neifca.svg"), plot=plot1, width=12, height=8)

# spatial distribution
# select a dataset and a parameter
data <- observer_data_lobster_out[[2]]
response <- data$nominal.cpue_potset
response.name <- "nominal cpue (kg per number of pots hauled)"
xlim <- c(min(data$longitude, na.rm = TRUE) - 0.9 * 5, 
          max(data$longitude, na.rm = TRUE) * 2.5)
ylim <- c(min(data$latitude, na.rm = TRUE) * 0.95, 
          max(data$latitude, na.rm = TRUE) * 1.02)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#eez <- sf::read_sf(dsn = "data/shapefiles/World_EEZ_v12_20231025/eez_v12.shp", stringsAsFactors = FALSE)

# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "data/ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)

# read in ICES rectangles for wales
# subset nifca landings
ices_rec <- readr::read_delim(file = "data/neifca/ices_rectangles_england.csv") |> 
  dplyr::filter(proportion != 0)
neifca_rec <- c("36E9", "36F0", "36F1", "37E9", "37F0", "38E8", "38E9", "39F0") 
ices_rec_neifca <- ices_rec |> 
  dplyr::filter(`ICES Rectangle` %in% neifca_rec)

# subset neifca
shp_ices.rec_neifca <- shp_ices.rec |> 
  dplyr::right_join(ices_rec_neifca, by = c("ICESNAME"="ICES Rectangle")) |>
  dplyr::mutate(PERCENTAGE = PERCENTAGE*proportion)

(plot2 <- ggplot2::ggplot(data = world) +  
    ggplot2::scale_color_manual(values = mycolors) +
    ggplot2::geom_sf(data = shp_ices.rec_neifca, fill = NA, colour = "darkblue") +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(xlim = xlim, 
                      ylim = ylim, expand = FALSE) +
    ggplot2::geom_point(data=data, 
                      ggplot2::aes(x=longitude, y=latitude, size = nominal.cpue_potset, color=as.factor(month)), 
                      alpha=I(0.3)) + 
    ggplot2::theme_bw() +
    ggplot2::theme( 
      panel.grid.minor = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      axis.line = ggplot2::element_line(colour = "black"),
      axis.title.x = ggplot2::element_text(size=10),
      axis.title.y = ggplot2::element_text(size=10),	
      axis.text.x = ggplot2::element_text(size=8), 
      axis.text.y = ggplot2::element_text(size=8),
      legend.background = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour="black", size = 8),
      plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(), 
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
    ggplot2::facet_wrap(~ year, 
                      strip.position = "top", 
                      ncol = 3))

# export plot
ggplot2::ggsave(file=paste0("plots/neifca/", response.name, "_observer_space_neifca.svg"), 
                plot=plot2, width=12, height=8)


# size structure
# select a dataset
observer_data_lobster_no.na <-  observer_data_lobster |> dplyr::filter(!is.na(sex))
observer_data_crab_no.na <-  observer_data_crab |> dplyr::filter(!is.na(sex))
response.name <- "carapace length (mm)"
sex.label <- c("male", "female")
names(sex.label) <- c(0, 1)

(plot3 <- ggplot2::ggplot(observer_data_crab_no.na, 
                ggplot2::aes(x = cl, y = as.factor(year))) +
  ggridges::geom_density_ridges(scale = 2.5, 
                                alpha = 0.3, 
                                quantile_lines = TRUE, 
                                quantiles = 0.5, 
                                ggplot2::aes(fill = as.factor(sex))) +
  ggplot2::xlab("carapace width (mm)") +
  ggplot2::ylab("year") +
  ggplot2::scale_fill_manual(labels = c("male", "female"), 
                             values = c("darkblue", "darkred")) +
  ggplot2::theme_classic() + 
  ggplot2::coord_flip() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
  ggplot2::facet_wrap(~ sex, 
                      labeller = ggplot2::labeller(sex = sex.label), 
                      strip.position = "top", 
                      ncol = 3)) 

# export plot
ggplot2::ggsave(file=paste0("plots/neifca/", response.name,  "size.comp_observer_neifca.svg"), plot=plot3, width=12, height=8)
