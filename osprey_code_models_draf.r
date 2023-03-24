
# Experiments to categorize fixes with HMM

setwd("C:/Tesi/R/osprey/data/")

list.of.packages <- c("tidyverse",
                      "lubridate",
                      "sf", 
                      "mapview", 
                      "adehabitatLT",
                      "adehabitatHR",
                      "stargazer",
                      "kableExtra",
                      "ggmap",
                      "terra",
                      "tidyterra",
                      "moveHMM",
                      "momentuHMM",
                      "foieGras",
                      "crawl",
                      "gridExtra",
                      "ggspatial")

# with this line of code I check if alll the packages are installed and then I load it

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}


# Load funcitons for later
# source("https://github.com/FedericoTossani/osprey/blob/main/utility_function.r")  # this doesn't work


# ================================= #
          ## Data import ##
# ================================= #

list_csv <- list.files(pattern = "20")
csv_allfile <- lapply(list_csv, read.csv)

col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                  "gsm.gsm.signal.strength", "sensor.type", "individual.local.identifier")

csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
osprey_raw <- bind_rows(csv_file_sel_col)

osprey_death <- read.csv("osprey_death.csv")


# ================================= #
         ## Filtering data ##
# ================================= #

# dead ospreys

osprey_dead <- c("Balearics2013_FOSP11-Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                 "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

# dataset to use for further analysis

osprey <- osprey_raw %>%
            dplyr::select("timestamp", "location.long", "location.lat", "external.temperature", 
                          "gsm.gsm.signal.strength", "sensor.type", "individual.local.identifier")%>%
            rename("lon"="location.long",
                   "lat"="location.lat",
                   "ext_temp"="external.temperature",
                   "gsm_signal_strength"="gsm.gsm.signal.strength",
                   "sensor_type"="sensor.type",
                   "id"="individual.local.identifier")%>%
            mutate(id = as.factor(id),
                   time = as.POSIXct(timestamp, tz = "UTC"),
                   signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                   date = as_date(parse_date_time(timestamp, orders = c("%Y-%m-%d %H:%M:%S"))),
                   day = day(time),
                   month = month(time),
                   year = year(time))%>%
            unite(m_day, c(month, day), sep="/", remove = F)%>%
            left_join(osprey_death, by = c("id" = "id"))%>%
            mutate( season = case_when(
                       month %in% 10:12 ~ "Fall",
                       month %in%  1:3  ~ "Winter",
                       month %in%  4:6  ~ "Spring",
                       TRUE ~ "Summer"),
                    ID = case_when(
                    id == "Balearics2013_FOSP11-Juv_ringH7" ~ "H7",
                    id == "Corsica2013_FOSP17_Juv_ringCBK" ~ "CBK",
                    id == "Corsica2014_FOSP21-Juv_ringCIV" ~ "CIV",
                    id == "Italy2014_FOSP27-Juv_ringE7_Edy" ~ "E7",
                    id == "Italy2015_FOSP30-juv-ringA7-Cook" ~ "A7",
                    id == "Italy2015_FOSP33-juv-Antares" ~ "Antares",
                    id == "Italy2016_FOSP28_juv_ringIAD_Ciccia" ~ "IAD",
                    id == "Italy2016_FOSP37-adult-ringCAM Mora" ~ "CAM",
                    id == "Italy2017_FOSP43_juv_ringIBI_Agrippa" ~ "IBI",
                    id == "Italy2018_FIOS45_juv_ringIAB_Anassagora" ~ "IAB",
                    id == "Italy2019_OrnitelaWhite_juv_ringICZ_Odaba" ~ "ICZ",
                    id == "Italy2020_FIOS21_juv_ringIBS_Mauna Loa" ~ "IBS",
                    id == "Italy2020_Ornitela_juv_ringIBH_Infiernillo" ~ "IBH",
                    id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK"))
              
osprey <- osprey%>%
select(-c("timestamp", "id"))%>%
relocate("ID", "time", "date", "day", "month", "year", "m_day",
                     "death_date", "season", "ext_temp", "lon", "lat", "sensor_type", "gsm_signal_strength", "signal_interruption_cause", "death_comment")


# subsetting only the columns we need

data <- osprey%>%
      dplyr::select(ID, time, lon, lat)


# visualize the data set
ggplot(data, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path() +
coord_map("mercator")


data_reg <- crawlWrap(data_sf, timeStep = "1 hour")

ggplot(data_pred, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path()

data_pred <- data_reg$crwPredict





# split at gap function 

          split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
              # Number of tracks
              n_tracks <- length(unique(data$ID))

              # Save old ID and reinitialise ID column
              data$ID_old <- data$ID
              data$ID <- character(nrow(data))

              # Loop over tracks (i.e., over IDs)
              for(i_track in 1:n_tracks) {
                  # Indices for this track
                  ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
                  track_length <- length(ind_this_track)

                  # Time intervals in min
                  dtimes <- difftime(data$time[ind_this_track[-1]], 
                                     data$time[ind_this_track[-track_length]],
                                     units = "mins")

                  # Indices of gaps longer than max_gap
                  ind_gap <- c(0, which(dtimes > max_gap), track_length)

                  # Create new ID based on split track
                  subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
                  data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
              }

              # Only keep sub-tracks longer than some duration
              track_lengths <- sapply(unique(data$ID), function(id) {
                  ind <- which(data$ID == id)
                  difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
              })
              ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
              data <- subset(data, ID %in% ID_keep)

              return(data)
          }

# Regularise tracks with foieGras

fit <- fit_ssm(data, vmax= 4, model = "crw", time.step = 24, control = ssm_control(verbose = 0, se = FALSE))

fmp <- fit_mpm(fit, what = "predicted", model = "jmpm", control = mpm_control(verbose = 0))

plot(fmp, pages = 1, ncol = 3, pal = "Zissou1", rev = TRUE)



fmap(fit, fmp, what = "predicted", pal = "Cividis")



# Regularise tracks with crawl packages


data_sf <- sf::st_as_sf(data, coords = c("lon","lat")) %>% 
  sf::st_set_crs(4326)

data_sf_lines <- data_sf %>% 
  dplyr::arrange(ID, time) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(data_sf$ID))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(ID = as.factor(unique(data_sf$ID)))


esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() + 
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(data_sf, size = 0.75,aes(color = ID)) +
  scale_x_continuous(expand = expand_scale(mult = c(.6, .6))) +
  #scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  facet_wrap(~ID)+
  ggtitle("Observed Location Paths", 
          subtitle = "Ospreys (n=14), Mediterranean basin, UE")


# 

data_sf <- sf::st_transform(data_sf, 3857)

sf::st_geometry(data_sf)

data_sf <- data_sf %>%
  dplyr::mutate(
    error_semi_major_axis = ifelse(type == 'FastGPS', 50,
                                   error_semi_major_axis),
    error_semi_minor_axis = ifelse(type == 'FastGPS', 50,
                                   error_semi_minor_axis),
    error_ellipse_orientation = ifelse(type == 'FastGPS', 0,
                                       error_ellipse_orientation)
    )

crawl::crwMLE(data_sf)





