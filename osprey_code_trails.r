
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


# subsetting only the columns we need

data <- osprey%>%
      dplyr::select(ID, time, lon, lat)


# visualize the data set
ggplot(data, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path() +
coord_map("mercator")

##########################
#                        #
#   Plot coord vs time   #
#                        #
##########################

# Longitude vs time
all_lon_time <-
ggplot(data, aes(time, lon, col = ID)) +
geom_point(size = 0.5) +
#facet_wrap(~ ID) +
geom_path()


# Latitude vs time
all_lat_time <-
ggplot(data, aes(time, lat, col = ID)) +
geom_point(size = 0.5) +
#facet_wrap(~ ID) +
geom_path()

grid.arrange(all_lon_time, all_lat_time, nrow=2)

# plot of coord vs time for every animals

        #"H7" 
        h7 <- data%>%
            filter(ID == "H7")

        h7_lat_time <-
        ggplot(h7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_lon_time <-
        ggplot(h7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(h7_lon_time, h7_lat_time, nrow=2)


        #"CBK"
        cbk <- data%>%
            filter(ID == "CBK")

        cbk_lat_time <-
        ggplot(cbk, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        cbk_lon_time <-
        ggplot(cbk, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(cbk_lon_time, cbk_lat_time, nrow=2)

        #"CIV"
        civ <- data%>%
            filter(ID == "CIV")

        civ_lat_time <-
        ggplot(civ, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        civ_lon_time <-
        ggplot(civ, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(civ_lon_time, civ_lat_time, nrow=2)

        #"E7"
        e7 <- data%>%
            filter(ID == "E7")

        e7_lat_time <-
        ggplot(e7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        e7_lon_time <-
        ggplot(e7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(e7_lon_time, e7_lat_time, nrow=2)

        #"A7" 
        a7 <- data%>%
            filter(ID == "A7")

        a7_lat_time <-
        ggplot(a7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        a7_lon_time <-
        ggplot(a7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(a7_lon_time, a7_lat_time, nrow=2)

        #"Antares"
        antares <- data%>%
            filter(ID == "Antares")

        antares_lat_time <-
        ggplot(antares, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        antares_lon_time <-
        ggplot(antares, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(antares_lon_time, antares_lat_time, nrow=2)

        #"IAD"
        iad <- data%>%
            filter(ID == "IAD")

        iad_lat_time <-
        ggplot(iad, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        iad_lon_time <-
        ggplot(iad, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(iad_lon_time, iad_lat_time, nrow=2)

        #"CAM" 
        cam <- data%>%
            filter(ID == "CAM")

        cam_lat_time <-
        ggplot(cam, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        cam_lon_time <-
        ggplot(cam, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(cam_lon_time, cam_lat_time, nrow=2)

        #"IBI" 
        ibi <- data%>%
            filter(ID == "IBI")

        ibi_lat_time <-
        ggplot(ibi, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ibi_lon_time <-
        ggplot(ibi, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibi_lon_time, ibi_lat_time, nrow=2)

        #"IAB"     
        iab <- data%>%
            filter(ID == "IAB")

        iab_lat_time <-
        ggplot(iab, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        iab_lon_time <-
        ggplot(iab, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(iab_lon_time, iab_lat_time, nrow=2)

        #"ICZ"    
        icz <- data%>%
            filter(ID == "ICZ")

        icz_lat_time <-
        ggplot(icz, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        icz_lon_time <-
        ggplot(icz, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(icz_lon_time, icz_lat_time, nrow=2)

        #"IBS"   
        ibs <- data%>%
            filter(ID == "IBS")

        ibs_lat_time <-
        ggplot(ibs, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ibs_lon_time <-
        ggplot(ibs, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibs_lon_time, ibs_lat_time, nrow=2)

        #"IBH" 
        ibh <- data%>%
            filter(ID == "IBH")

        ibh_lat_time <-
        ggplot(ibh, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ibh_lon_time <-
        ggplot(ibh, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibh_lon_time, ibh_lat_time, nrow=2)

        #"IBK"
        ibk <- data%>%
            filter(ID == "IBK")

        ibk_lat_time <-
        ggplot(ibk, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ibk_lon_time <-
        ggplot(ibk, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibk_lon_time, ibk_lat_time, nrow=2)


# Project to UTM
llcoord <- st_as_sf(data[, c("lon", "lat")], coords = c("lon", "lat"),
crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=32 +datum=WGS84"))

# Add Easting-Northing to data (in km)
data[, c("x", "y")] <- st_coordinates(utmcoord)/1000

# Plot Northing vs Easting
ggplot(data, aes(x, y, col = ID)) +
geom_point(size = 0.5) +
geom_path() +
coord_equal()

# Table of time intervals in data
plot(table(diff(data$time)), xlim = c(0, 1000),
xlab = "time interval (min)", ylab = "count")


data_reg <- crawlWrap(data, timeStep = "30 min")

ggplot(data_reg$crwPredict, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path()



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





