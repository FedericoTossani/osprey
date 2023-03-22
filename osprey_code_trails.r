
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
                      "gridExtra")

# with this line of code I check if alll the packages are installed and then I load it

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}


# Load funcitons for later
# source("utility_functions.r")  # this doesn't work


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
            rename("long"="location.long",
                   "lat"="location.lat",
                   "ext_temp"="external.temperature",
                   "gsm_signal_strength"="gsm.gsm.signal.strength",
                   "sensor_type"="sensor.type",
                   "id"="individual.local.identifier")%>%
            mutate(id = as.factor(id),
                   timestamp = as.POSIXct(timestamp, tz = "UTC"),
                   signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                   day = day(timestamp),
                   month = month(timestamp),
                   year = year(timestamp))%>%
            unite(m_day, c(month, day), sep="/", remove = F)%>%
            left_join(osprey_death, by = c("id" = "id"))%>%
            mutate( season = case_when(
                       month %in% 10:12 ~ "Fall",
                       month %in%  1:3  ~ "Winter",
                       month %in%  4:6  ~ "Spring",
                       TRUE ~ "Summer"),
                    ring_id = case_when(
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
      dplyr::select(ring_id, timestamp, long, lat)


# visualize the data set
ggplot(data, aes(long, lat, col = ring_id)) +
geom_point(size = 0.5) +
geom_path() +
coord_map("mercator")

##########################
#                        #
#   Plot coord vs time   #
#                        #
##########################

# Longitude vs time
all_long_time <-
ggplot(data, aes(timestamp, long, col = ring_id)) +
geom_point(size = 0.5) +
#facet_wrap(~ ring_id) +
geom_path()


# Latitude vs time
all_lat_time <-
ggplot(data, aes(timestamp, lat, col = ring_id)) +
geom_point(size = 0.5) +
#facet_wrap(~ ring_id) +
geom_path()

grid.arrange(all_long_time, all_lat_time, nrow=2)

# plot of coord vs time for every animals

        #"H7" 
        h7 <- data%>%
            filter(ring_id == "H7")

        h7_lat_time <-
        ggplot(h7, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_long_time <-
        ggplot(h7, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(h7_long_time, h7_lat_time, nrow=2)


        #"CBK"
        cbk <- data%>%
            filter(ring_id == "CBK")

        cbk_lat_time <-
        ggplot(cbk, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        cbk_long_time <-
        ggplot(cbk, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(cbk_long_time, cbk_lat_time, nrow=2)

        #"CIV"
        civ <- data%>%
            filter(ring_id == "CIV")

        civ_lat_time <-
        ggplot(civ, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        civ_long_time <-
        ggplot(civ, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(civ_long_time, civ_lat_time, nrow=2)

        #"E7"
        e7 <- data%>%
            filter(ring_id == "E7")

        e7_lat_time <-
        ggplot(e7, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        e7_long_time <-
        ggplot(e7, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(e7_long_time, e7_lat_time, nrow=2)

        #"A7" 
        #"E7"
        a7 <- data%>%
            filter(ring_id == "A7")

        a7_lat_time <-
        ggplot(a7, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        a7_long_time <-
        ggplot(a7, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(a7_long_time, a7_lat_time, nrow=2)

        #"Antares"
        antares <- data%>%
            filter(ring_id == "Antares")

        antares_lat_time <-
        ggplot(antares, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        antares_long_time <-
        ggplot(antares, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(antares_long_time, antares_lat_time, nrow=2)

        #"IAD"
        iad <- data%>%
            filter(ring_id == "IAD")

        iad_lat_time <-
        ggplot(iad, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        iad_long_time <-
        ggplot(iad, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(iad_long_time, iad_lat_time, nrow=2)

        #"CAM" 
        cam <- data%>%
            filter(ring_id == "CAM")

        cam_lat_time <-
        ggplot(cam, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        cam_long_time <-
        ggplot(cam, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(cam_long_time, cam_lat_time, nrow=2)

        #"IBI" 
        ibi <- data%>%
            filter(ring_id == "IBI")

        ibi_lat_time <-
        ggplot(ibi, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        ibi_long_time <-
        ggplot(ibi, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibi_long_time, ibi_lat_time, nrow=2)

        #"IAB"     
        iab <- data%>%
            filter(ring_id == "IAB")

        iab_lat_time <-
        ggplot(iab, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        iab_long_time <-
        ggplot(iab, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(iab_long_time, iab_lat_time, nrow=2)

        #"ICZ"    
        icz <- data%>%
            filter(ring_id == "ICZ")

        icz_lat_time <-
        ggplot(icz, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        icz_long_time <-
        ggplot(icz, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(icz_long_time, icz_lat_time, nrow=2)

        #"IBS"   
        ibs <- data%>%
            filter(ring_id == "IBS")

        ibs_lat_time <-
        ggplot(ibs, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        ibs_long_time <-
        ggplot(ibs, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibs_long_time, ibs_lat_time, nrow=2)

        #"IBH" 
        ibh <- data%>%
            filter(ring_id == "IBH")

        ibh_lat_time <-
        ggplot(ibh, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        ibh_long_time <-
        ggplot(ibh, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibh_long_time, ibh_lat_time, nrow=2)

        #"IBK"
        ibk <- data%>%
            filter(ring_id == "IBK")

        ibk_lat_time <-
        ggplot(ibk, aes(timestamp, lat, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        ibk_long_time <-
        ggplot(ibk, aes(timestamp, long, col = ring_id)) +
        geom_point(size = 0.5) +
        geom_path()

        grid.arrange(ibk_long_time, ibk_lat_time, nrow=2)


# Project to UTM
llcoord <- st_as_sf(data[, c("long", "lat")], coords = c("long", "lat"),
crs = CRS("+proj=longlat +datum=WGS84"))
utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=32 +datum=WGS84"))

# Add Easting-Northing to data (in km)
data[, c("x", "y")] <- st_coordinates(utmcoord)/1000

# Plot Northing vs Easting
ggplot(data, aes(x, y, col = ring_id)) +
geom_point(size = 0.5) +
geom_path() +
coord_equal()

# Table of time intervals in data
plot(table(diff(data$timestamp)), xlim = c(0, 300),
xlab = "time interval (min)", ylab = "count")













