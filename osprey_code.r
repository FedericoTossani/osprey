
# ------------------------------------------------ #       
#         Ranging behavior of non-breeding
#         Mediterranean ospreys tracked by
#         GPS-satellite telemetry
# ------------------------------------------------ #



# Relatore: Prof. Duccio Rocchini
# Correlatori: Dott. Francesco Pezzo
#              Dott. Flavio Monti
#              Dott. Andrea Sforzi
# Univesity: Alma Mater Studiorum University of Bologna



# =========================== #
         ## SUMMARY ##
# =========================== #

# 1. SetWD and packages
# 2. Data import
# 3. Filtering data
# 4. 
# 5. 
# 6. 

# ================================ #
      ## SetWD and packages ##
# ================================ #
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
                      "tidyterra")

# with this line of code I check if alll the packages are installed and then I load it

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

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

head(osprey)


# wintering osprey

h7_winter <- osprey%>%
                  filter (ID == "H7", season == "Winter")%>%
                  dplyr::select(lon, lat)

h7_winter_sp <- SpatialPoints(h7_winter)

h7_winter_mcp <- mcp(h7_winter_sp, percent = 95)

h7_plot_mcp <- 
ggplot(eu_bond) +
geom_spatvector()+
  geom_path(data = h7_winter, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "H7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# wintering home range

h7_winter_sp <- SpatialPointsDataFrame(coords = h7_winter, proj4string = CRS("+proj=longlat +datum=WGS84"))

h7_winter_hr <- clusthr(h7_winter_sp)


# Visualize the movement

cbbox <- make_bbox(lon = osprey$lon, lat = osprey$lat, f = .1) #from ggmap
sq_map <- get_map(location = cbbox, maptype = "terrain", source = "stamen")

eu_bond <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
eu_bond <- crop(eu_bond, osprey_ext)


osprey_track <- 
ggplot(eu_bond) +
geom_spatvector() + 
 geom_path(data = osprey, aes(x = lon, y = lat, color = ID), 
            linewidth = 0.5, lineend = "round") +
labs(x = " ", y = " ", title = "Inividual tracks") +
#facet_wrap(~ ID) +
theme(legend.position="none") +
theme_minimal()


########################
## Mean fixes per day ##
########################

osprey_mean_fix <-
osprey%>%
group_by(ID)%>%
count(date)%>%
summarize(mean_day_fix = mean(n), 
          max_day_fix = max(n), 
          min_day_fix = min(n))


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


# H7 track

h7 <- osprey%>%
filter(ID == "H7")%>%
group_by(date)


h7_track <- 
ggplot(eu_bond) +
#geom_spatvector()+
  geom_path(data = h7, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round") +
  labs(x = " ", y = " ", title = "H7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")



# Subsetting for natal dispersal

# H7 -> "time" > '2015-04-02 08:00:00' AND "time" < '2015-04-30 16:30:00'

h7_nd <- osprey%>%
         filter(ID == 'H7', time > '2015-04-02 08:00:00' & time < '2015-04-30 16:30:00')

h7_track <- 
ggplot(eu_bond) +
geom_spatvector()+
  geom_path(data = h7_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "H7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CIV -> "time" > '2016-03-29 00:01:00' AND "time" < '2016-10-29 18:00:00'

civ_nd <- osprey%>%
         filter(ID == 'CIV', time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00')

civ_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = civ_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CIV inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CBK -> ???

cbk_nd <- osprey%>%
         filter(ID == 'CBK', time > ??? & time < ???)

cbk_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = cbk_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CBK inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# E7 -> "time" > '2016-03-10 05:00:00'

e7_nd <- osprey%>%
         filter(ID == 'E7', time > '2016-03-10 05:00:00')

e7_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = e7_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "E7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# A7 -> "time" >= '2017-02-20 00:00:00'

a7_nd <- osprey%>%
         filter(ID == 'A7', time >= '2017-02-20 00:00:00')

a7_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = a7_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "A7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# Antares -> ???

antares_nd <- osprey%>%
         filter(ID == 'Antares', time > ???)

antares_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = antares_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "Antares inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IAD -> "time" >= '2018-03-28 06:00:00' AND "time" <= '2018-12-31 19:00:00'

iad_nd <- osprey%>%
         filter(ID == 'IAD', time >= '2018-03-28 06:00:00' & time <= '2018-12-31 19:00:00')

iad_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = iad_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IAD inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CAM -> ???

cam_nd <- osprey%>%
         filter(ID == 'CAM', time >= ???)

cam_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = cam_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CAM inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBI -> ???

ibi_nd <- osprey%>%
         filter(ID == 'IBI', time >= ???)

ibi_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibi_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBI inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IAB -> ???

iab_nd <- osprey%>%
         filter(ID == 'IAB', time >= ???)

iab_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = iab_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IAB inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# ICZ -> ???

icz_nd <- osprey%>%
         filter(ID == 'ICZ', time >= ???)

icz_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = icz_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "ICZ inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBS -> "time" > '2022-03-19 00:00:47' AND "time" < '2022-06-05 00:01:24'

ibs_nd <- osprey%>%
         filter(ID == 'IBS', time > '2022-03-19 00:00:47' & time < '2022-06-05 00:01:24')

ibs_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibs_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBS inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBH -> "time" >= '2022-04-09 04:47:05'

ibh_nd <- osprey%>%
         filter(ID == 'IBH', time >= '2022-04-09 04:47:05')

ibh_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibh_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBH inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBK -> "time" >= '2022-01-24 06:44:48'

ibk_nd <- osprey%>%
         filter(ID == 'IBK', time >= '2022-01-24 06:44:48')

ibk_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibk_nd, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBK inividual track") +
  theme_minimal() +
  theme(legend.position = "none")



# let's check if there is na in position's columns
table(is.na(osprey$lon))
table(is.na(osprey$lat))


# let's calculate some basic statistics per individual
table(osprey$ID) %>% sd


# osprey fix summary stat per individual
osprey_fix_stat_id <- 
table(osprey$ID) %>%
  data.frame %>%
  summary

#                               Var1        Freq      
# Balearics2013_FOSP11-Juv_ringH7 :1   Min.   : 4007  
# Corsica2013_FOSP17_Juv_ringCBK  :1   1st Qu.: 9338  
# Corsica2014_FOSP21-Juv_ringCIV  :1   Median :13026  
# Italy2014_FOSP27-Juv_ringE7_Edy :1   Mean   :14149  
# Italy2015_FOSP30-juv-ringA7-Cook:1   3rd Qu.:16566  
# Italy2015_FOSP33-juv-Antares    :1   Max.   :39531 
# (Other)                         :8 

dates <- osprey%>%
         group_by(ID)%>%
         summarize(start = min(time), end = max(time))%>% 
         dplyr::select(ID, start, end)%>%
         unique()


osprey_fix_stat_id_tab <- osprey%>%
         group_by(ID)%>%
         count(ID)%>%
         mutate(n_fix = n,
                percent = (n/sum(osprey_fix_stat_id_tab$n))*100)%>%
         dplyr::select(-n)%>%
         left_join(dates, by = ('ID'))%>%
         arrange(start)%>%
         mutate(duration = round(difftime(end, start)))

# export this table to tex

osprey_fix_stat_id_tab %>%
    kable(format = 'latex', booktabs = TRUE) 

# osprey fix summary stat per season
osprey_fix_stat_season <- 
  table(osprey$season) %>%
  data.frame %>%
  summary

#       Var1        Freq      
# Fall  :1   Min.   :40266  
# Spring:1   1st Qu.:45368  
# Summer:1   Median :48048  
# Winter:1   Mean   :49522  
#            3rd Qu.:52202  
#            Max.   :61725 

# number of fix per season
osprey%>%
  group_by
  count(season)

#   season     n
#     Fall 47069
#   Spring 49027
#   Summer 61725
#   Winter 40266

# for how long this ospreys are monitored?
difftime(max(osprey$time), min(osprey$time), units = "days")
#Time difference of 3539.25 days

(difftime(max(osprey$time), min(osprey$time), units = "days") %>% as.numeric)/365.25
# 9.69 years

# create a table that shows the number of fix per seasons grouped by individual
osprey_summary1 <- osprey %>%
  group_by(ID, season) %>%
  count(season)%>%
  arrange(ID,desc(n))

osprey_plot_season <-
         osprey_summary1%>%
         ggplot(osprey, mapping = aes(x = ID, y = n, fill = season))+
         geom_bar(stat = "identity")+
         geom_text(aes(label = n), vjust = 1)+
         labs()+
         theme_bw()+         
         facet_wrap(~season)
         
  

# visualize monitoring duration per individual
n_summary <- osprey %>%
                group_by(ID) %>% 
                summarize(start = min(time), end = max(time))%>% 
                arrange(start) %>% 
                mutate(ID = factor(ID, levels = ID))

osprey_dead <- osprey%>%
                  dplyr::select(c("signal_interruption_cause", "ID"))%>%
                  drop_na(signal_interruption_cause)%>%
                  unique()


n_summary <- n_summary%>%
                  left_join(osprey_dead, by = c("ID"))%>% 
                  mutate(ID = factor(ID, levels = ID))
         

bystart <- with(n_summary, reorder(ID, start))

ggplot(n_summary, aes(y = ID, xmin = start, xmax = end)) + 
geom_linerange(linewidth = 1)+
geom_jitter(data = n_summary[n_summary$signal_interruption_cause == "Death", ], aes(x = start, y = ID), position = position_nudge(y = 0.3), shape = "†", size=4, color="black")+
geom_text(aes(x = start, y = ID, label = ID), nudge_y = -0.25) +
theme_bw()+
xlab("Year")+
ylab("Animals ID")+
theme(axis.text.x = element_text(color="#000000", size=10),
      axis.text.y = element_text(color="#000000", size=10),
      axis.title.x = element_text(color="#000000", size=13, face="bold"),
      axis.title.y = element_text(color="#000000", size=13, face="bold"))

ggsave( "mon_duration.jpg", plot = last_plot())

# ========================================== #
#              Processing data
# ========================================== #








