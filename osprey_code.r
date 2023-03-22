
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

head(osprey)

# Visualize the movement

cbbox <- make_bbox(lon = osprey$long, lat = osprey$lat, f = .1) #from ggmap
sq_map <- get_map(location = cbbox, maptype = "terrain", source = "stamen")

eu_bond <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
eu_bond <- crop(eu_bond, osprey_ext)


osprey_track <- 
ggplot(eu_bond) +
geom_spatvector() + 
 geom_path(data = osprey, aes(x = long, y = lat, color = ring_id), 
            linewidth = 0.5, lineend = "round") +
labs(x = " ", y = " ", title = "Inividual tracks") +
#facet_wrap(~ ring_id) +
theme(legend.position="none") +
theme_minimal()


# Subsetting for natal dispersal

# H7 -> "timestamp" > '2015-04-02 08:00:00' AND "timestamp" < '2015-04-30 16:30:00'

h7_nd <- osprey%>%
         filter(ring_id == 'H7', timestamp > '2015-04-02 08:00:00' & timestamp < '2015-04-30 16:30:00')

h7_track <- 
ggplot(eu_bond) +
geom_spatvector()+
  geom_path(data = h7_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "H7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CIV -> "timestamp" > '2016-03-29 00:01:00' AND "timestamp" < '2016-10-29 18:00:00'

civ_nd <- osprey%>%
         filter(ring_id == 'CIV', timestamp > '2016-03-29 00:01:00' & timestamp < '2016-10-29 18:00:00')

civ_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = civ_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CIV inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CBK -> ???

cbk_nd <- osprey%>%
         filter(ring_id == 'CBK', timestamp > ??? & timestamp < ???)

cbk_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = cbk_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CBK inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# E7 -> "timestamp" > '2016-03-10 05:00:00'

e7_nd <- osprey%>%
         filter(ring_id == 'E7', timestamp > '2016-03-10 05:00:00')

e7_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = e7_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "E7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# A7 -> "timestamp" >= '2017-02-20 00:00:00'

a7_nd <- osprey%>%
         filter(ring_id == 'A7', timestamp >= '2017-02-20 00:00:00')

a7_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = a7_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "A7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# Antares -> ???

antares_nd <- osprey%>%
         filter(ring_id == 'Antares', timestamp > ???)

antares_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = antares_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "Antares inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IAD -> "timestamp" >= '2018-03-28 06:00:00' AND "timestamp" <= '2018-12-31 19:00:00'

iad_nd <- osprey%>%
         filter(ring_id == 'IAD', timestamp >= '2018-03-28 06:00:00' & timestamp <= '2018-12-31 19:00:00')

iad_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = iad_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IAD inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# CAM -> ???

cam_nd <- osprey%>%
         filter(ring_id == 'CAM', timestamp >= ???)

cam_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = cam_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "CAM inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBI -> ???

ibi_nd <- osprey%>%
         filter(ring_id == 'IBI', timestamp >= ???)

ibi_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibi_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBI inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IAB -> ???

iab_nd <- osprey%>%
         filter(ring_id == 'IAB', timestamp >= ???)

iab_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = iab_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IAB inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# ICZ -> ???

icz_nd <- osprey%>%
         filter(ring_id == 'ICZ', timestamp >= ???)

icz_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = icz_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "ICZ inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBS -> "timestamp" > '2022-03-19 00:00:47' AND "timestamp" < '2022-06-05 00:01:24'

ibs_nd <- osprey%>%
         filter(ring_id == 'IBS', timestamp > '2022-03-19 00:00:47' & timestamp < '2022-06-05 00:01:24')

ibs_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibs_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBS inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBH -> "timestamp" >= '2022-04-09 04:47:05'

ibh_nd <- osprey%>%
         filter(ring_id == 'IBH', timestamp >= '2022-04-09 04:47:05')

ibh_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibh_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBH inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# IBK -> "timestamp" >= '2022-01-24 06:44:48'

ibk_nd <- osprey%>%
         filter(ring_id == 'IBK', timestamp >= '2022-01-24 06:44:48')

ibk_track <- 
ggplot(eu_bond) +
  geom_spatvector()+
  geom_path(data = ibk_nd, aes(x = long, y = lat), 
            linewidth = 0.5, lineend = "round", col = 'red') +
  labs(x = " ", y = " ", title = "IBK inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


# wintering osprey

osprey_winter <- osprey%>%
                  filter (season == "Winter")%>%
                  dplyr::select(ring_id, long, lat)

# wintering home range

osprey_winter_sp <- SpatialPoints(osprey_winter)

osprey_winter_hr <- clusthr(osprey_winter)


# let's check if there is na in position's columns
table(is.na(osprey$long))
table(is.na(osprey$lat))


# let's calculate some basic statistics per individual
table(osprey$ring_id) %>% sd


# osprey fix summary stat per individual
osprey_fix_stat_id <- 
table(osprey$ring_id) %>%
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
         group_by(ring_id)%>%
         summarize(start = min(timestamp), end = max(timestamp))%>% 
         dplyr::select(ring_id, start, end)%>%
         unique()


osprey_fix_stat_id_tab <- osprey%>%
         group_by(ring_id)%>%
         count(ring_id)%>%
         mutate(n_fix = n,
                percent = (n/sum(osprey_fix_stat_id_tab$n))*100)%>%
         dplyr::select(-n)%>%
         left_join(dates, by = ('ring_id'))%>%
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
difftime(max(osprey$timestamp), min(osprey$timestamp), units = "days")
#Time difference of 3539.25 days

(difftime(max(osprey$timestamp), min(osprey$timestamp), units = "days") %>% as.numeric)/365.25
# 9.69 years

# create a table that shows the number of fix per seasons grouped by individual
osprey_summary1 <- osprey %>%
  group_by(ring_id, season) %>%
  count(season)%>%
  arrange(ring_id,desc(n))

osprey_plot_season <-
         osprey_summary1%>%
         ggplot(osprey, mapping = aes(x = ring_id, y = n, fill = season))+
         geom_bar(stat = "identity")+
         geom_text(aes(label = n), vjust = 1)+
         labs()+
         theme_bw()+         
         facet_wrap(~season)
         
  

# visualize monitoring duration per individual
n_summary <- osprey %>%
                group_by(ring_id) %>% 
                summarize(start = min(timestamp), end = max(timestamp))%>% 
                arrange(start) %>% 
                mutate(ring_id = factor(ring_id, levels = ring_id))

osprey_dead <- osprey%>%
                  dplyr::select(c("signal_interruption_cause", "ring_id"))%>%
                  drop_na(signal_interruption_cause)%>%
                  unique()


n_summary <- n_summary%>%
                  left_join(osprey_dead, by = c("ring_id"))%>% 
                  mutate(ring_id = factor(ring_id, levels = ring_id))
         

bystart <- with(n_summary, reorder(ring_id, start))

ggplot(n_summary, aes(y = ring_id, xmin = start, xmax = end)) + 
geom_linerange(linewidth = 1)+
geom_jitter(data = n_summary[n_summary$signal_interruption_cause == "Death", ], aes(x = start, y = ring_id), position = position_nudge(y = 0.3), shape = "†", size=4, color="black")+
geom_text(aes(x = start, y = ring_id, label = ring_id), nudge_y = -0.25) +
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








