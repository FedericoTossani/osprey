# ================================ #
#       1.SetWD and packages       #
# ================================ #
         setwd("C:/Tesi/R/osprey/data/")

         # this doesn't work, fix it!
         # source(file = "https://github.com/FedericoTossani/osprey/blob/main/osprey_code_packages.r")


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
                               "gridExtra",
                               "ggpubr",
                               "DescTools")

         # with this line of code I check if alll the packages are installed and then I load it

         {
           new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

           if(length(new.packages)) install.packages(new.packages)

           lapply(list.of.packages, require, character.only = TRUE)
         }

# ================================= #
#          2.Data import            #
# ================================= #

         list_csv <- list.files(pattern = "20")
         csv_allfile <- lapply(list_csv, read.csv)

         col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                           "individual.local.identifier")

         csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
         osprey_raw <- bind_rows(csv_file_sel_col)

         osprey_death <- read.csv("osprey_death.csv")


# ================================= #
#        3. Filtering data          #
# ================================= #

# dead ospreys

         osprey_dead <- c("Balearics2013_FOSP11-Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                          "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

# dataset to use for further analysis

         osprey <- osprey_raw %>%
                     dplyr::select("timestamp", "location.long", "location.lat", "external.temperature", 
                                   "individual.local.identifier")%>%
                     rename("lon"="location.long",
                            "lat"="location.lat",
                            "ext_temp"="external.temperature",
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
                             id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK",
                             id == "Italy2022_OSPI08_juv_ringIFP_Ildebrando" ~ "IFP"))

         osprey_nd <- osprey%>%
                  select(-c("timestamp", "id"))%>%
                  relocate("ID", "time", "date", "day", "month", "year", "m_day",
                                       "death_date", "season", "ext_temp", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
                  select(-c("day", "month", "year", "m_day", "ext_temp", "signal_interruption_cause", "death_comment"))%>%
                  unique()

         osprey_nd <- osprey%>%
                  filter(ID == 'H7' & time > '2015-04-02 05:00:00' & time < '2015-05-10 00:00:00' |
                         ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00' | ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00' | 
                         ID == 'E7' & time > '2016-03-10 05:00:00' |
                         ID == 'A7' & time >= '2017-02-20 00:00:00' |
                         ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' | ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00' | 
                         ID == 'IBS' & time > '2022-03-22 00:00:00' & time < '2022-06-04 15:00:00' |
                         ID == 'IBH' & time >= '2022-04-09 06:00:00' |
                         ID == 'IBK' & time >= '2022-01-24 06:44:48' |
                         ID == 'IFP' & time > '2023-04-24 00:00:00' & time < '2023-04-30 04:00:00' | ID == "IFP" & time > '2023-05-16 00:00:00' & time < '2023-06-08 20:00:00' |
                         ID == 'ICZ' & time >= '2020-04-11 10:45:00' & time <= '2020-04-25 00:00:00' |
                         ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00' |
                         ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00' | ID == 'IAB' & time >= '2020-03-16 00:00:00' |
                         ID == "CAM" & time >= "2016-04-14 06:00:00" & time <= "2016-04-19 20:00:00" | ID == "CAM" & time >= "2016-05-03 08:00:00" & time <= "2016-05-06 18:00:00" | ID == "CAM" & time >= "2016-05-20 05:00:00" & time <= "2016-05-24 18:00:00" | ID == "CAM" & time >= "2016-07-02 23:00:00" & time <= "2016-07-06 20:00:00" |
                         ID == "IBI" & time >= "2017-07-21 05:00:00" & time <= "2017-07-27 20:00:00" |
                         ID == "Antares" & time >= "2015-08-15 11:00:00" & time <= "2015-08-18 19:00:00" | ID == "Antares" & time >= "2015-09-08 10:30:00" & time <= "2015-09-13 20:00:00" | ID == "Antares" & time >= "2016-03-19 13:00:00" & time >= "2016-05-23 14:00:00" 
                        )


# Convert lon and lat columns to a spatial object with WGS84 coordinate system
osp_nd_v <- vect(osprey_nd, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

# Define the desired projected coordinate system in meters. You can choose a suitable CRS for your specific location.
proj_crs <- "+proj=utm +zone=32 +datum=WGS84 +ellps:WGS84 +units=m"

# Convert the lon-lat points to the projected coordinate system
osp_nd_v_utm <- terra::project(osp_nd_v, proj_crs)

utm_coord <- geom(osp_nd_v_utm, df = T)

# Extract the X and Y coordinates in meters from the projected points
osprey_nd <- osprey_nd%>%
                  mutate(x = utm_coord$x,
                         y = utm_coord$y)

# View the updated data.frame with the converted coordinates
str(osprey_nd)


osp_nd_v <- vect(osprey_nd, geom = c("x", "y"), crs = "+proj=utm +zone=32 +datum=WGS84 +ellps:WGS84 +units=m")


#########################
# Natal dispersal track #
#########################
         
                  countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
         
                  osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
                  osprey_eu <- crop(countries, osprey_ext)
                  osprey_eu_utm <- terra::project(osprey_eu, proj_crs)

         # let's see all the tracks togheter

              osprey_track <- 
                  ggplot(osprey_eu_utm) +
                  geom_spatvector() + 
                   geom_path(data = osprey_nd, aes(x = x, y = y, color = ID), 
                              linewidth = 0.5, lineend = "round") +
                  labs(x = " ", y = " ", title = "Inividual tracks") +
                  #facet_wrap(~ ID) +
                  theme(legend.position="none") +
                  theme_minimal()
         
                  osprey_track

                 # save the plot to the working directory 
                  ggsave( "osprey_nd.jpg", plot = osprey_track)


# H7 
                  # First let's crop 
                           h7_ext <- ext(c(-7.0000, 8.50000, 35.5000, 45.00000))
                           h7_eu <- crop(countries, h7_ext)
                           h7_eu_utm <- terra::project(h7_eu, proj_crs)
         
                  h7_nd <- osprey_nd%>%
                           filter(ID == 'H7')
         
                  h7_track <- 
                  ggplot(h7_eu_utm)+
                  geom_spatvector()+
                  geom_path(data = h7_nd, aes(x = x, y = y), 
                            linewidth = 0.5, lineend = "round", col = 'red') +
                  labs(x = " ", y = " ", title = "H7 natal dispersal GPS track") +
                  theme_minimal() +
                  theme(legend.position = "none")
         
                  h7_track

                 # save the plot to the working directory 
                  ggsave( "h7_nd.jpg", plot = h7_track)


# CIV
                  # First let's crop 
                           civ_ext <- ext(c(5.0000, 12.0000, 36.0000, 44.00000))
                           civ_eu <- crop(countries, civ_ext)
                           civ_eu_utm <- terra::project(civ_eu, proj_crs)
         
                  civ_nd15 <- osprey_nd%>%
                           filter(ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00')
         
                  civ_track15 <- 
                  ggplot(civ_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = civ_nd15, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CIV 2015 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  civ_track15

                 # save the plot to the working directory 
                  ggsave( "civ_nd15.jpg", plot = civ_track15)

                  civ_nd16 <- osprey_nd%>%
                           filter(ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00')
         
                  civ_track16 <- 
                  ggplot(civ_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = civ_nd16, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CIV 2016 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  civ_track16

                 # save the plot to the working directory 
                  ggsave( "civ_nd16.jpg", plot = civ_track16)
         
                  civ_all_nd <- ggarrange(civ_track15, civ_track16, ncol = 2, nrow = 1)
                  ggsave( "civ_all_nd.jpg", plot = civ_all_nd)


# CBK
                  # First let's crop 
                           cbk_ext <- ext(c(7.0000, 12.5000, 37.5000, 44.00000))
                           cbk_eu <- crop(countries, cbk_ext)
                           cbk_eu_utm <- terra::project(cbk_eu, proj_crs)
         
                  cbk_nd <- osprey_nd%>%
                           filter(ID == 'CBK')
         
                  cbk_track <- 
                  ggplot(cbk_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = cbk_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CBK natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  cbk_track
         
                 # save the plot to the working directory 
                  ggsave( "cbk_nd.jpg", plot = cbk_track)


# E7
                  # First let's crop
                           e7_ext <- ext(c(-1.00000, 17.5000, 36.9409, 49.00000))
                           e7_eu <- crop(countries, e7_ext)
                           e7_eu_utm <- terra::project(e7_eu, proj_crs)
         
                  e7_nd <- osprey_nd%>%
                           filter(ID == 'E7')
         
                  e7_track <- 
                  ggplot(e7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = e7_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "E7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  e7_track
         
                 # save the plot to the working directory 
                  ggsave( "e7_nd.jpg", plot = e7_track)


# A7
                  # First let's crop
                           a7_ext <- ext(c(7.00000, 17.50000, 40.00000, 46.50000 ))
                           a7_eu <- crop(countries, a7_ext)
                           a7_eu_utm <- terra::project(a7_eu, proj_crs)
         
                  a7_nd <- osprey_nd%>%
                           filter(ID == 'A7')
         
                  a7_track <- 
                  ggplot(a7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = a7_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "A7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  a7_track
         
                 # save the plot to the working directory 
                  ggsave( "a7_nd.jpg", plot = a7_track)


# IAD
                  # First let's crop
                           iad_ext <- ext(c(1.50000, 19.50000, 38.00000, 54.00000 ))
                           iad_eu <- crop(countries, iad_ext)
                           iad_eu_utm <- terra::project(iad_eu, proj_crs)
         
                  iad_nd18 <- osprey_nd%>%
                           filter(ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00')
         
                  iad_track18 <- 
                  ggplot(iad_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iad_nd18, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2018 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iad_track18
         
                 # save the plot to the working directory 
                  ggsave( "iad_nd18.jpg", plot = iad_track18)

                  # BERLIN
                  iad_nd19 <- osprey_nd%>%
                           filter(ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00')
         
                  iad_track19 <- 
                  ggplot(iad_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iad_nd19, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2019 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iad_track19
         
                 # save the plot to the working directory 
                  ggsave( "iad_nd19.jpg", plot = iad_track19)

                  iad_all_nd <- ggarrange(iad_track18, iad_track19, ncol = 2, nrow = 1)
                  ggsave( "iad_all_nd.jpg", plot = iad_all_nd)

# IAB
                  # First let's crop
                           iab_ext <- ext(c(7.00000, 17.50000, 37.00000, 45.00000 ))
                           iab_eu <- crop(countries, iab_ext)
                           iab_eu_utm <- terra::project(iab_eu, proj_crs)
         
                  iab_nd <- osprey_nd%>%
                           filter(ID == 'IAB')
         
                  iab_track <- 
                  ggplot(iab_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iab_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAB natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iab_track
         
                 # save the plot to the working directory 
                  ggsave( "iab_nd.jpg", plot = iab_track)

# ICZ
                  # First let's crop
                           icz_ext <- ext(c(10.00000, 16.00000, 37.00000, 44.00000 ))
                           icz_eu <- crop(countries, icz_ext)
                           icz_eu_utm <- terra::project(icz_eu, proj_crs)
         
                  icz_nd <- osprey_nd%>%
                           filter(ID == 'ICZ')
         
                  icz_track <- 
                  ggplot(icz_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = icz_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "ICZ natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  icz_track
         
                 # save the plot to the working directory 
                  ggsave( "icz_nd.jpg", plot = icz_track)

# IBS      
                  # First let's crop
                           ibs_ext <- ext(c(2.00000, 21.00000, 39.00000, 48.00000 ))
                           ibs_eu <- crop(countries, ibs_ext)
                           ibs_eu_utm <- terra::project(ibs_eu, proj_crs)
         
                  ibs_nd <- osprey_nd%>%
                           filter(ID == 'IBS')
         
                  ibs_track <- ggplot(ibs_eu_utm) +
                             geom_spatvector()+
                             geom_path(data = ibs_nd, aes(x = x, y = y), 
                                       linewidth = 0.5, lineend = "round", col = 'red') +
                             labs(x = " ", y = " ", title = "IBS natal dispersal GPS track") +
                             theme_minimal() +
                             theme(legend.position = "none")
         
                  ibs_track
         
                 # save the plot to the working directory 
                  ggsave( "ibs_nd.jpg", plot = ibs_track)


# IBH
                  # First let's crop
                           ibh_ext <- ext(c(9.00000, 16.50000, 36.50000, 45.50000))
                           ibh_eu <- crop(countries, ibh_ext)
                           ibh_eu_utm <- terra::project(ibh_eu, proj_crs)
         
                  ibh_nd <- osprey_nd%>%
                           filter(ID == 'IBH')
         
                  ibh_track <- 
                  ggplot(ibh_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ibh_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBH natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibh_track
         
                 # save the plot to the working directory 
                  ggsave( "ibh_nd.jpg", plot = ibh_track)

# IBK
# residente
                  # First let's crop
                           ibk_ext <- ext(c(9.00000, 12.00000, 42.00000, 43.50000))
                           ibk_eu <- crop(countries, ibk_ext)
                           ibk_eu_utm <- terra::project(ibk_eu, proj_crs)
         
                  ibk_nd <- osprey_nd%>%
                           filter(ID == 'IBK')
         
                  ibk_track <- 
                  ggplot(ibk_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ibk_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBK natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibk_track
         
                 # save the plot to the working directory 
                  ggsave( "ibk_nd.jpg", plot = ibk_track)

# IFP
                  # First let's crop
                           ifp_ext <- ext(c(4.00000, 17.00000, 41.00000, 45.00000))
                           ifp_eu <- crop(countries, ifp_ext)
                           ifp_eu_utm <- terra::project(ifp_eu, proj_crs)
         
                  ifp_nd <- osprey_nd%>%
                           filter(ID == 'IFP')
         
                  ifp_track <- 
                  ggplot(ifp_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ifp_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IFP natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ifp_track
         
                 # save the plot to the working directory 
                  ggsave( "ifp_nd.jpg", plot = ifp_track)

         ########################
         # ND track da definire #
         ########################

# IBI
# residente
                  # First let's crop
                           ibi_ext <- ext(c(6.50000, 13.00000, 41.50000, 45.00000))
                           ibi_eu <- crop(countries, ibi_ext)
                           ibi_eu_utm <- terra::project(ibi_eu, proj_crs)
         
                  ibi_nd <- osprey%>%
                           filter(ID == 'IBI')
         
                  ibi_track <- 
                  ggplot(ibi_eu) +
                    geom_spatvector()+
                    geom_path(data = ibi_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBI natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibi_track
         
# CAM 
# residente
         # First let's crop
                           cam_ext <- ext(c(5.00000, 12.00000, 39.50000, 44.50000 ))
                           cam_eu <- crop(countries, cam_ext)
                           cam_eu_utm <- terra::project(cam_eu, proj_crs)
         
                  cam_nd <- osprey%>%
                           filter(ID == 'CAM')
         
                  cam_track <- 
                  ggplot(cam_eu) +
                    geom_spatvector()+
                    geom_path(data = cam_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CAM natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  cam_track
         
# Antares
                  # First let's crop
                           antares_ext <- ext(c(8.50000, 14.50000, 40.50000, 45.50000 ))
                           antares_eu <- crop(countries, antares_ext)
                           antares_eu_utm <- terra::project(antares_eu, proj_crs)
         
                  antares_nd <- osprey%>%
                           filter(ID == 'Antares')
         
                  antares_track <- 
                  ggplot(antares_eu) +
                    geom_spatvector()+
                    geom_path(data = antares_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "Antares natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  antares_track
         
         # save the plots!!
         
         ggsave("", plots = )

#########################
#  Natal dispersal stat #
#########################
         
         # First let's create a ltraj object with UTM coordinates 
         
                  osp_ndlt <- as.ltraj(osprey_nd[, c("x", "y")],
                                        date = osprey_nd$time, 
                                        id = osprey_nd$ID,
                                        typeII=TRUE)
         
         # let's create a function that split bursts with multiple years         
                 
                  foo <- function(dt){ 
                           return(dt>(100*3600*24))
                                   }
                  
                  osp_ndlt2 <- cutltraj(osp_ndlt,"foo(dt)",nextr=TRUE)
         
                  a7lt <- ld(osp_ndlt2[[1]])
         
                  plotltr(a7lt)
         
                  plotltr(osp_ndlt2, "dist")
         
                  osp_ndlt_df <- ld(osp_ndlt2)

                  osp_ndlt_df <- osp_ndlt_df%>%
                                    mutate(day = as.Date(date),
                                           distKM = dist/1000)
         

# SUMMARY dfs

         summary_distance <- osp_ndlt_df%>%
                                    group_by(burst, day)%>%
                                    summarize(minDistxDay = min(dist),
                                              meanDistxDay = mean(dist),
                                              maxDistxDay = max(dist))

         summary_id_distance <- summary_distance%>%
                                    group_by(burst)%>%
                                    summarize(minDist = min(minDistxDay, na.rm=T),
                                              meanDist = mean(meanDistxDay, na.rm=T),
                                              maxDist = max(maxDistxDay, na.rm=T))


dist_plot <- ggplot(osp_ndlt_df, aes(x = osp_ndlt_df$distKM)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Distance (Km)") +
  ylab("Count") +
  ggtitle("Distribution of Distance")



         # export this table to tex
         
                  summary_id_distance %>%
                      kable(format = 'latex', booktabs = TRUE, digits = c(1, 2, 2, 2)) 


nd_duration <- osp_ndlt_df %>%
  group_by(burst) %>% 
  summarize(start = min(date), end = max(date)) %>%
  mutate(duration = round(difftime(end, start)))

summary_speed <- osp_ndlt_df%>%
  group_by(burst)%>%
  mutate(Speed = (dist/1000)/(dt/3600)) %>% 
  summarize(minSpeed = min(Speed, na.rm=T),
            meanSpeed = mean(Speed, na.rm=T),
            maxSpeed = max(Speed, na.rm=T)) 

osp_ndlt_df<- osp_ndlt_df%>%
  mutate(Speed = (dist/1000)/(dt/3600),
         deg_turnAngle = rel.angle * (180/pi))

                  
                  speed_plot <- ggplot(osp_ndlt_df, aes(x = Speed)) +
                    geom_histogram(binwidth = 1) +
                    labs(x = "Speed", y = "Frequency") +
                    theme_bw()

summary_dt <- osp_ndlt_df%>%
  group_by(burst)%>%
  mutate(dt = dt/3600) %>% 
  summarize(minDt = min(dt, na.rm=T),
            meanDt = mean(dt, na.rm=T),
            maxDt = max(dt, na.rm=T)) 


osp_ndlt_df <- osp_ndlt_df%>%
                  mutate(deg_turnAngle = rel.angle * (180/pi))
















                  # Create an empty list to store the summary data frames:
         
                           osp_summaries <- list()
         
                  # Use the split() function to split the larger data frame based on the unique IDs:
         
                           id_groups <- split(osp_ndlt_df, osp_ndlt_df$id)
         
                  # Iterate over each ID group using a for loop:
                           for (id in names(id_groups)) {
                             id_group <- id_groups[[id]]                   # Access the current ID group
                             summary_stats <- lapply(id_group, summary)    # Calculate summary statistics (e.g., mean, median) for each variable in the ID group
                             summary_df <- as.data.frame(summary_stats)    # Convert the summary statistics into a data frame
                             summary_dfs[[id]] <- summary_df               # Append the summary data frame to the list
                           }
         

         
         # ---------------------------------------------------------------  #
         
                 getMoveStats <- function(df){
                  # df - is a generic data frame that will contain X,Y and Time columns
                  Z <- df$x + 1i*df$y
                  Time <- df$time
                  Step <- c(NA, diff(Z)) # we add the extra NA because there is no step to the first location
                  dT <- c(NA, difftime(Time[-1], Time[-length(Time)], hours) %>% as.numeric)
         
                  SL <- Mod(Step)/1e3 # convert to km - so the speed is in km/hour
                  MR <- SL/dT # computing the movement rate
         
                  # this is what the function returns
                  data.frame(df, Z, dT, Step, SL, MR)
                           }
         
         
         
                  osprey_move_nd <- getMoveStats(osprey_nd) 
         
                  osprey_move_nd <- osprey_move_nd %>%
                                    dplyr::filter( ID != "IBS" & ID != "IBH")
                                             
                  ggplot(osprey_move_nd, aes(ID, MR)) +
                  geom_boxplot()
         
         
                  mr_summarystats <- osprey_move_nd %>%
                                    dplyr::filter( ID != "IBS")%>%
                                    plyr::ddply(c("ID", "season"), summarize,
                                     min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                                     n = length(MR), NA.count = sum(is.na(MR)),
                                     Zero.count = sum(MR == 0, na.rm = TRUE))
         
                  SL_summarystats <- osprey_move_nd %>%
                                    plyr::ddply(c("ID", "season"), summarize,
                                     min = min(SL, na.rm = TRUE), max = max(SL, na.rm = TRUE),
                                     n = length(SL), NA.count = sum(is.na(SL)),
                                     Zero.count = sum(SL == 0, na.rm = TRUE))
         
         
         
                  
                  plot(osprey_move_nd$Step, asp=1, type="n")
                  arrows(rep(0, length(osprey_move_nd$Step)), rep(0, length(osprey_move_nd$Step)), Re(osprey_move_nd$Step), Im(osprey_move_nd$Step), col=rgb(0,0,0,.5), lwd=2, length=0.1)
         
         hist(osprey_move_nd$SL, col="grey", bor="darkgrey", freq=FALSE)
         lines(density(osprey_move_nd$SL), col=2, lwd=2)
         
         
         
