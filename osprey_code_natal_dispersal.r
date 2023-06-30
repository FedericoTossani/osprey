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

         osprey_nd <- osprey_nd%>%
                  filter(ID == 'H7' & time > '2015-04-02 05:00:00' & time < '2015-05-10 00:00:00' |
                         ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00' | # ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00' | 
                         ID == 'E7' & time > '2016-03-10 05:00:00' |
                         ID == 'A7' & time >= '2017-02-20 00:00:00' |
                         ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' | # ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00' | 
                         ID == 'IBS' & time > '2022-03-22 00:00:00' & time < '2022-06-04 15:00:00' |
                         ID == 'IBH' & time >= '2022-04-09 06:00:00' |
                         ID == 'IBK' & time >= '2022-01-24 06:44:48' |
                         ID == 'IFP' & time > '2023-04-24 00:00:00' & time < '2023-04-30 04:00:00' | # ID == "IFP" & time > '2023-05-16 00:00:00' & time < '2023-06-08 20:00:00' |
                         ID == 'ICZ' & time >= '2020-04-11 10:45:00' & time <= '2020-04-25 00:00:00' |
                         ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00' |
                         ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00'
                        )

# manca IBI (residente), CAM e Antares



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


     osprey_track <- 
         ggplot(osprey_eu) +
         geom_spatvector() + 
          geom_path(data = osp_nd_v, aes(x = x, y = y, color = ID), 
                     linewidth = 0.5, lineend = "round") +
         labs(x = " ", y = " ", title = "Inividual tracks") +
         #facet_wrap(~ ID) +
         theme(legend.position="none") +
         theme_minimal()

         osprey_track





#########################
# Natal dispersal track #
#########################

         countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')

         osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
         osprey_eu <- crop(countries, osprey_ext)
         osprey_eu_utm <- terra::project(osprey_eu, proj_crs)

    # H7 

         h7_nd <- osprey_nd%>%
                  filter(ID == 'H7')

         h7_track <- 
         ggplot(osprey_eu_utm)+
         geom_spatvector()+
         geom_path(data = h7_nd, aes(x = x, y = y), 
                   linewidth = 0.5, lineend = "round", col = 'red') +
         labs(x = " ", y = " ", title = "H7 natal dispersal track") +
         theme_minimal() +
         theme(legend.position = "none")

         h7_track


    # CIV

         civ_nd15 <- osprey_nd%>%
                  filter(ID == 'CIV', time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00')

         civ_track15 <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = civ_nd15, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "CIV inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         civ_track15

         civ_nd16 <- osprey_nd%>%
                  filter(ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00')

         civ_track16 <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = civ_nd16, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "CIV inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         civ_track16


   # CBK

         cbk_nd <- osprey%>%
                  filter(ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00')

         cbk_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = cbk_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "CBK inividual track") +
           theme_minimal() +
           theme(legend.position = "none")


   # E7

         e7_nd <- osprey%>%
                  filter(ID == 'E7' 6 time > '2016-03-10 05:00:00')

         e7_track <- 
         ggplot(europe) +
           geom_spatvector()+
           geom_path(data = e7_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "E7 inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         e7_track


    # A7

         a7_nd <- osprey%>%
                  filter(ID == 'A7' & time >= '2017-02-20 00:00:00')

         a7_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = a7_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "A7 inividual track") +
           theme_minimal() +
           theme(legend.position = "none")
         
         a7_track


    # Antares -> ???

         antares_nd <- osprey%>%
                  filter(ID == 'Antares', time > ???)

         antares_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = antares_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "Antares inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         antares_track


    # IAD -> "time" >= '2018-03-28 06:00:00' AND "time" <= '2018-12-31 19:00:00'

         iad_nd18 <- osprey%>%
                  filter(ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00')

         iad_track18 <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = iad_nd18, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "IAD inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         iad_track18


    # CAM -> ???

         cam_nd <- osprey%>%
                  filter(ID == 'CAM', time >= ???)

         cam_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = cam_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "CAM inividual track") +
           theme_minimal() +
           theme(legend.position = "none")


    # IBI -> ???

# trattato come residente

         ibi_nd <- osprey%>%
                  filter(ID == 'IBI', time >= ???)

         ibi_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = ibi_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "IBI inividual track") +
           theme_minimal() +
           theme(legend.position = "none")


    # IAB -> time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00'

         iab_nd <- osprey%>%
                  filter(ID == 'IAB', time >= time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00')

         iab_track <- 
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = iab_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "IAB inividual track") +
           theme_minimal() +
           theme(legend.position = "none")


    # ICZ -> time >= '2020-04-11 10:45:00' & time <= '2020-04-25 00:00:00'

         icz_nd <- osprey%>%
                  filter(ID == 'ICZ', time >= ???)

         icz_track <- 
         ggplot(countries) +
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
         ggplot(countries) +
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
         ggplot(countries) +
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
         ggplot(countries) +
           geom_spatvector()+
           geom_path(data = ibk_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "IBK inividual track") +
           theme_minimal() +
           theme(legend.position = "none")


#########################
#  Natal dispersal stat #
#########################

        getMoveStats <- function(df){
         # df - is a generic data frame that will contain X,Y and Time columns
         Z <- df$lon + 1i*df$lat
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



