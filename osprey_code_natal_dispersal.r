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

         osprey_nd <- osprey_raw %>%
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

         osprey_nd <- osprey_nd%>%
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
                         ID == 'IBK' & time >= '2022-01-24 06:44:48'
                           )




#########################
# Natal dispersal track #
#########################

    # H7 -> "time" > '2015-04-02 08:00:00' AND "time" < '2015-04-30 16:30:00'

         h7_nd <- osprey%>%
                  filter(ID == 'H7', time > '2015-04-02 08:00:00' & time < '2015-04-30 16:30:00')

         h7_track <- 
         ggplot(countries)+
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
         ggplot(countries) +
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
         ggplot(countries) +
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
         ggplot(europe) +
           geom_spatvector()+
           geom_path(data = e7_nd, aes(x = lon, y = lat), 
                     linewidth = 0.5, lineend = "round", col = 'red') +
           labs(x = " ", y = " ", title = "E7 inividual track") +
           theme_minimal() +
           theme(legend.position = "none")

         e7_track

    # A7 -> "time" >= '2017-02-20 00:00:00'

         a7_nd <- osprey%>%
                  filter(ID == 'A7', time >= '2017-02-20 00:00:00')

         a7_track <- 
         ggplot(countries) +
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
         ggplot(countries) +
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
         ggplot(countries) +
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


    # IAB -> ???

         iab_nd <- osprey%>%
                  filter(ID == 'IAB', time >= ???)

         iab_track <- 
         ggplot(countries) +
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



         osprey_move <- getMoveStats(osprey) 
                                    
         ggplot(osprey_move, aes(ID, MR)) +
         geom_boxplot()


         mr_summarystats <- osprey_move %>%
                           plyr::ddply(c("ID", "season"), summarize,
                            min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                            n = length(MR), NA.count = sum(is.na(MR)),
                            Zero.count = sum(MR == 0, na.rm = TRUE))

         SL_summarystats <- osprey_move %>%
                           plyr::ddply(c("ID", "season"), summarize,
                            min = min(SL, na.rm = TRUE), max = max(SL, na.rm = TRUE),
                            n = length(SL), NA.count = sum(is.na(SL)),
                            Zero.count = sum(SL == 0, na.rm = TRUE))

         osprey_move_nd <- osprey_move%>%
                  filter(ID == 'H7' & time > '2015-04-02 05:00:00' & time < '2015-05-10 00:00:00' |
                         ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00' | # ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00' | 
                         ID == 'E7' & time > '2016-03-10 05:00:00' |
                         ID == 'A7' & time >= '2017-02-20 00:00:00' |
                         ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' | # ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00' | 
                         ID == 'IBS' & time > '2022-03-22 00:00:00' & time < '2022-06-04 15:00:00' |
                         ID == 'IBH' & time >= '2022-04-09 06:00:00' |
                         ID == 'IBK' & time >= '2022-01-24 06:44:48'
                           )

         NatalDispersal_mr_summarystats <- osprey_move_nd %>%
                           plyr::ddply(c("ID"), summarize,
                            min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                            n = length(MR), NA.count = sum(is.na(MR)),
                            Zero.count = sum(MR == 0, na.rm = TRUE))

         NatalDispersal_SL_summarystats <- osprey_move_nd %>%
                           plyr::ddply(c("ID", "season"), summarize,
                            min = min(SL, na.rm = TRUE), max = max(SL, na.rm = TRUE),
                            n = length(SL), NA.count = sum(is.na(SL)),
                            Zero.count = sum(SL == 0, na.rm = TRUE))



           h7 <- osprey%>%
            filter( ID == 'IAD' & time >= '2019-05-07 00:00:00' & time <= '2019-05-07 15:00:00')
       
     h7_lat_time <-
        ggplot(h7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_lon_time <-
        ggplot(h7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_lon_lat <- ggarrange(h7_lon_time, h7_lat_time, ncol = 1, nrow = 2)
         
        h7_lon_lat












