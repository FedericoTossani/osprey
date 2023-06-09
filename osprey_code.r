
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
# 4. Processing data
# 5. 
# 6. 

# ================================ #
#       1.SetWD and packages       #
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
                               "tidyterra",
                               "gridExtra",
                               "ggpubr")

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
                           "gsm.gsm.signal.strength", "sensor.type", "individual.local.identifier")

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


# ========================================== #
#            4.Processing data               #
# ========================================== #


###########################
# Visualize the movements #
###########################

         # let's check if there is na in position's columns
         table(is.na(osprey$lon))
         table(is.na(osprey$lat))

         countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')

         #
         osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
         osprey_eu <- crop(countries, osprey_ext)


         osprey_track <- 
         ggplot(osprey_eu) +
         geom_spatvector() + 
          geom_path(data = osprey, aes(x = lon, y = lat, color = ID), 
                     linewidth = 0.5, lineend = "round") +
         labs(x = " ", y = " ", title = "Inividual tracks") +
         #facet_wrap(~ ID) +
         theme(legend.position="none") +
         theme_minimal()

         osprey_track

         # This should create a more beautiful map BUT need to be fix!
         # cbbox <- make_bbox(lon = osprey$lon, lat = osprey$lat, f = .1) #from ggmap
         # sq_map <- get_map(location = cbbox, maptype = "terrain", source = "stamen")

#####################
#   COORD vs TIME   #
#####################

# Longitude vs time
         all_lon_time <-
         ggplot(osprey, aes(time, lon, col = ID)) +
         geom_point(size = 0.5) +
         #facet_wrap(~ ID) +
         geom_path()


# Latitude vs time
         all_lat_time <-
         ggplot(osprey, aes(time, lat, col = ID)) +
         geom_point(size = 0.5) +
         #facet_wrap(~ ID) +
         geom_path()

         grid.arrange(all_lon_time, all_lat_time, nrow=2)

# COORD vs TIME per animal

  # H7 
        h7 <- osprey%>%
            filter(ID == "H7")

        h7_lat_time <-
        ggplot(h7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_lon_time <-
        ggplot(h7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        h7_lon_lat <- ggarrange(h7_lon_time, h7_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("h7_lon_lat.jpg", plot=h7_lon_lat)

        h7_lon_lat <- grid.arrange(h7_lon_time, h7_lat_time, nrow=2)


  # CBK

# si sposta tra roost notturno e area di foraggiamento
# dalla latitudine si vede che si comporta come uno svernante normale
        cbk <- osprey%>%
            filter(ID == "CBK")

        cbk_lat_time <-
        ggplot(cbk, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        cbk_lon_time <-
        ggplot(cbk, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        cbk_lon_lat <- ggarrange(cbk_lon_time, cbk_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("cbk_lon_lat.jpg", plot=cbk_lon_lat)

        grid.arrange(cbk_lon_time, cbk_lat_time, nrow=2)

  # CIV

        civ <- osprey%>%
            filter(ID == "CIV")

        civ_lat_time <-
        ggplot(civ, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        civ_lon_time <-
        ggplot(civ, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        civ_lon_lat <- ggarrange(civ_lon_time, civ_lat_time, ncol = 1, nrow = 2)
     
      #  ggsave("civ_lon_lat.jpg", plot=civ_lon_lat)

        grid.arrange(civ_lon_time, civ_lat_time, nrow=2)

  # E7
        e7 <- osprey%>%
            filter(ID == "E7")

        e7_lat_time <-
        ggplot(e7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        e7_lon_time <-
        ggplot(e7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        e7_lon_lat <- ggarrange(e7_lon_time, e7_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("e7_lon_lat.jpg", plot=e7_lon_lat)

        grid.arrange(e7_lon_time, e7_lat_time, nrow=2)

  # A7 
        a7 <- osprey%>%
            filter(ID == "A7")

        a7_lat_time <-
        ggplot(a7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        a7_lon_time <-
        ggplot(a7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        a7_lon_lat <- ggarrange(a7_lon_time, a7_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("a7_lon_lat.jpg", plot=a7_lon_lat)

        grid.arrange(a7_lon_time, a7_lat_time, nrow=2)

  # Antares

# controlla come sono stati trattati gli individui residenti nell'articolo di IBIS
# calcola media dei movimenti e individua spostamento significativo
        antares <- osprey%>%
            filter(ID == "Antares")

        antares_lat_time <-
        ggplot(antares, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        antares_lon_time <-
        ggplot(antares, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        antares_lon_lat <- ggarrange(antares_lon_time, antares_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("antares_lon_lat.jpg", plot=antares_lon_lat)

        grid.arrange(antares_lon_time, antares_lat_time, nrow=2)

  # IAD
        iad <- osprey%>%
            filter(ID == "IAD")

        iad_lat_time <-
        ggplot(iad, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        iad_lon_time <-
        ggplot(iad, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        iad_lon_lat <- ggarrange(iad_lon_time, iad_lat_time, ncol = 1, nrow = 2)
     
      #  ggsave("iad_lon_lat.jpg", plot=iad_lon_lat)

        grid.arrange(iad_lon_time, iad_lat_time, nrow=2)

  # CAM 
# tienilo per ultimo
        cam <- osprey%>%
            filter(ID == "CAM")

        cam_lat_time <-
        ggplot(cam, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        cam_lon_time <-
        ggplot(cam, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        cam_lon_lat <- grid.arrange(cam_lon_time, cam_lat_time, nrow=2)

  # IBI
# trattalo come residente

        ibi <- osprey%>%
            filter(ID == "IBI")

        ibi_lat_time <-
        ggplot(ibi, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ibi_lon_time <-
        ggplot(ibi, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        ibi_lon_lat <- grid.arrange(ibi_lon_time, ibi_lat_time, nrow=2)

  # IAB     
        iab <- osprey%>%
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

  # ICZ  
        icz <- osprey%>%
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

  # IBS  
        ibs <- osprey%>%
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

  # IBH 
        ibh <- osprey%>%
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

  # IBK
        ibk <- osprey%>%
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


# Check this part!
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
ggplot(countries) +
#geom_spatvector()+
  geom_path(data = h7, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round") +
  labs(x = " ", y = " ", title = "H7 inividual track") +
  theme_minimal() +
  theme(legend.position = "none")


##########################
# Distances between fix  #
##########################

# H7


         # Estimating movement rate
                  h7_winter <- osprey%>%
                                    dplyr::filter(ID == "H7" & date >= "2014-12-01" & date < "2015-04-02")%>%
                                    dplyr::select(ID, time, lon, lat)   

                  crdref <- "+proj=longlat +datum=WGS84"
                  h7_winter_shp <- vect(h7_winter, crs=crdref)
                  h7_winter_df <- as.data.frame(h7_winter_shp)


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


                  h7_winter_move <- getMoveStats(h7_winter) 
                                    
                  ggplot(h7_winter_move, aes(ID, MR)) +
                           geom_boxplot()


                  mr_summarystats <- h7_winter_move %>%
                           plyr::ddply(c("ID"), summarize,
                            min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                            n = length(MR), NA.count = sum(is.na(MR)),
                            Zero.count = sum(MR == 0, na.rm = TRUE))

         # Mean ditance between fixs during winter

         # Mean ditance between fixs during natal dispersal




##########################
# Descriptive statistics #
##########################

table(osprey$ID)%>%
         sd


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










