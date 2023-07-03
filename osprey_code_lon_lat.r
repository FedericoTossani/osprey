
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

         osprey<- osprey%>%
                  select(-c("timestamp", "id"))%>%
                  relocate("ID", "time", "date", "day", "month", "year", "m_day",
                                       "death_date", "season", "ext_temp", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
                  unique()





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
         
        h7_lon_lat

       # ggsave("h7_lon_lat.jpg", plot=h7_lon_lat)


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


  # IFP 
        ifp <- osprey%>%
            filter(ID == "IFP")

        ifp_lat_time <-
        ggplot(ifp, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_path()

        ifp_lon_time <-
        ggplot(ifp, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_path()

        ifp_lon_lat <- ggarrange(ifp_lon_time, ifp_lat_time, ncol = 1, nrow = 2)
         
        ifp_lon_lat

       # ggsave("ifp_lon_lat.jpg", plot=ifp_lon_lat)


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
filter(ID == "IBK")%>%
group_by(date)


h7_track <- 
ggplot(ibk_eu) +
geom_spatvector()+
  geom_path(data = h7, aes(x = lon, y = lat), 
            linewidth = 0.5, lineend = "round") +
  labs(x = " ", y = " ", title = "IBK inividual track") +
  theme_minimal() +
  theme(legend.position = "none")

h7_track
